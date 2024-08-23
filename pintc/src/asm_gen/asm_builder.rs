use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::{
        BinaryOp, Expr, ExternalIntrinsic, Immediate, InternalIntrinsic, IntrinsicKind,
        TupleAccess, UnaryOp,
    },
    predicate::{Contract, ExprKey, Predicate, State as StateVar},
    span::empty_span,
    types::Type,
};
use essential_types::{predicate::Predicate as CompiledPredicate, ContentAddress};
use state_asm::{
    Access, Alu, Constraint, Crypto, Op as StateRead, Pred, Stack, StateSlots, TotalControlFlow,
};
use std::collections::HashMap;

/// This object keeps track of the various assembly blocks that are being generated throughout
/// assembly generation. It also keeps track of predicate addresses as they become available. Those
/// addresses are useful when one predicate references another predicate in the same contract
pub struct AsmBuilder<'a> {
    // Opcodes to read state.
    pub s_asm: Vec<Vec<StateRead>>,
    // Opcodes to specify constraints
    pub c_asm: Vec<Vec<Constraint>>,
    // A reference to a list of the predicate addresses which are known so far
    pub compiled_predicates: &'a HashMap<String, (CompiledPredicate, ContentAddress)>,
}

/// "Location" of an expression:
/// 1. `DecisionVar` expressions refer to expressions that require the `DecisionVar` or
///    `DecisionVarRange` opcodes. The `usize` here is the index of the decision variable.
/// 2. `State` expressions refer to expressions that require the `State` or `StateRange` opcodes.
///    The `bool` is the "delta": are we referring to the current or the next state?
/// 3. `Storage` expressions refer to expressions that are storage keys and need to be read from
///    using `KeyRange` or `KeyRangeExtern`.
/// 4. `Transient` expressions refer to expressions that require the `Transient` opcode. The first
///    `ExprKey` is an expression that represents the transient key length. The second `ExprKey` is
///    an expression that represents the pathway in the solution.
/// 5. `Value` expressions are just raw values such as immediates or the outputs of binary ops.
enum Location {
    DecisionVar(usize),
    State(bool),
    Storage { key_len: usize, is_extern: bool },
    Transient { key_len: ExprKey, pathway: ExprKey },
    Value,
}

impl AsmBuilder<'_> {
    /// Generates assembly for a given state read
    pub(super) fn compile_state(
        &mut self,
        handler: &Handler,
        state: &StateVar,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<(), ErrorEmitted> {
        let mut c_asm: Vec<Constraint> = Vec::new();
        let mut s_asm: Vec<StateRead> = Vec::new();

        self.compile_expr(handler, &mut c_asm, &mut s_asm, &state.expr, contract, pred)?;
        self.s_asm.push(s_asm);

        Ok(())
    }

    /// Generates assembly for a given constraint
    pub(super) fn compile_constraint(
        &mut self,
        handler: &Handler,
        expr: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<(), ErrorEmitted> {
        let mut asm = Vec::new();
        let mut s_asm = Vec::new();

        self.compile_expr(handler, &mut asm, &mut s_asm, expr, contract, pred)?;
        self.c_asm.push(asm);

        Ok(())
    }

    /// Generates assembly for an `ExprKey`. Populates `c_asm` if this a non-storage expression and
    /// `s_asm` otherwise.
    ///
    /// Returns the number of opcodes used to express `expr`. For now, this the number of opcodes
    /// used in `s_asm` if this is a storage expression. Otherwise, it is the number of opcodes
    /// used in `c_asm`.
    fn compile_expr(
        &self,
        handler: &Handler,
        c_asm: &mut Vec<Constraint>,
        s_asm: &mut Vec<StateRead>,
        expr: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<usize, ErrorEmitted> {
        let old_c_asm_len = c_asm.len();
        let old_s_asm_len = s_asm.len();

        let expr_ty = expr.get_ty(contract);
        match self.compile_expr_pointer(handler, c_asm, s_asm, expr, contract, pred)? {
            Location::DecisionVar(len) => {
                if len == 1 {
                    // If the decision variable itself is a single word, just `DecisionVar`
                    // directly. Otherwise, we may need `DecisionVarRange` (e.g. tuple access,
                    // etc.)
                    c_asm.push(Access::DecisionVar.into());
                } else {
                    c_asm.push(Stack::Push(expr_ty.size(handler, contract)? as i64).into()); // len
                    c_asm.push(Access::DecisionVarRange.into());
                }
                Ok(c_asm.len() - old_c_asm_len)
            }
            Location::State(next_state) => {
                let slots = expr_ty.storage_or_transient_slots(handler, contract)?;
                if slots == 1 {
                    c_asm.push(Stack::Push(next_state as i64).into());
                    c_asm.push(Access::State.into());
                } else {
                    c_asm.push(Stack::Push(slots as i64).into());
                    c_asm.push(Stack::Push(next_state as i64).into());
                    c_asm.push(Access::StateRange.into());
                }
                Ok(c_asm.len() - old_c_asm_len)
            }
            Location::Transient { key_len, pathway } => {
                // We may need several `Transient` opcodes if the expression we're trying to read
                // is stored in multiple slots (e.g. a tuple or an array).
                for i in 0..expr_ty.storage_or_transient_slots(handler, contract)? {
                    if i > 0 {
                        // Re-compute the key and then offset with `+ i`. We do this manually here
                        // because we don't have a `TransientRange` op that is similar to
                        // `KeyRange`
                        self.compile_expr_pointer(handler, c_asm, s_asm, expr, contract, pred)?;
                        c_asm.push(Stack::Push(i as i64).into());
                        c_asm.push(Alu::Add.into());
                    }

                    // Now just compile the key length and the pathway expression and follow with
                    // the `Transient` opcode
                    self.compile_expr(handler, c_asm, s_asm, &key_len, contract, pred)?;
                    self.compile_expr(handler, c_asm, s_asm, &pathway, contract, pred)?;
                    c_asm.push(Constraint::Access(Access::Transient));
                }
                Ok(c_asm.len() - old_c_asm_len)
            }
            Location::Value => Ok(c_asm.len() - old_c_asm_len),
            Location::Storage { key_len, is_extern } => {
                s_asm.extend(
                    c_asm
                        .iter()
                        .map(|op| StateRead::Constraint(*op))
                        .collect::<Vec<_>>(),
                );

                let storage_or_transient_slots =
                    expr_ty.storage_or_transient_slots(handler, contract)?;

                s_asm.extend([
                    Stack::Push(storage_or_transient_slots as i64).into(),
                    StateSlots::AllocSlots.into(),
                    Stack::Push(key_len as i64).into(), // key_len
                    Stack::Push(storage_or_transient_slots as i64).into(), // num_keys_to_read
                    Stack::Push(0).into(),              // slot_index
                    if is_extern {
                        StateRead::KeyRangeExtern
                    } else {
                        StateRead::KeyRange
                    },
                    TotalControlFlow::Halt.into(),
                ]);
                Ok(s_asm.len() - old_s_asm_len)
            }
        }
    }

    /// Generates assembly for an `ExprKey` as a _pointer_. What this means is that, if the expr
    /// refers to something other than a "value" (like an immedate), we generate assembly for the
    /// pointer (i.e. `Location`) only. A "pointer" may be a state slot or a transient data key for
    /// example.
    fn compile_expr_pointer(
        &self,
        handler: &Handler,
        c_asm: &mut Vec<Constraint>,
        s_asm: &mut Vec<StateRead>,
        expr: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        fn compile_immediate(c_asm: &mut Vec<Constraint>, imm: &Immediate) {
            match imm {
                Immediate::Int(val) => c_asm.push(Stack::Push(*val).into()),
                Immediate::Bool(val) => c_asm.push(Stack::Push(*val as i64).into()),
                Immediate::B256(val) => {
                    c_asm.push(Stack::Push(val[0] as i64).into());
                    c_asm.push(Stack::Push(val[1] as i64).into());
                    c_asm.push(Stack::Push(val[2] as i64).into());
                    c_asm.push(Stack::Push(val[3] as i64).into());
                }
                Immediate::Array(elements) => {
                    for element in elements {
                        compile_immediate(c_asm, element);
                    }
                }
                Immediate::Tuple(fields) => {
                    for (_, field) in fields {
                        compile_immediate(c_asm, field);
                    }
                }
                Immediate::Error | Immediate::Nil | Immediate::Real(_) | Immediate::String(_) => {
                    unimplemented!("other literal types are not yet supported")
                }
            }
        }

        match &expr.get(contract) {
            Expr::Immediate { value, .. } => {
                compile_immediate(c_asm, value);
                Ok(Location::Value)
            }
            Expr::Array { elements, .. } => {
                for element in elements {
                    self.compile_expr(handler, c_asm, s_asm, element, contract, pred)?;
                }
                Ok(Location::Value)
            }
            Expr::Tuple { fields, .. } => {
                for (_, field) in fields {
                    self.compile_expr(handler, c_asm, s_asm, field, contract, pred)?;
                }
                Ok(Location::Value)
            }
            Expr::Path(path, _) => Self::compile_path(handler, c_asm, path, contract, pred),
            Expr::UnaryOp { op, expr, .. } => {
                self.compile_unary_op(handler, c_asm, s_asm, op, expr, contract, pred)
            }
            Expr::BinaryOp { op, lhs, rhs, .. } => {
                self.compile_binary_op(handler, c_asm, s_asm, op, lhs, rhs, contract, pred)
            }
            Expr::IntrinsicCall { kind, args, .. } => {
                self.compile_intrinsic_call(handler, c_asm, s_asm, &kind.0, args, contract, pred)
            }
            Expr::Select {
                condition,
                then_expr,
                else_expr,
                ..
            } => self.compile_select(
                handler, c_asm, s_asm, condition, then_expr, else_expr, contract, pred,
            ),
            Expr::Index { expr, index, .. } => {
                self.compile_index(handler, c_asm, s_asm, expr, index, contract, pred)
            }
            Expr::TupleFieldAccess { tuple, field, .. } => {
                self.compile_tuple_field_access(handler, c_asm, s_asm, tuple, field, contract, pred)
            }
            Expr::Error(_)
            | Expr::StorageAccess { .. }
            | Expr::ExternalStorageAccess { .. }
            | Expr::MacroCall { .. }
            | Expr::Cast { .. }
            | Expr::In { .. }
            | Expr::Range { .. }
            | Expr::Generator { .. } => Err(handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "These expressions should have been lowered by now",
                    span: empty_span(),
                },
            })),
        }
    }

    /// Compile a path expression. Assumes that each path expressions corresponds to a decision
    /// variable or a state variable. All other paths should have been lowered to something else by
    /// now.
    fn compile_path(
        handler: &Handler,
        c_asm: &mut Vec<Constraint>,
        path: &String,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        if let Some((var_index, (var_key, _))) = pred
            .vars()
            .filter(|(_, var)| !var.is_pub)
            .enumerate()
            .find(|(_, (_, var))| &var.name == path)
        {
            // Handle (private) decision vars first
            let var_ty_size = var_key.get_ty(pred).size(handler, contract)?;
            c_asm.push(Stack::Push(var_index as i64).into()); // slot
            if var_key.get_ty(pred).size(handler, contract)? > 1 {
                c_asm.push(Stack::Push(0).into()); // placeholder for index computation
            }
            Ok(Location::DecisionVar(var_ty_size))
        } else if let Some(state_index) = pred.states().position(|(_, state)| &state.name == path) {
            // Now handle state vars
            c_asm.push(
                Stack::Push(
                    pred.states()
                        .take(state_index)
                        .try_fold(0, |acc, (state_key, _)| {
                            state_key
                                .get_ty(pred)
                                .storage_or_transient_slots(handler, contract)
                                .map(|slots| acc + slots)
                        })? as i64,
                )
                .into(),
            );

            Ok(Location::State(false))
        } else {
            // Must not have anything else at this point. All other path expressions should have
            // been lowered to something else by now
            return Err(handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "this path expression should have been lowered by now",
                    span: empty_span(),
                },
            }));
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_unary_op(
        &self,
        handler: &Handler,
        c_asm: &mut Vec<Constraint>,
        s_asm: &mut Vec<StateRead>,
        op: &UnaryOp,
        expr: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        match op {
            UnaryOp::Not => {
                self.compile_expr(handler, c_asm, s_asm, expr, contract, pred)?;
                c_asm.push(Pred::Not.into());
                Ok(Location::Value)
            }
            UnaryOp::NextState => {
                // Next state expressions produce state expressions (i.e. ones that require `State`
                // or `StateRange`
                self.compile_expr_pointer(handler, c_asm, s_asm, expr, contract, pred)?;
                Ok(Location::State(true))
            }
            UnaryOp::Neg => {
                // Push `0` (i.e. `lhs`) before the `expr` (i.e. `rhs`) opcodes. Then subtract
                // `lhs` - `rhs` to negate the value.
                c_asm.push(Constraint::Stack(Stack::Push(0)));
                self.compile_expr(handler, c_asm, s_asm, expr, contract, pred)?;
                c_asm.push(Alu::Sub.into());
                Ok(Location::Value)
            }
            UnaryOp::Error => unreachable!("unexpected Unary::Error"),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_binary_op(
        &self,
        handler: &Handler,
        c_asm: &mut Vec<Constraint>,
        s_asm: &mut Vec<StateRead>,
        op: &BinaryOp,
        lhs: &ExprKey,
        rhs: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        let lhs_len = self.compile_expr(handler, c_asm, s_asm, lhs, contract, pred)?;
        let rhs_len = self.compile_expr(handler, c_asm, s_asm, rhs, contract, pred)?;

        match op {
            BinaryOp::Add => c_asm.push(Alu::Add.into()),
            BinaryOp::Sub => c_asm.push(Alu::Sub.into()),
            BinaryOp::Mul => c_asm.push(Alu::Mul.into()),
            BinaryOp::Div => c_asm.push(Alu::Div.into()),
            BinaryOp::Mod => c_asm.push(Alu::Mod.into()),
            BinaryOp::Equal => {
                let type_size = lhs.get_ty(contract).size(handler, contract)?;
                if type_size == 1 {
                    c_asm.push(Pred::Eq.into());
                } else {
                    c_asm.push(Stack::Push(type_size as i64).into());
                    c_asm.push(Pred::EqRange.into());
                }
            }
            BinaryOp::NotEqual => {
                c_asm.push(Pred::Eq.into());
                c_asm.push(Pred::Not.into());
            }
            BinaryOp::LessThanOrEqual => c_asm.push(Pred::Lte.into()),
            BinaryOp::LessThan => c_asm.push(Pred::Lt.into()),
            BinaryOp::GreaterThanOrEqual => {
                c_asm.push(Pred::Gte.into());
            }
            BinaryOp::GreaterThan => c_asm.push(Pred::Gt.into()),
            BinaryOp::LogicalAnd => {
                // Short-circuit AND. Using `JumpForwardIf`, converts `x && y` to:
                // if !x { false } else { y }

                // Location right before the `lhs` opcodes
                let lhs_position = c_asm.len() - rhs_len - lhs_len;

                // Location right before the `rhs` opcodes
                let rhs_position = c_asm.len() - rhs_len;

                // Push `false` before `lhs` opcodes. This is the result of the `AND` operation if
                // `lhs` is false.
                c_asm.insert(lhs_position, Stack::Push(0).into());

                // Then push the number of instructions to skip over if the `lhs` is true.  That's
                // `rhs_len + 2` because we're going to add to add `Pop` later and we want to skip
                // over that AND all the `rhs` opcodes
                c_asm.insert(lhs_position + 1, Stack::Push(rhs_len as i64 + 2).into());

                // Now, invert `lhs` to get the jump condition which is `!lhs`
                c_asm.insert(rhs_position + 2, Pred::Not.into());

                // Then, add the `JumpForwardIf` instruction after the `rhs` opcodes and the two
                // newly added opcodes. The `lhs` is the condition.
                c_asm.insert(
                    rhs_position + 3,
                    Constraint::TotalControlFlow(TotalControlFlow::JumpForwardIf),
                );

                // Finally, insert a ` Pop`. The point here is that if the jump condition (i.e.
                // `!lhs`) is false, then we want to remove the `true` we push on the stack above.
                c_asm.insert(rhs_position + 4, Stack::Pop.into());
            }
            BinaryOp::LogicalOr => {
                // Short-circuit OR. Using `JumpForwardIf`, converts `x || y` to:
                // if x { true } else { y }

                // Location right before the `lhs` opcodes
                let lhs_position = c_asm.len() - rhs_len - lhs_len;

                // Location right before the `rhs` opcodes
                let rhs_position = c_asm.len() - rhs_len;

                // Push `true` before `lhs` opcodes. This is the result of the `OR` operation if
                // `lhs` is true.
                c_asm.insert(lhs_position, Stack::Push(1).into());

                // Then push the number of instructions to skip over if the `lhs` is true.  That's
                // `rhs_len + 2` because we're going to add to add `Pop` later and we want to skip
                // over that AND all the `rhs` opcodes
                c_asm.insert(lhs_position + 1, Stack::Push(rhs_len as i64 + 2).into());

                // Now add the `JumpForwardIf` instruction after the `rhs` opcodes and the two
                // newly added opcodes. The `lhs` is the condition.
                c_asm.insert(
                    rhs_position + 2,
                    Constraint::TotalControlFlow(TotalControlFlow::JumpForwardIf),
                );

                // Then, insert a ` Pop`. The point here is that if the jump condition (i.e. `lhs`)
                // is false, then we want to remove the `true` we push on the stack above.
                c_asm.insert(rhs_position + 3, Stack::Pop.into());
            }
        }
        Ok(Location::Value)
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_intrinsic_call(
        &self,
        handler: &Handler,
        c_asm: &mut Vec<Constraint>,
        s_asm: &mut Vec<StateRead>,
        kind: &IntrinsicKind,
        args: &[ExprKey],
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        match kind {
            IntrinsicKind::External(kind) => self
                .compile_external_intrinsic_call(handler, c_asm, s_asm, kind, args, contract, pred),
            IntrinsicKind::Internal(kind) => self
                .compile_internal_intrinsic_call(handler, c_asm, s_asm, kind, args, contract, pred),
            IntrinsicKind::Error => Err(handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "intrinsic of kind `Error` encounter",
                    span: empty_span(),
                },
            })),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_external_intrinsic_call(
        &self,
        handler: &Handler,
        c_asm: &mut Vec<Constraint>,
        s_asm: &mut Vec<StateRead>,
        kind: &ExternalIntrinsic,
        args: &[ExprKey],
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        if let ExternalIntrinsic::AddressOf = kind {
            assert_eq!(args.len(), 1);
            assert!(args[0].get_ty(contract).is_string());
            if let Some(Expr::Immediate {
                value: Immediate::String(s),
                ..
            }) = args[0].try_get(contract)
            {
                // Push the predicate address on the stack, one word at a time.
                let predicate_address = &self
                    .compiled_predicates
                    .get(s)
                    .expect("predicate address should exist!")
                    .1;

                for word in essential_types::convert::word_4_from_u8_32(predicate_address.0) {
                    c_asm.push(Constraint::Stack(Stack::Push(word)));
                }
            }
        } else if let ExternalIntrinsic::StateLen = kind {
            // StateLen is handled separately from other intrinsics, for now. This tells me that
            // its design is flawed somehow. Therefore, we should redesign it properly in the
            // future so that this function is simplified.

            assert_eq!(args.len(), 1);

            // Check argument:
            // - `state_var` must be a path to a state var or a "next state" expression
            let is_state = match args[0].try_get(contract) {
                Some(Expr::Path(name, _)) => pred.states().any(|(_, state)| state.name == *name),
                Some(Expr::UnaryOp {
                    op: UnaryOp::NextState,
                    ..
                }) => true,
                _ => false,
            };

            if !is_state {
                c_asm.push(Constraint::Stack(Stack::Push(0)));
            } else {
                self.compile_expr(handler, c_asm, s_asm, &args[0], contract, pred)?;

                // After compiling a path to a state var or a "next state" expression, we expect
                // that the last opcode is a `State` or a `StateRange`. Pop that and replace it
                // with `StateLen` or `StateLenRange` since we're after the state length here and
                // not the actual state.
                if let Some(Constraint::Access(Access::State)) = c_asm.last() {
                    c_asm.pop();
                    c_asm.push(Constraint::Access(Access::StateLen));
                } else if let Some(Constraint::Access(Access::StateRange)) = c_asm.last() {
                    c_asm.pop();
                    c_asm.push(Constraint::Access(Access::StateLenRange));
                    // Now, add all the resulting state length. We should get back as many as we
                    // have slots for `args[0]`.
                    let slots = args[0]
                        .get_ty(contract)
                        .storage_or_transient_slots(handler, contract)?;
                    (0..slots - 1).for_each(|_| c_asm.push(Alu::Add.into()));
                }
            }
        } else {
            for (i, arg) in args.iter().enumerate() {
                self.compile_expr(handler, c_asm, s_asm, arg, contract, pred)?;
                if kind.args()[i].is_any() {
                    c_asm.push(Constraint::Stack(Stack::Push(
                        arg.get_ty(contract).size(handler, contract)? as i64,
                    )));
                }
            }

            c_asm.push(match kind {
                ExternalIntrinsic::PredicateAt => Constraint::Access(Access::PredicateAt),
                ExternalIntrinsic::RecoverSECP256k1 => Constraint::Crypto(Crypto::RecoverSecp256k1),
                ExternalIntrinsic::Sha256 => Constraint::Crypto(Crypto::Sha256),
                ExternalIntrinsic::ThisAddress => Constraint::Access(Access::ThisAddress),
                ExternalIntrinsic::ThisContractAddress => {
                    Constraint::Access(Access::ThisContractAddress)
                }
                ExternalIntrinsic::ThisPathway => Constraint::Access(Access::ThisPathway),
                ExternalIntrinsic::VerifyEd25519 => Constraint::Crypto(Crypto::VerifyEd25519),
                ExternalIntrinsic::StateLen | ExternalIntrinsic::AddressOf => {
                    unreachable!("StateLen and AddressOf are handled above")
                }
                ExternalIntrinsic::VecLen => {
                    return Err(handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "__vec_len should have been lowered to something else by now",
                            span: empty_span(),
                        },
                    }))
                }
            })
        }

        Ok(Location::Value)
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_internal_intrinsic_call(
        &self,
        handler: &Handler,
        c_asm: &mut Vec<Constraint>,
        s_asm: &mut Vec<StateRead>,
        kind: &InternalIntrinsic,
        args: &[ExprKey],
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        // Ideally, we would rewrite this in the style of `compile_external_intrinsic_call`, but
        // `kind.args()` is not implemented yet and the way internal intrinsics have been added is
        // not great. We should redesign this in the future.
        match kind {
            InternalIntrinsic::EqSet => {
                assert_eq!(args.len(), 2);
                self.compile_expr(handler, c_asm, s_asm, &args[0], contract, pred)?;
                self.compile_expr(handler, c_asm, s_asm, &args[1], contract, pred)?;
                c_asm.push(Constraint::Pred(Pred::EqSet));
                Ok(Location::Value)
            }

            InternalIntrinsic::MutKeys => {
                assert!(args.is_empty());
                c_asm.push(Constraint::Access(Access::MutKeys));
                Ok(Location::Value)
            }

            InternalIntrinsic::StorageGet => {
                // Expecting a single argument that is an array of integers representing a key
                assert_eq!(args.len(), 1);
                let Ok(key_len) = args[0].get_ty(contract).size(handler, contract) else {
                    return Err(handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "unable to get key size",
                            span: empty_span(),
                        },
                    }));
                };

                self.compile_expr(handler, c_asm, s_asm, &args[0], contract, pred)?;
                Ok(Location::Storage {
                    key_len,
                    is_extern: false,
                })
            }

            InternalIntrinsic::StorageGetExtern => {
                // Expecting two arguments:
                // 1. An address that is a `b256`
                // 2. A key: an array of integers
                assert_eq!(args.len(), 2);
                let Ok(key_len) = args[1].get_ty(contract).size(handler, contract) else {
                    return Err(handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "unable to get key size",
                            span: empty_span(),
                        },
                    }));
                };

                // First, get the contract address and the storage key
                self.compile_expr(handler, c_asm, s_asm, &args[0], contract, pred)?;
                self.compile_expr(handler, c_asm, s_asm, &args[1], contract, pred)?;
                Ok(Location::Storage {
                    key_len,
                    is_extern: true,
                })
            }

            InternalIntrinsic::Transient => {
                // This particular intrinsic returns a `Location::Transient`. All other intrinsics
                // are just values.

                // Expecting 3 arguments:
                assert_eq!(args.len(), 3);
                // First argument is a key. Let that be anything

                // Second argument is a key length. We want that to be an integer
                assert!(args[1].get_ty(contract).is_int());

                // Third argument is the "pathway". We want that to be a path expression (to a
                // pathway variable) or an intrinsic call (to `__this_pathway`). Not being too
                // exact here but that's fine since this is all internal anyways.
                assert!(matches!(
                    args[2].try_get(contract),
                    Some(Expr::Path(..) | Expr::IntrinsicCall { .. })
                ));

                // First, compile the key
                self.compile_expr(handler, c_asm, s_asm, &args[0], contract, pred)?;

                Ok(Location::Transient {
                    key_len: args[1],
                    pathway: args[2],
                })
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_select(
        &self,
        handler: &Handler,
        c_asm: &mut Vec<Constraint>,
        s_asm: &mut Vec<StateRead>,
        condition: &ExprKey,
        then_expr: &ExprKey,
        else_expr: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        if then_expr.can_panic(contract, pred) || else_expr.can_panic(contract, pred) {
            // We need to short circuit these with control flow to avoid potential panics.  The
            // 'else' is put before the 'then' since it's easier to jump-if-true.
            //
            // This jump to 'then' will get updated with the proper distance below.
            let to_then_jump_idx = c_asm.len();
            c_asm.push(Constraint::Stack(Stack::Push(-1)));
            self.compile_expr(handler, c_asm, s_asm, condition, contract, pred)?;
            c_asm.push(Constraint::TotalControlFlow(
                TotalControlFlow::JumpForwardIf,
            ));

            // Compile the 'else' selection, update the prior jump.  We need to jump over the size
            // of 'else` plus 3 instructions it uses to jump the 'then'.
            let else_size = self.compile_expr(handler, c_asm, s_asm, else_expr, contract, pred)?;
            c_asm[to_then_jump_idx] = Constraint::Stack(Stack::Push(else_size as i64 + 4));

            // This (unconditional) jump over 'then' will also get updated.
            let to_end_jump_idx = c_asm.len();
            c_asm.push(Constraint::Stack(Stack::Push(-1)));
            c_asm.push(Constraint::Stack(Stack::Push(1)));
            c_asm.push(Constraint::TotalControlFlow(
                TotalControlFlow::JumpForwardIf,
            ));

            // Compile the 'then' selection, update the prior jump.
            let then_size = self.compile_expr(handler, c_asm, s_asm, then_expr, contract, pred)?;
            c_asm[to_end_jump_idx] = Constraint::Stack(Stack::Push(then_size as i64 + 1));
        } else {
            // Alternatively, evaluate both options and use ASM `select` to choose one.
            let type_size = then_expr.get_ty(contract).size(handler, contract)?;
            self.compile_expr(handler, c_asm, s_asm, else_expr, contract, pred)?;
            self.compile_expr(handler, c_asm, s_asm, then_expr, contract, pred)?;
            if type_size == 1 {
                self.compile_expr(handler, c_asm, s_asm, condition, contract, pred)?;
                c_asm.push(Constraint::Stack(Stack::Select));
            } else {
                c_asm.push(Constraint::Stack(Stack::Push(type_size as i64)));
                self.compile_expr(handler, c_asm, s_asm, condition, contract, pred)?;
                c_asm.push(Constraint::Stack(Stack::SelectRange));
            }
        }
        Ok(Location::Value)
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_index(
        &self,
        handler: &Handler,
        c_asm: &mut Vec<Constraint>,
        s_asm: &mut Vec<StateRead>,
        expr: &ExprKey,
        index: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        let location = self.compile_expr_pointer(handler, c_asm, s_asm, expr, contract, pred)?;
        match location {
            Location::State(_) | Location::Transient { .. } | Location::DecisionVar(_) => {
                // Offset calculation is pretty much the same for all types of data

                // Grab the element ty of the array
                let Type::Array { ty, .. } = expr.get_ty(contract) else {
                    return Err(handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "type must exist and be an array type",
                            span: empty_span(),
                        },
                    }));
                };

                // Compile the index
                self.compile_expr(handler, c_asm, s_asm, index, contract, pred)?;

                // Multiply the index by the number of storage slots for `ty` to get the offset,
                // then add the result to the base key
                c_asm.push(
                    // Decision vars are flattened in a given slots, so we look at the raw size in
                    // words. For transient and storage variables, we look at the "storage size"
                    // which may yield different results (e.g. a `b256` is 4 words but its storage
                    // size is 1
                    Stack::Push(if let Location::DecisionVar(_) = location {
                        ty.size(handler, contract)?
                    } else {
                        ty.storage_or_transient_slots(handler, contract)?
                    } as i64)
                    .into(),
                );
                c_asm.push(Alu::Mul.into());
                c_asm.push(Alu::Add.into());

                Ok(location)
            }
            Location::Value | Location::Storage { .. } => {
                unimplemented!("we'll handle these eventually as a fallback option")
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_tuple_field_access(
        &self,
        handler: &Handler,
        c_asm: &mut Vec<Constraint>,
        s_asm: &mut Vec<StateRead>,
        tuple: &ExprKey,
        field: &TupleAccess,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        let location = self.compile_expr_pointer(handler, c_asm, s_asm, tuple, contract, pred)?;
        match location {
            Location::State(_) | Location::Transient { .. } | Location::DecisionVar(_) => {
                // Offset calculation is pretty much the same for all types of data

                // Grab the fields of the tuple
                let Type::Tuple { ref fields, .. } = tuple.get_ty(contract) else {
                    return Err(handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "type must exist and be a tuple type",
                            span: empty_span(),
                        },
                    }));
                };

                // The field index is based on the type definition
                let field_idx = match field {
                    TupleAccess::Index(idx) => *idx,
                    TupleAccess::Name(ident) => fields
                        .iter()
                        .position(|(field_name, _)| {
                            field_name
                                .as_ref()
                                .map_or(false, |name| name.name == ident.name)
                        })
                        .expect("field name must exist, this was checked in type checking"),
                    TupleAccess::Error => {
                        return Err(handler.emit_err(Error::Compile {
                            error: CompileError::Internal {
                                msg: "unexpected TupleAccess::Error",
                                span: empty_span(),
                            },
                        }));
                    }
                };

                // This is the offset from the base key where the full tuple is stored.
                let key_offset: usize =
                    fields.iter().take(field_idx).try_fold(0, |acc, (_, ty)| {
                        // Decision vars are flattened in a given slots, so we look at the raw size
                        // in words. For transient and storage variables, we look at the "storage
                        // size" which may yield different results (e.g. a `b256` is 4 words but
                        // its storage size is 1
                        if let Location::DecisionVar(_) = location {
                            ty.size(handler, contract)
                        } else {
                            ty.storage_or_transient_slots(handler, contract)
                        }
                        .map(|slots| acc + slots)
                    })?;

                // Now offset using `Add`
                c_asm.push(Stack::Push(key_offset as i64).into());
                c_asm.push(Alu::Add.into());
                Ok(location)
            }
            Location::Value | Location::Storage { .. } => {
                unimplemented!("we'll handle these eventually as a fallback option")
            }
        }
    }
}
