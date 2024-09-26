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
    Access, Alu, Constraint as ConstraintOp, Crypto, Op as StateOp, Pred, Stack, StateSlots,
    TotalControlFlow,
};
use std::collections::HashMap;

/// This object keeps track of the various assembly blocks that are being generated throughout
/// assembly generation. It also keeps track of predicate addresses as they become available. Those
/// addresses are useful when one predicate references another predicate in the same contract
pub struct AsmBuilder<'a> {
    // Opcodes to read state.
    pub state_programs: Vec<Vec<StateOp>>,
    // Opcodes to specify constraints
    pub constraint_programs: Vec<Vec<ConstraintOp>>,
    // A reference to a list of the predicate addresses which are known so far
    pub compiled_predicates: &'a HashMap<String, (CompiledPredicate, ContentAddress)>,
}

/// A single assembly program which may be a "constraint program" or a "state program"
enum Asm {
    Constraint(Vec<ConstraintOp>),
    State(Vec<StateOp>),
}

impl Asm {
    /// Push a single `ConstraintOp` onto the program. Convert the op to a `StateOp` if `self` is a
    /// "state program".
    fn push(&mut self, op: ConstraintOp) {
        match self {
            Self::Constraint(ref mut ops) => ops.push(op),
            Self::State(ref mut ops) => ops.push(StateOp::Constraint(op)),
        }
    }

    /// Set the op at `index` to `op`. Panics if `index` is out of bounds.
    fn set(&mut self, index: usize, op: ConstraintOp) {
        match self {
            Self::Constraint(ref mut ops) => ops[index] = op,
            Self::State(ref mut ops) => ops[index] = StateOp::Constraint(op),
        }
    }

    /// Try to push a single `SatetOp` onto the program. Because the op is a `StateOp`, this could
    /// fail if `self` is a "constraint program"
    fn try_push(&mut self, handler: &Handler, op: StateOp) -> Result<(), ErrorEmitted> {
        match self {
            Self::Constraint(_) => Err(handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "These expressions should have been lowered by now",
                    span: empty_span(),
                },
            })),
            Self::State(ref mut ops) => {
                ops.push(op);
                Ok(())
            }
        }
    }

    /// Insert a single `ConstraintOp` in the program at a given index. Convert the op to a
    /// `StateOp` if `self` is a "state program".
    fn insert(&mut self, index: usize, op: ConstraintOp) {
        match self {
            Self::Constraint(ref mut ops) => ops.insert(index, op),
            Self::State(ref mut ops) => ops.insert(index, StateOp::Constraint(op)),
        }
    }

    /// Returns the number of ops in asm program `self`
    fn len(&self) -> usize {
        match self {
            Self::Constraint(ops) => ops.len(),
            Self::State(ops) => ops.len(),
        }
    }
}

/// "Location" of an expression:
/// 1. `DecisionVar` expressions refer to expressions that require the `DecisionVar` opcode.
/// 2. `State` expressions refer to expressions that require the `State` opcode. The `bool` is the
///    "delta": are we referring to the current or the next state?
/// 3. `Storage` expressions refer to expressions that are storage keys and need to be read from
///    using `KeyRange` or `KeyRangeExtern`. The `bool` is `true` if the storage access is external
///    (i.e. requires `KeyRangeExtern`) and `false` otherwise.
/// 4. `PubVar` expressions refer to expressions that require the `PubVar` opcode. The `ExprKey` is
///    the pathway.
/// 5. `Value` expressions are just raw values such as immediates or the outputs of binary ops.
enum Location {
    DecisionVar,
    State(bool),
    Storage(bool),
    PubVar(ExprKey),
    Value,
}

impl AsmBuilder<'_> {
    /// Given an asm program `asm`, push it to the appropridate vector of programs in `self`. This
    /// may be a "constraint program" or a "state program"
    fn push_asm_program(&mut self, asm: Asm) {
        match asm {
            Asm::Constraint(ops) => self.constraint_programs.push(ops),
            Asm::State(ops) => self.state_programs.push(ops),
        }
    }

    /// Generates assembly for a given state read and adds the resulting program to `self.
    pub(super) fn compile_state(
        &mut self,
        handler: &Handler,
        state: &StateVar,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<(), ErrorEmitted> {
        let mut asm: Asm = Asm::State(Vec::new());
        self.compile_expr(handler, &mut asm, &state.expr, contract, pred)?;
        self.push_asm_program(asm);
        Ok(())
    }

    /// Generates assembly for a given constraint and adds the resulting program to `self.
    pub(super) fn compile_constraint(
        &mut self,
        handler: &Handler,
        expr: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<(), ErrorEmitted> {
        let mut asm: Asm = Asm::Constraint(Vec::new());
        self.compile_expr(handler, &mut asm, expr, contract, pred)?;
        self.push_asm_program(asm);
        Ok(())
    }

    /// Generates assembly for an `ExprKey` and insert it into `asm`. Returns the number of opcodes
    /// used to express `expr`.
    fn compile_expr(
        &self,
        handler: &Handler,
        asm: &mut Asm,
        expr: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<usize, ErrorEmitted> {
        let old_asm_len = asm.len();

        let expr_ty = expr.get_ty(contract);
        match self.compile_expr_pointer(handler, asm, expr, contract, pred)? {
            Location::DecisionVar => {
                asm.push(Stack::Push(expr_ty.size(handler, contract)? as i64).into()); // len
                asm.push(Access::DecisionVarRange.into());
                Ok(asm.len() - old_asm_len)
            }

            Location::State(next_state) => {
                let mut primitive_elements: Vec<usize> = Vec::new();
                expr_ty.primitive_elements(handler, contract, &mut primitive_elements)?;

                // Each primitive element in the type lives in its own slot. Go over each primitive
                // element and read it separately.
                for (i, _size) in primitive_elements.iter().enumerate() {
                    // _size will be used when we switch to the new access opcodes
                    //
                    // if `i == 0`, the state slot is already computed. Otherwise, compile
                    // again and offset
                    if i > 0 {
                        self.compile_expr_pointer(handler, asm, expr, contract, pred)?;
                        asm.push(Stack::Push(i as i64).into());
                        asm.push(Alu::Add.into()); // slot_ix
                    }

                    asm.push(Stack::Push(next_state as i64).into()); // delta
                    asm.push(Access::State.into());
                }

                Ok(asm.len() - old_asm_len)
            }

            Location::PubVar(pathway) => {
                let mut primitive_elements: Vec<usize> = Vec::new();
                expr_ty.primitive_elements(handler, contract, &mut primitive_elements)?;

                // Each primitive element in the type gets its own key. Go over each primitive
                // element and read it separately.
                for (i, _size) in primitive_elements.iter().enumerate() {
                    // _size will be used when we switch to the new access opcodes
                    //
                    // if `i == 0`, the pub var key is already computed. Otherwise, compile again
                    // and offset. We need to swap first because the computed key also includes the
                    // key length at the end
                    if i > 0 {
                        self.compile_expr_pointer(handler, asm, expr, contract, pred)?;
                        asm.push(Stack::Swap.into());
                        asm.push(Stack::Push(i as i64).into());
                        asm.push(Alu::Add.into());
                        asm.push(Stack::Swap.into());
                    }

                    self.compile_expr(handler, asm, &pathway, contract, pred)?;
                    asm.push(ConstraintOp::Access(Access::Transient));
                }
                Ok(asm.len() - old_asm_len)
            }

            Location::Value => Ok(asm.len() - old_asm_len),

            Location::Storage(is_extern) => {
                let num_keys_to_read = expr_ty.storage_or_pub_var_slots(handler, contract)?;

                // Allocate slots
                asm.push(Stack::Push(num_keys_to_read as i64).into());
                asm.try_push(handler, StateSlots::AllocSlots.into())?;

                // Read the storage keys into the slots
                asm.push(Stack::Push(num_keys_to_read as i64).into()); // num_keys_to_read
                asm.push(Stack::Push(0).into()); // slot_index
                asm.try_push(
                    handler,
                    if is_extern {
                        StateOp::KeyRangeExtern
                    } else {
                        StateOp::KeyRange
                    },
                )?;
                asm.try_push(handler, TotalControlFlow::Halt.into())?;

                Ok(asm.len() - old_asm_len)
            }
        }
    }

    /// Generates assembly for an `ExprKey` as a _pointer_. What this means is that, if the expr
    /// refers to something other than a "value" (like an immedate), we generate assembly for the
    /// pointer (i.e. `Location`) only. A "pointer" may be a state slot or a  pub var data key for
    /// example.
    fn compile_expr_pointer(
        &self,
        handler: &Handler,
        asm: &mut Asm,
        expr: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        fn compile_immediate(asm: &mut Asm, imm: &Immediate) {
            match imm {
                Immediate::Int(val) | Immediate::Enum(val, _) => asm.push(Stack::Push(*val).into()),
                Immediate::Bool(val) => asm.push(Stack::Push(*val as i64).into()),
                Immediate::B256(val) => {
                    asm.push(Stack::Push(val[0] as i64).into());
                    asm.push(Stack::Push(val[1] as i64).into());
                    asm.push(Stack::Push(val[2] as i64).into());
                    asm.push(Stack::Push(val[3] as i64).into());
                }
                Immediate::Array(elements) => {
                    for element in elements {
                        compile_immediate(asm, element);
                    }
                }
                Immediate::Tuple(fields) => {
                    for (_, field) in fields {
                        compile_immediate(asm, field);
                    }
                }
                Immediate::Error | Immediate::Nil | Immediate::Real(_) | Immediate::String(_) => {
                    unreachable!("Unexpected literal")
                }
            }
        }

        match expr.get(contract) {
            Expr::Immediate { value, .. } => {
                compile_immediate(asm, value);
                Ok(Location::Value)
            }
            Expr::Array { elements, .. } => {
                for element in elements {
                    self.compile_expr(handler, asm, element, contract, pred)?;
                }
                Ok(Location::Value)
            }
            Expr::Tuple { fields, .. } => {
                for (_, field) in fields {
                    self.compile_expr(handler, asm, field, contract, pred)?;
                }
                Ok(Location::Value)
            }
            Expr::Path(path, _) => Self::compile_path(handler, asm, path, contract, pred),
            Expr::UnionVariant { path, value, .. } => {
                self.compile_union_expr(handler, asm, expr, path, value, contract, pred)
            }
            Expr::UnaryOp { op, expr, .. } => {
                self.compile_unary_op(handler, asm, op, expr, contract, pred)
            }
            Expr::BinaryOp { op, lhs, rhs, .. } => {
                self.compile_binary_op(handler, asm, op, lhs, rhs, contract, pred)
            }
            Expr::IntrinsicCall { kind, args, .. } => {
                self.compile_intrinsic_call(handler, asm, &kind.0, args, contract, pred)
            }
            Expr::Select {
                condition,
                then_expr,
                else_expr,
                ..
            } => self.compile_select(
                handler, asm, condition, then_expr, else_expr, contract, pred,
            ),
            Expr::Index { expr, index, .. } => {
                self.compile_index(handler, asm, expr, index, contract, pred)
            }
            Expr::TupleFieldAccess { tuple, field, .. } => {
                self.compile_tuple_field_access(handler, asm, tuple, field, contract, pred)
            }
            Expr::UnionTag { union_expr, .. } => {
                self.compile_union_tag_is(handler, asm, union_expr, contract, pred)
            }
            Expr::UnionValue { union_expr, .. } => {
                self.compile_union_get_value(handler, asm, union_expr, contract, pred)
            }
            Expr::Error(_)
            | Expr::StorageAccess { .. }
            | Expr::ExternalStorageAccess { .. }
            | Expr::MacroCall { .. }
            | Expr::Cast { .. }
            | Expr::In { .. }
            | Expr::Range { .. }
            | Expr::Generator { .. }
            | Expr::Match { .. } => Err(handler.emit_err(Error::Compile {
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
        asm: &mut Asm,
        path: &String,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        if let Some((var_index, _)) = pred
            .vars()
            .filter(|(_, var)| !var.is_pub)
            .enumerate()
            .find(|(_, (_, var))| &var.name == path)
        {
            asm.push(Stack::Push(var_index as i64).into()); // slot
            asm.push(Stack::Push(0).into()); // placeholder for index computation
            Ok(Location::DecisionVar)
        } else if let Some(state_index) = pred.states().position(|(_, state)| &state.name == path) {
            // Now handle state vars
            asm.push(
                Stack::Push(
                    pred.states()
                        .take(state_index)
                        .try_fold(0, |acc, (state_key, _)| {
                            state_key
                                .get_ty(pred)
                                .storage_or_pub_var_slots(handler, contract)
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

    fn compile_unary_op(
        &self,
        handler: &Handler,
        asm: &mut Asm,
        op: &UnaryOp,
        expr: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        match op {
            UnaryOp::Not => {
                self.compile_expr(handler, asm, expr, contract, pred)?;
                asm.push(Pred::Not.into());
                Ok(Location::Value)
            }
            UnaryOp::NextState => {
                // Next state expressions produce state expressions (i.e. ones that require `State`
                // or `StateRange`
                self.compile_expr_pointer(handler, asm, expr, contract, pred)?;
                Ok(Location::State(true))
            }
            UnaryOp::Neg => {
                // Push `0` (i.e. `lhs`) before the `expr` (i.e. `rhs`) opcodes. Then subtract
                // `lhs` - `rhs` to negate the value.
                asm.push(ConstraintOp::Stack(Stack::Push(0)));
                self.compile_expr(handler, asm, expr, contract, pred)?;
                asm.push(Alu::Sub.into());
                Ok(Location::Value)
            }
            UnaryOp::Error => unreachable!("unexpected Unary::Error"),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_binary_op(
        &self,
        handler: &Handler,
        asm: &mut Asm,
        op: &BinaryOp,
        lhs: &ExprKey,
        rhs: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        let lhs_len = self.compile_expr(handler, asm, lhs, contract, pred)?;
        let rhs_len = self.compile_expr(handler, asm, rhs, contract, pred)?;

        match op {
            BinaryOp::Add => asm.push(Alu::Add.into()),
            BinaryOp::Sub => asm.push(Alu::Sub.into()),
            BinaryOp::Mul => asm.push(Alu::Mul.into()),
            BinaryOp::Div => asm.push(Alu::Div.into()),
            BinaryOp::Mod => asm.push(Alu::Mod.into()),
            BinaryOp::Equal => {
                let type_size = lhs.get_ty(contract).size(handler, contract)?;
                if type_size == 1 {
                    asm.push(Pred::Eq.into());
                } else {
                    asm.push(Stack::Push(type_size as i64).into());
                    asm.push(Pred::EqRange.into());
                }
            }
            BinaryOp::NotEqual => {
                asm.push(Pred::Eq.into());
                asm.push(Pred::Not.into());
            }
            BinaryOp::LessThanOrEqual => asm.push(Pred::Lte.into()),
            BinaryOp::LessThan => asm.push(Pred::Lt.into()),
            BinaryOp::GreaterThanOrEqual => {
                asm.push(Pred::Gte.into());
            }
            BinaryOp::GreaterThan => asm.push(Pred::Gt.into()),
            BinaryOp::LogicalAnd => {
                // Short-circuit AND. Using `JumpForwardIf`, converts `x && y` to:
                // if !x { false } else { y }

                // Location right before the `lhs` opcodes
                let lhs_position = asm.len() - rhs_len - lhs_len;

                // Location right before the `rhs` opcodes
                let rhs_position = asm.len() - rhs_len;

                // Push `false` before `lhs` opcodes. This is the result of the `AND` operation if
                // `lhs` is false.
                asm.insert(lhs_position, Stack::Push(0).into());

                // Then push the number of instructions to skip over if the `lhs` is true.  That's
                // `rhs_len + 2` because we're going to add to add `Pop` later and we want to skip
                // over that AND all the `rhs` opcodes
                asm.insert(lhs_position + 1, Stack::Push(rhs_len as i64 + 2).into());

                // Now, invert `lhs` to get the jump condition which is `!lhs`
                asm.insert(rhs_position + 2, Pred::Not.into());

                // Then, add the `JumpForwardIf` instruction after the `rhs` opcodes and the two
                // newly added opcodes. The `lhs` is the condition.
                asm.insert(
                    rhs_position + 3,
                    ConstraintOp::TotalControlFlow(TotalControlFlow::JumpForwardIf),
                );

                // Finally, insert a ` Pop`. The point here is that if the jump condition (i.e.
                // `!lhs`) is false, then we want to remove the `true` we push on the stack above.
                asm.insert(rhs_position + 4, Stack::Pop.into());
            }
            BinaryOp::LogicalOr => {
                // Short-circuit OR. Using `JumpForwardIf`, converts `x || y` to:
                // if x { true } else { y }

                // Location right before the `lhs` opcodes
                let lhs_position = asm.len() - rhs_len - lhs_len;

                // Location right before the `rhs` opcodes
                let rhs_position = asm.len() - rhs_len;

                // Push `true` before `lhs` opcodes. This is the result of the `OR` operation if
                // `lhs` is true.
                asm.insert(lhs_position, Stack::Push(1).into());

                // Then push the number of instructions to skip over if the `lhs` is true.  That's
                // `rhs_len + 2` because we're going to add to add `Pop` later and we want to skip
                // over that AND all the `rhs` opcodes
                asm.insert(lhs_position + 1, Stack::Push(rhs_len as i64 + 2).into());

                // Now add the `JumpForwardIf` instruction after the `rhs` opcodes and the two
                // newly added opcodes. The `lhs` is the condition.
                asm.insert(
                    rhs_position + 2,
                    ConstraintOp::TotalControlFlow(TotalControlFlow::JumpForwardIf),
                );

                // Then, insert a ` Pop`. The point here is that if the jump condition (i.e. `lhs`)
                // is false, then we want to remove the `true` we push on the stack above.
                asm.insert(rhs_position + 3, Stack::Pop.into());
            }
        }
        Ok(Location::Value)
    }

    fn compile_intrinsic_call(
        &self,
        handler: &Handler,
        asm: &mut Asm,
        kind: &IntrinsicKind,
        args: &[ExprKey],
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        let expected_args = kind.args();
        for (expected, arg) in expected_args.iter().zip(args.iter()) {
            let found = arg.get_ty(contract);
            if !expected.eq(&contract.new_types, found) {
                handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "unexpected intrinsic arg type",
                        span: empty_span(),
                    },
                });
            }
        }

        // Also, ensure that the number of arguments is correct
        if args.len() != expected_args.len() {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "unexpected number of args for intrinsic",
                    span: empty_span(),
                },
            });
        }

        match kind {
            IntrinsicKind::External(kind) => {
                self.compile_external_intrinsic_call(handler, asm, kind, args, contract, pred)
            }

            IntrinsicKind::Internal(kind) => {
                self.compile_internal_intrinsic_call(handler, asm, kind, args, contract, pred)
            }

            IntrinsicKind::Error => Err(handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "intrinsic of kind `Error` encounter",
                    span: empty_span(),
                },
            })),
        }
    }

    fn compile_external_intrinsic_call(
        &self,
        handler: &Handler,
        asm: &mut Asm,
        kind: &ExternalIntrinsic,
        args: &[ExprKey],
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        match kind {
            ExternalIntrinsic::AddressOf => {
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
                        asm.push(ConstraintOp::Stack(Stack::Push(word)));
                    }
                }
            }

            ExternalIntrinsic::StateLen => {
                // StateLen is handled separately from other intrinsics, for now. This tells me that
                // its design is flawed somehow. Therefore, we should redesign it properly in the
                // future so that this function is simplified.

                let is_state = match args[0].try_get(contract) {
                    Some(Expr::Path(name, _)) => {
                        pred.states().any(|(_, state)| state.name == *name)
                    }
                    Some(Expr::UnaryOp {
                        op: UnaryOp::NextState,
                        ..
                    }) => true,
                    _ => false,
                };

                if !is_state {
                    // the "state length" of non-state expressions is 0
                    asm.push(ConstraintOp::Stack(Stack::Push(0)));
                } else if let Location::State(next_state) =
                    self.compile_expr_pointer(handler, asm, &args[0], contract, pred)?
                {
                    let num_slots = args[0]
                        .get_ty(contract)
                        .storage_or_pub_var_slots(handler, contract)?;

                    for i in 0..num_slots {
                        if i > 0 {
                            self.compile_expr_pointer(handler, asm, &args[0], contract, pred)?;
                        }
                        asm.push(Stack::Push(i as i64).into());
                        asm.push(Alu::Add.into()); // slot_ix
                        asm.push(Stack::Push(next_state as i64).into()); // delta
                        asm.push(Access::StateLen.into()); // Range length for State
                    }

                    (0..num_slots - 1).for_each(|_| asm.push(Alu::Add.into()));
                }
            }

            // All other external intrinsics can be handled generically
            _ => {
                for (i, arg) in args.iter().enumerate() {
                    self.compile_expr(handler, asm, arg, contract, pred)?;

                    // if the type of the arg is `Any`, then follow with its size
                    if kind.args()[i].is_any() {
                        asm.push(ConstraintOp::Stack(Stack::Push(
                            arg.get_ty(contract).size(handler, contract)? as i64,
                        )));
                    }
                }

                asm.push(match kind {
                    ExternalIntrinsic::PredicateAt => ConstraintOp::Access(Access::PredicateAt),
                    ExternalIntrinsic::RecoverSECP256k1 => {
                        ConstraintOp::Crypto(Crypto::RecoverSecp256k1)
                    }
                    ExternalIntrinsic::Sha256 => ConstraintOp::Crypto(Crypto::Sha256),
                    ExternalIntrinsic::ThisAddress => ConstraintOp::Access(Access::ThisAddress),
                    ExternalIntrinsic::ThisContractAddress => {
                        ConstraintOp::Access(Access::ThisContractAddress)
                    }
                    ExternalIntrinsic::ThisPathway => ConstraintOp::Access(Access::ThisPathway),
                    ExternalIntrinsic::VerifyEd25519 => ConstraintOp::Crypto(Crypto::VerifyEd25519),
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
        }

        Ok(Location::Value)
    }

    fn compile_internal_intrinsic_call(
        &self,
        handler: &Handler,
        asm: &mut Asm,
        kind: &InternalIntrinsic,
        args: &[ExprKey],
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        for (i, arg) in args.iter().enumerate() {
            if matches!(kind, InternalIntrinsic::PubVar) && i == 0 {
                // Do not compile the pathway here because the opcode `Transient` expects it after
                // the key and we still need to offset the key, if needed.
                continue;
            }

            self.compile_expr(handler, asm, arg, contract, pred)?;

            // if the type of the arg is `Any`, then follow with its size
            //
            // The only exception is set types since they are not very well specified yet. The
            // length of a set currently lives in the set itself.
            if !matches!(kind, InternalIntrinsic::EqSet) && kind.args()[i].is_any() {
                asm.push(ConstraintOp::Stack(Stack::Push(
                    arg.get_ty(contract).size(handler, contract)? as i64,
                )));
            }
        }

        match kind {
            InternalIntrinsic::EqSet => {
                asm.push(ConstraintOp::Pred(Pred::EqSet));
                Ok(Location::Value)
            }
            InternalIntrinsic::MutKeys => {
                asm.push(ConstraintOp::Access(Access::MutKeys));
                Ok(Location::Value)
            }
            InternalIntrinsic::StorageGet => Ok(Location::Storage(false)),
            InternalIntrinsic::StorageGetExtern => Ok(Location::Storage(true)),
            InternalIntrinsic::PubVar => Ok(Location::PubVar(args[0] /* pathway */)),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_select(
        &self,
        handler: &Handler,
        asm: &mut Asm,
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
            let to_then_jump_idx = asm.len();
            asm.push(ConstraintOp::Stack(Stack::Push(-1)));
            self.compile_expr(handler, asm, condition, contract, pred)?;
            asm.push(ConstraintOp::TotalControlFlow(
                TotalControlFlow::JumpForwardIf,
            ));

            // Compile the 'else' selection, update the prior jump.  We need to jump over the size
            // of 'else` plus 3 instructions it uses to jump the 'then'.
            let else_size = self.compile_expr(handler, asm, else_expr, contract, pred)?;

            asm.set(
                to_then_jump_idx,
                ConstraintOp::Stack(Stack::Push(else_size as i64 + 4)),
            );

            // This (unconditional) jump over 'then' will also get updated.
            let to_end_jump_idx = asm.len();
            asm.push(ConstraintOp::Stack(Stack::Push(-1)));
            asm.push(ConstraintOp::Stack(Stack::Push(1)));
            asm.push(ConstraintOp::TotalControlFlow(
                TotalControlFlow::JumpForwardIf,
            ));

            // Compile the 'then' selection, update the prior jump.
            let then_size = self.compile_expr(handler, asm, then_expr, contract, pred)?;

            asm.set(
                to_end_jump_idx,
                ConstraintOp::Stack(Stack::Push(then_size as i64 + 1)),
            );
        } else {
            // Alternatively, evaluate both options and use ASM `select` to choose one.
            let type_size = then_expr.get_ty(contract).size(handler, contract)?;
            self.compile_expr(handler, asm, else_expr, contract, pred)?;
            self.compile_expr(handler, asm, then_expr, contract, pred)?;
            if type_size == 1 {
                self.compile_expr(handler, asm, condition, contract, pred)?;
                asm.push(ConstraintOp::Stack(Stack::Select));
            } else {
                asm.push(ConstraintOp::Stack(Stack::Push(type_size as i64)));
                self.compile_expr(handler, asm, condition, contract, pred)?;
                asm.push(ConstraintOp::Stack(Stack::SelectRange));
            }
        }
        Ok(Location::Value)
    }

    fn compile_index(
        &self,
        handler: &Handler,
        asm: &mut Asm,
        expr: &ExprKey,
        index: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        let location = self.compile_expr_pointer(handler, asm, expr, contract, pred)?;

        let compile_offset = |asm: &mut Asm| {
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
            self.compile_expr(handler, asm, index, contract, pred)?;

            // Multiply the index by the number of storage slots for `ty` to get the offset,
            // then add the result to the base key
            asm.push(
                // Decision vars are flattened in a given slots, so we look at the raw size in
                // words. For pub var and storage variables, we look at the "storage size"
                // which may yield different results (e.g. a `b256` is 4 words but its storage
                // size is 1
                Stack::Push(if let Location::DecisionVar = location {
                    ty.size(handler, contract)?
                } else {
                    ty.storage_or_pub_var_slots(handler, contract)?
                } as i64)
                .into(),
            );
            asm.push(Alu::Mul.into());
            asm.push(Alu::Add.into());

            Ok(())
        };

        match location {
            Location::PubVar(_) => {
                // For pub vars, and before computing the offset, we need to swap first because the
                // computed key also includes the key length at the end
                asm.push(Stack::Swap.into());
                compile_offset(asm)?;
                asm.push(Stack::Swap.into());
            }
            Location::State(_) | Location::DecisionVar => {
                compile_offset(asm)?;
            }
            Location::Value | Location::Storage(_) => {
                unimplemented!("we'll handle these eventually as a fallback option")
            }
        }

        Ok(location)
    }

    fn compile_tuple_field_access(
        &self,
        handler: &Handler,
        asm: &mut Asm,
        tuple: &ExprKey,
        field: &TupleAccess,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        let location = self.compile_expr_pointer(handler, asm, tuple, contract, pred)?;

        let compile_offset = |asm: &mut Asm| {
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
            let key_offset: usize = fields.iter().take(field_idx).try_fold(0, |acc, (_, ty)| {
                // Decision vars are flattened in a given slots, so we look at the raw size
                // in words. For pub var and storage variables, we look at the "storage
                // size" which may yield different results (e.g. a `b256` is 4 words but
                // its storage size is 1
                if let Location::DecisionVar = location {
                    ty.size(handler, contract)
                } else {
                    ty.storage_or_pub_var_slots(handler, contract)
                }
                .map(|slots| acc + slots)
            })?;

            // Now offset using `Add`
            asm.push(Stack::Push(key_offset as i64).into());
            asm.push(Alu::Add.into());
            Ok(())
        };

        match location {
            Location::PubVar(_) => {
                // For pub vars, and before computing the offset, we need to swap first because the
                // computed key also includes the key length at the end
                asm.push(Stack::Swap.into());
                compile_offset(asm)?;
                asm.push(Stack::Swap.into());
            }
            Location::State(_) | Location::DecisionVar => {
                compile_offset(asm)?;
            }
            Location::Value | Location::Storage(_) => {
                unimplemented!("we'll handle these eventually as a fallback option")
            }
        }

        Ok(location)
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_union_expr(
        &self,
        handler: &Handler,
        asm: &mut Asm,
        union_expr_key: &ExprKey,
        tag: &crate::types::Path,
        value: &Option<ExprKey>,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        // Find the tag string in the union decl and convert to an index.
        let tag_num = union_expr_key
            .get_ty(contract)
            .get_union_variant_names(&contract.unions)
            .into_iter()
            .enumerate()
            .find_map(|(idx, variant_name)| (variant_name == tag[2..]).then_some(idx))
            .ok_or_else(|| {
                handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "Union tag not found in union decl",
                        span: contract.expr_key_to_span(*union_expr_key),
                    },
                })
            })?;

        // Push the tag and then compile the value if necessary.
        asm.push(Stack::Push(tag_num as i64).into());
        if let Some(value_key) = value {
            self.compile_expr(handler, asm, value_key, contract, pred)?;
        }

        Ok(Location::Value)
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_union_tag_is(
        &self,
        handler: &Handler,
        asm: &mut Asm,
        union_expr_key: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        // Get the location of the union but read just a single word, as we only want the tag at
        // the front.
        match self.compile_expr_pointer(handler, asm, union_expr_key, contract, pred)? {
            Location::DecisionVar => {
                asm.push(Stack::Push(1).into()); // len
                asm.push(Access::DecisionVarRange.into());
            }

            // Are these supported?
            Location::State(_)
            | Location::Storage { .. }
            | Location::PubVar { .. }
            | Location::Value => todo!("support union matches in non- decision variables?"),
        }

        Ok(Location::Value)
    }

    fn compile_union_get_value(
        &self,
        handler: &Handler,
        asm: &mut Asm,
        union_expr_key: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        let location = self.compile_expr_pointer(handler, asm, union_expr_key, contract, pred)?;
        match location {
            Location::State(_) | Location::PubVar { .. } | Location::DecisionVar => {
                // Skip the tag.
                asm.push(Stack::Push(1).into());
                asm.push(Alu::Add.into());
                Ok(location)
            }

            Location::Value | Location::Storage { .. } => {
                unimplemented!("union value or unions in storage")
            }
        }
    }
}
