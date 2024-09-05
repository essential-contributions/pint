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
    Access, Alu, Constraint as ConstraintOp, Crypto, Op as StateOp, Pred, Stack, StateMemory,
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

    // A reference to a `HahsMap` from predicate names to the compiled predicates and their
    // addresses
    compiled_predicates: &'a HashMap<String, (CompiledPredicate, ContentAddress)>,

    // A map from names of state variables to their chosen state slot indices. Each state variable
    // is stored in a single slot.
    state_var_to_slot_indices: HashMap<String, usize>,

    // A map from storage access expressions to their chosen state slot indices. Each storage
    // access spans one or more consecutive slots, hence the `Range`.
    storage_access_to_slot_indices: HashMap<ExprKey, std::ops::Range<usize>>,

    // This keeps track of the total number of slots allocated so far, globally
    global_state_slots: usize,
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
/// 4. `PubVar` expressions refer to expressions that require the `PubVar` opcode.
/// 5. `Value` expressions are just raw values such as immediates or the outputs of binary ops.
enum Location {
    DecisionVar,
    State(bool),
    Storage(bool),
    PubVar,
    Value,
}

impl<'a> AsmBuilder<'a> {
    /// Creates a new `AsmBuilder` given a set of compiled predicates and their addresses.
    pub fn new(
        compiled_predicates: &'a HashMap<String, (CompiledPredicate, ContentAddress)>,
    ) -> Self {
        Self {
            state_programs: Vec::new(),
            constraint_programs: Vec::new(),
            compiled_predicates,
            state_var_to_slot_indices: HashMap::new(),
            storage_access_to_slot_indices: HashMap::new(),
            global_state_slots: 0,
        }
    }

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
        let mut local_state_slots = 0;

        // Allocates `num_slots` number of state slots. Returns the local and global indices of the
        // first slot allocated. The local index is the index in a given state program, which
        // starts at 0 for every state program. The global index is a unique index across all the
        // state programs.
        let mut allocate = |num_slots: usize| -> Result<(usize, usize), ErrorEmitted> {
            asm.push(Stack::Push(num_slots as i64).into());
            asm.try_push(handler, StateMemory::AllocSlots.into())?;
            local_state_slots += num_slots;
            self.global_state_slots += num_slots;
            Ok((
                local_state_slots - num_slots,
                self.global_state_slots - num_slots,
            ))
        };

        // Allocate a single slot for the state var. Keep track of the local and global indices for
        // this newly allocated slot.
        let (state_var_local_slot_index, state_var_global_slot_index) = allocate(1)?;

        // Collect all storage accesses used in the state var initializer, and allocate enough
        // slots for all of them. We do this ahead of time so that we know exactly how many slots
        // are allocated. Due to short-circuting, this also means that some allocations may not be
        // used, but this is okay for now.
        for access in state.expr.collect_storage_accesses(contract) {
            // This is how many slots this storage access requires
            let num_keys_to_read = access.get_ty(contract).storage_slots(handler, contract)?;

            // Now, allocate
            let base_slot_index = allocate(num_keys_to_read)?.0;

            // Keep track of the local indices of the newly allocated slots
            self.storage_access_to_slot_indices
                .insert(access, base_slot_index..base_slot_index + num_keys_to_read);
        }

        // Prepare for the `StateMemory::Store` opcode
        asm.push(Stack::Push(state_var_local_slot_index as i64).into()); // slot_ix
        asm.push(Stack::Push(0).into()); // value_ix

        if let Some(state_slots) =
            self.compile_expr_pointer_deref(handler, &mut asm, &state.expr, contract, pred)?
        {
            // If the result is stored state slots, then load those slots to the stack
            for i in state_slots.start..state_slots.end {
                asm.push(Stack::Push(i as i64).into()); // slot_ix

                asm.push(Stack::Push(0).into()); // value_ix

                asm.push(Stack::Push(i as i64).into());
                asm.try_push(handler, StateMemory::ValueLen.into())?; // len, using `ValueLen`

                asm.try_push(handler, StateMemory::Load.into())?;
            }

            // Then, find the _total_ number of words loaded
            asm.push(Stack::Push(0).into());
            for i in state_slots.start..state_slots.end {
                asm.push(Stack::Push(i as i64).into());
                asm.try_push(handler, StateMemory::ValueLen.into())?;
                asm.push(Alu::Add.into());
            }
        } else {
            // Otherwise, the data is already on the stack. Just follow with the size of the data
            // according to the state expr type.
            asm.push(
                Stack::Push(state.expr.get_ty(contract).size(handler, contract)? as i64).into(),
            );
        }

        // Now, store the result into the slot allocated for the state var
        asm.try_push(handler, StateMemory::Store.into())?;
        asm.try_push(handler, TotalControlFlow::Halt.into())?;

        // Keep track of the global index of the state slot where this state variable lives
        self.state_var_to_slot_indices
            .insert(state.name.clone(), state_var_global_slot_index);

        // Clear out this map because it's local to each state variable
        self.storage_access_to_slot_indices.clear();

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
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        expr: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<usize, ErrorEmitted> {
        let old_asm_len = asm.len();

        if let Some(state_slots) =
            self.compile_expr_pointer_deref(handler, asm, expr, contract, pred)?
        {
            // If the result is stored state slots, then load those slots to the stack
            for i in state_slots.start..state_slots.end {
                asm.push(Stack::Push(i as i64).into());

                asm.push(Stack::Push(0).into()); // value_ix

                asm.push(Stack::Push(i as i64).into());
                asm.try_push(handler, StateMemory::ValueLen.into())?; // len

                asm.try_push(handler, StateMemory::Load.into())?;
            }
        }
        Ok(asm.len() - old_asm_len)
    }

    /// Dereferences a pointer using the appropridate opcode depending on the location of the
    /// pointer. If the pointer is a "storage" pointer, then the data is read in state slots (not
    /// pushed on the stack) and the (local) range of those slots is returned.
    fn compile_expr_pointer_deref(
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        expr: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Option<std::ops::Range<usize>>, ErrorEmitted> {
        let expr_ty = expr.get_ty(contract);
        match self.compile_expr_pointer(handler, asm, expr, contract, pred)? {
            Location::DecisionVar => {
                asm.push(Stack::Push(expr_ty.size(handler, contract)? as i64).into()); // len
                asm.push(Access::DecisionVar.into());
                Ok(None)
            }

            Location::State(next_state) => {
                asm.push(Stack::Push(expr_ty.size(handler, contract)? as i64).into()); // value_len
                asm.push(Stack::Push(next_state as i64).into()); // delta
                asm.push(Access::State.into());
                Ok(None)
            }

            Location::Storage(is_extern) => {
                let num_keys_to_read = expr_ty.storage_slots(handler, contract)?;

                // Allocate as many slots as we have keys
                let base_slot_index = self.storage_access_to_slot_indices[expr].start;

                // Read the storage keys into the slots, starting with `base_slot_index`
                asm.push(Stack::Push(num_keys_to_read as i64).into()); // num_keys_to_read
                asm.push(Stack::Push(base_slot_index as i64).into()); // slot_index
                asm.try_push(
                    handler,
                    if is_extern {
                        StateOp::KeyRangeExtern
                    } else {
                        StateOp::KeyRange
                    },
                )?;

                // Storage accesses are read into state slots, so return the range of those slots
                Ok(Some(base_slot_index..num_keys_to_read + base_slot_index))
            }

            Location::PubVar => {
                asm.push(Stack::Push(expr_ty.size(handler, contract)? as i64).into()); // value_len
                asm.push(ConstraintOp::Access(Access::PubVar));
                Ok(None)
            }

            Location::Value => Ok(None),
        }
    }

    /// Generates assembly for an `ExprKey` as a _pointer_. What this means is that, if the expr
    /// refers to something other than a "value" (like an immedate), we generate assembly for the
    /// pointer (i.e. `Location`) only. A "pointer" may be a state slot or a  pub var data key for
    /// example.
    fn compile_expr_pointer(
        &mut self,
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
            Expr::Path(path, _) => self.compile_path(handler, asm, path, pred),
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
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        path: &String,
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
        } else if pred.states().any(|(_, state)| &state.name == path) {
            asm.push(Stack::Push(self.state_var_to_slot_indices[path] as i64).into()); // slot
            asm.push(Stack::Push(0).into()); // placeholder for index computation
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
        &mut self,
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
        &mut self,
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
        &mut self,
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
        &mut self,
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

            ExternalIntrinsic::SizeOf => {
                // SizeOf is handled separately from other intrinsics, for now.
                match self.compile_expr_pointer(handler, asm, &args[0], contract, pred)? {
                    Location::State(next_state) => {
                        println!("{}", contract.with_ctrct(&args[0]));
                        // Remove the placeholder for index computation since it is not needed for
                        // the `SizeOf` opcode.
                        asm.push(Stack::Pop.into());

                        asm.push(Stack::Push(next_state as i64).into()); // delta
                        asm.push(Access::StateLen.into()); // Range length for State
                    }
                    Location::Storage(is_extern) => {
                        let num_keys_to_read =
                            args[0].get_ty(contract).storage_slots(handler, contract)?;

                        // Allocate as many slots as we have keys
                        let base_slot_index = self.storage_access_to_slot_indices[&args[0]].start;

                        // Read the storage keys into the slots, starting with `base_slot_index`
                        asm.push(Stack::Push(num_keys_to_read as i64).into()); // num_keys_to_read
                        asm.push(Stack::Push(base_slot_index as i64).into()); // slot_index
                        asm.try_push(
                            handler,
                            if is_extern {
                                StateOp::KeyRangeExtern
                            } else {
                                StateOp::KeyRange
                            },
                        )?;

                        // Sum the `ValueLen` of all the state slots involved.
                        asm.push(Stack::Push(0).into());
                        for i in base_slot_index..num_keys_to_read + base_slot_index {
                            asm.push(Stack::Push(i as i64).into());
                            asm.try_push(handler, StateMemory::ValueLen.into())?;
                            asm.push(Alu::Add.into());
                        }
                    }
                    Location::DecisionVar | Location::PubVar | Location::Value => {
                        // These "locations" can just rely on the knwon size of the type since they
                        // can't be `nil`.
                        asm.push(
                            Stack::Push(args[0].get_ty(contract).size(handler, contract)? as i64)
                                .into(),
                        );
                    }
                }
            }

            ExternalIntrinsic::VerifyEd25519 => {
                self.compile_expr(handler, asm, &args[0], contract, pred)?;
                asm.push(ConstraintOp::Stack(Stack::Push(
                    8 * args[0].get_ty(contract).size(handler, contract)? as i64,
                )));
                self.compile_expr(handler, asm, &args[1], contract, pred)?;
                self.compile_expr(handler, asm, &args[2], contract, pred)?;
                asm.push(ConstraintOp::Crypto(Crypto::VerifyEd25519))
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

                match kind {
                    ExternalIntrinsic::PredicateAt => {
                        asm.push(ConstraintOp::Access(Access::PredicateAt))
                    }

                    ExternalIntrinsic::RecoverSECP256k1 => {
                        asm.push(ConstraintOp::Crypto(Crypto::RecoverSecp256k1))
                    }

                    ExternalIntrinsic::Sha256 => {
                        asm.push(ConstraintOp::Stack(Stack::Push(8))); // placeholder for index
                        asm.push(Alu::Mul.into());
                        asm.push(ConstraintOp::Crypto(Crypto::Sha256))
                    }

                    ExternalIntrinsic::ThisAddress => {
                        asm.push(ConstraintOp::Access(Access::ThisAddress))
                    }

                    ExternalIntrinsic::ThisContractAddress => {
                        asm.push(ConstraintOp::Access(Access::ThisContractAddress))
                    }

                    ExternalIntrinsic::ThisPathway => {
                        asm.push(ConstraintOp::Access(Access::ThisPathway))
                    }

                    ExternalIntrinsic::AddressOf
                    | ExternalIntrinsic::SizeOf
                    | ExternalIntrinsic::VerifyEd25519 => {
                        unreachable!("SizeOf and AddressOf are handled above")
                    }

                    ExternalIntrinsic::VecLen => {
                        return Err(handler.emit_err(Error::Compile {
                            error: CompileError::Internal {
                                msg: "__vec_len should have been lowered to something else by now",
                                span: empty_span(),
                            },
                        }))
                    }
                }
            }
        }

        Ok(Location::Value)
    }

    fn compile_internal_intrinsic_call(
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        kind: &InternalIntrinsic,
        args: &[ExprKey],
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        for (i, arg) in args.iter().enumerate() {
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
            InternalIntrinsic::PubVar => {
                asm.push(ConstraintOp::Stack(Stack::Push(0))); // placeholder for index
                                                               // computations
                Ok(Location::PubVar)
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_select(
        &mut self,
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
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        expr: &ExprKey,
        index: &ExprKey,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        let location = self.compile_expr_pointer(handler, asm, expr, contract, pred)?;

        if let Location::Value | Location::Storage(_) = location {
            return Err(handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "unexpected index operator for `Location::Value` and `Location::Storage`",
                    span: empty_span(),
                },
            }));
        }

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

        // Multiply the index by the size of `ty` to get the offset, then add the result to the
        // base key
        asm.push(Stack::Push(ty.size(handler, contract)? as i64).into());
        asm.push(Alu::Mul.into());
        asm.push(Alu::Add.into());

        Ok(location)
    }

    fn compile_tuple_field_access(
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        tuple: &ExprKey,
        field: &TupleAccess,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        let location = self.compile_expr_pointer(handler, asm, tuple, contract, pred)?;

        if let Location::Value | Location::Storage(_) = location {
            return Err(handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "unexpected tuple access for `Location::Value` and `Location::Storage`",
                    span: empty_span(),
                },
            }));
        }

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

        // Use `Add` to compute the offset from the base key where the full tuple is stored.
        asm.push(
            Stack::Push(fields.iter().take(field_idx).try_fold(0, |acc, (_, ty)| {
                ty.size(handler, contract).map(|slots| acc + slots)
            })? as i64)
            .into(),
        );
        asm.push(Alu::Add.into());

        Ok(location)
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_union_expr(
        &mut self,
        handler: &Handler,
        asm: &mut Asm,
        union_expr_key: &ExprKey,
        tag: &crate::types::Path,
        value: &Option<ExprKey>,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<Location, ErrorEmitted> {
        // Find the tag string in the union decl and convert to an index.
        let union_ty = union_expr_key.get_ty(contract);
        let tag_num = union_ty
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

        // Track the actual value size in words.
        let mut actual_value_size = 0;

        // Push the tag and then compile the value if necessary.
        asm.push(Stack::Push(tag_num as i64).into());
        if let Some(value_key) = value {
            self.compile_expr(handler, asm, value_key, contract, pred)?;
            actual_value_size = value_key.get_ty(contract).size(handler, contract)?;
        }

        // Get the total union (max) size MINUS one since .size() includes the tag word.
        let union_value_size = union_ty.size(handler, contract)? - 1;
        while union_value_size > actual_value_size {
            // Pad out the value with zeros.
            asm.push(Stack::Push(0).into());
            actual_value_size += 1;
        }

        Ok(Location::Value)
    }

    #[allow(clippy::too_many_arguments)]
    fn compile_union_tag_is(
        &mut self,
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
                asm.push(Access::DecisionVar.into());
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
        &mut self,
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
