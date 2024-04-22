use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::{BinaryOp, Expr, Immediate, UnaryOp},
    intermediate::{ExprKey, IntermediateIntent, Program, ProgramKind, State as StateVar},
    span::empty_span,
    types::{PrimitiveKind, Type},
};
use constraint_asm::{Access, Alu, Crypto, Op, Pred};
use essential_types::{
    intent::{Directive, Intent},
    slots::{Slots, StateSlot},
};
use state_asm::{ControlFlow, Memory, State, StateReadOp};
use std::collections::{BTreeMap, HashMap};

mod display;
#[cfg(test)]
mod tests;

#[derive(Debug, Default, Clone)]
pub struct Intents {
    pub kind: ProgramKind,
    pub intents: BTreeMap<String, Intent>,
}

impl Intents {
    pub const ROOT_INTENT_NAME: &'static str = "";

    /// The root intent is the one named `Intents::ROOT_INTENT_NAME`
    pub fn root_intent(&self) -> &Intent {
        self.intents.get(Self::ROOT_INTENT_NAME).unwrap()
    }
}

/// Convert a `Program` into `Intents`
pub fn program_to_intents(handler: &Handler, program: &Program) -> Result<Intents, ErrorEmitted> {
    let mut intents: BTreeMap<String, Intent> = BTreeMap::new();
    match program.kind {
        ProgramKind::Stateless => {
            let (name, ii) = program.iis.iter().next().unwrap();
            if let Ok(intent) = handler.scope(|handler| intent_to_asm(handler, ii)) {
                intents.insert(name.to_string(), intent);
            }
        }
        ProgramKind::Stateful => {
            for (name, ii) in program.iis.iter() {
                if name != Program::ROOT_II_NAME {
                    if let Ok(intent) = handler.scope(|handler| intent_to_asm(handler, ii)) {
                        intents.insert(name.to_string(), intent);
                    }
                }
            }
        }
    }

    if handler.has_errors() {
        return Err(handler.cancel());
    }

    Ok(Intents {
        kind: program.kind.clone(),
        intents,
    })
}

#[derive(Default)]
pub struct AsmBuilder {
    // Opcodes to read state
    s_asm: Vec<Vec<StateReadOp>>,
    // Opcodes to specify constraints
    c_asm: Vec<Vec<Op>>,
    // Collection of state slots
    s_slots: Vec<StateSlot>,
    // Maps indices of `let` variables (which may be wider than a word) to a list of low level
    // word-wide decision variables
    var_to_d_vars: HashMap<usize, Vec<usize>>,
}

impl AsmBuilder {
    /// Generates assembly for producing a storage key  where `expr` is stored.
    /// Returns a bool indicating whether this is an external key (i.e. a key that we need to
    /// access via `StateReadWordRangeExtern`)
    fn compile_state_key(
        &mut self,
        handler: &Handler,
        s_asm: &mut Vec<StateReadOp>,
        expr: &ExprKey,
        intent: &IntermediateIntent,
    ) -> Result<bool, ErrorEmitted> {
        match &intent.exprs[*expr] {
            Expr::FnCall { name, args, .. } => {
                if name.ends_with("::storage_lib::get") {
                    // Expecting a single argument that is a `b256`: a key
                    assert_eq!(args.len(), 1);

                    let mut asm = Vec::new();
                    self.compile_expr(handler, &mut asm, &args[0], intent)?;
                    s_asm.extend(asm.iter().map(|op| StateReadOp::Constraint(*op)));
                    Ok(false)
                } else if name.ends_with("::storage_lib::get_extern") {
                    // Expecting two arguments that are both `b256`: an address and a key
                    assert_eq!(args.len(), 2);

                    // First, get the set-of-intents address and the storage key
                    let mut asm = Vec::new();
                    self.compile_expr(handler, &mut asm, &args[0], intent)?;
                    self.compile_expr(handler, &mut asm, &args[1], intent)?;
                    s_asm.extend(asm.iter().map(|op| StateReadOp::Constraint(*op)));
                    Ok(true)
                } else {
                    unimplemented!("Other calls are currently not supported")
                }
            }
            Expr::StorageAccess(name, _) => {
                let storage = &intent
                    .storage
                    .as_ref()
                    .expect("a storage block must have been declared")
                    .0;

                // Get the index of the storage variable in the storage block declaration
                let storage_index = storage
                    .iter()
                    .position(|var| var.name == *name)
                    .expect("storage access should have been checked before");

                // Get the storage key as the sum of the sizes of all the types in the storage
                // block that preceed the storage variable accessed.
                //
                // The actual storage key is a `b256` that is sum computed, left padded with 0s.
                let key: usize = storage
                    .iter()
                    .take(storage_index)
                    .map(|storage_var| storage_var.ty.size())
                    .sum();

                s_asm.extend(vec![
                    StateReadOp::Constraint(Op::Push(0)),
                    StateReadOp::Constraint(Op::Push(0)),
                    StateReadOp::Constraint(Op::Push(0)),
                    StateReadOp::Constraint(Op::Push(key as i64)),
                ]);
                Ok(false)
            }
            Expr::ExternalStorageAccess {
                extern_path, name, ..
            } => {
                // Get the `extern` declaration that the storage access refers to
                let r#extern = &intent
                    .externs
                    .iter()
                    .find(|e| e.name.to_string() == *extern_path)
                    .expect("an extern block named `extern_path` must have been declared");

                // Get the index of the storage variable in the storage block declaration
                let storage_index = r#extern
                    .storage_vars
                    .iter()
                    .position(|var| var.name == *name)
                    .expect("storage access should have been checked before");

                // Get the storage key as the sum of the sizes of all the types in the storage
                // block that preceed the storage variable accessed.
                //
                // The actual storage key is a `b256` that is sum computed, left padded with 0s.
                let key: usize = r#extern
                    .storage_vars
                    .iter()
                    .take(storage_index)
                    .map(|storage_var| storage_var.ty.size())
                    .sum();

                let Immediate::B256(val) = r#extern.address else {
                    panic!("the address of the external set-of-intents must be a `b256` immediate")
                };

                // Push the external set-of-intents address followed by the base key
                s_asm.extend(vec![
                    StateReadOp::Constraint(Op::Push(val[0] as i64)),
                    StateReadOp::Constraint(Op::Push(val[1] as i64)),
                    StateReadOp::Constraint(Op::Push(val[2] as i64)),
                    StateReadOp::Constraint(Op::Push(val[3] as i64)),
                    StateReadOp::Constraint(Op::Push(0)),
                    StateReadOp::Constraint(Op::Push(0)),
                    StateReadOp::Constraint(Op::Push(0)),
                    StateReadOp::Constraint(Op::Push(key as i64)),
                ]);

                // This is external!
                Ok(true)
            }
            Expr::Index { expr, index, .. } => {
                // Compile the key corresponding to `expr`
                let is_extern = self.compile_state_key(handler, s_asm, expr, intent)?;

                // Compile the index
                let mut asm = vec![];
                self.compile_expr(handler, &mut asm, index, intent)?;
                s_asm.extend(asm.iter().map(|op| StateReadOp::Constraint(*op)));

                // Sha256 the current key (4 words) with the compiled index to get the actual key.
                // We also need the length of the data to hash, so we rely on the size of the type
                // to get that.
                s_asm.push(StateReadOp::Constraint(Op::Push(
                    4 + intent.expr_types[*index].size() as i64,
                )));
                s_asm.push(StateReadOp::Constraint(Op::Crypto(Crypto::Sha256)));
                Ok(is_extern)
            }
            _ => unreachable!("there really shouldn't be anything else at this stage"),
        }
    }

    /// Generates assembly for an `ExprKey`.
    fn compile_expr(
        &mut self,
        handler: &Handler,
        asm: &mut Vec<Op>,
        expr: &ExprKey,
        intent: &IntermediateIntent,
    ) -> Result<(), ErrorEmitted> {
        // Always push to the vector of ops corresponding to the last constraint, i.e. the current
        // constraint being processed.
        //
        // Assume that there exists at least a single entry in `self.c_asm`.
        match &intent.exprs[*expr] {
            Expr::Immediate { value, .. } => match value {
                Immediate::Int(val) => asm.push(Op::Push(*val)),
                Immediate::B256(val) => {
                    asm.push(Op::Push(val[0] as i64));
                    asm.push(Op::Push(val[1] as i64));
                    asm.push(Op::Push(val[2] as i64));
                    asm.push(Op::Push(val[3] as i64));
                }
                _ => unimplemented!("other literal types are not yet supported"),
            },
            Expr::BinaryOp { op, lhs, rhs, .. } => {
                self.compile_expr(handler, asm, lhs, intent)?;
                self.compile_expr(handler, asm, rhs, intent)?;
                match op {
                    BinaryOp::Add => asm.push(Op::Alu(Alu::Add)),
                    BinaryOp::Sub => asm.push(Op::Alu(Alu::Sub)),
                    BinaryOp::Mul => asm.push(Op::Alu(Alu::Mul)),
                    BinaryOp::Div => asm.push(Op::Alu(Alu::Div)),
                    BinaryOp::Mod => asm.push(Op::Alu(Alu::Mod)),
                    BinaryOp::Equal => {
                        if matches!(
                            intent.expr_types[*lhs],
                            Type::Primitive {
                                kind: PrimitiveKind::B256,
                                ..
                            }
                        ) {
                            asm.push(Op::Pred(Pred::Eq4));
                        } else {
                            asm.push(Op::Pred(Pred::Eq));
                        }
                    }
                    BinaryOp::NotEqual => {
                        asm.push(Op::Pred(Pred::Eq));
                        asm.push(Op::Pred(Pred::Not));
                    }
                    BinaryOp::LessThanOrEqual => asm.push(Op::Pred(Pred::Lte)),
                    BinaryOp::LessThan => asm.push(Op::Pred(Pred::Lt)),
                    BinaryOp::GreaterThanOrEqual => {
                        asm.push(Op::Pred(Pred::Gte));
                    }
                    BinaryOp::GreaterThan => asm.push(Op::Pred(Pred::Gt)),
                    BinaryOp::LogicalAnd => asm.push(Op::Pred(Pred::And)),
                    BinaryOp::LogicalOr => asm.push(Op::Pred(Pred::Or)),
                }
            }
            Expr::UnaryOp { op, expr, .. } => {
                self.compile_expr(handler, asm, expr, intent)?;
                match op {
                    UnaryOp::Not => {
                        asm.push(Op::Pred(Pred::Not));
                    }
                    UnaryOp::NextState => {
                        // This assumes that the next state operator is applied on a state var path
                        // directly which should currently be guaranteed by the middleend.
                        //
                        // So, we simply change the second the last instruction to `Push(1)`
                        // instead of `Push(0)`. This changes the `delta` for the state read
                        // instruction from 0 to 1. We're basically switching from reading the
                        // current state to reading the next state.
                        assert!(matches!(
                            asm.last(),
                            Some(&Op::Access(Access::State | Access::StateRange))
                        ));
                        let len = asm.len();
                        assert!(len >= 2);
                        assert!(matches!(asm.get(asm.len() - 2), Some(&Op::Push(0))));
                        asm[len - 2] = Op::Push(1);
                    }
                    UnaryOp::Neg => unimplemented!("Unary::Neg is not yet supported"),
                    UnaryOp::Error => unreachable!("unexpected Unary::Error"),
                }
            }
            Expr::PathByName(path, _) => {
                // Search for a decision variable or a state variable.
                self.compile_path(asm, &path.to_string(), intent);
            }
            Expr::PathByKey(var_key, _) => {
                // Search for a decision variable or a state variable.
                self.compile_path(asm, &intent.vars[*var_key].name, intent);
            }
            Expr::StorageAccess(_, _) | Expr::ExternalStorageAccess { .. } => {
                return Err(handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "unexpected storage access",
                        span: empty_span(),
                    },
                }));
            }
            Expr::FnCall { name, args, .. } if name.ends_with("::context::sender") => {
                assert!(args.is_empty());
                asm.push(Op::Access(Access::Sender));
            }
            Expr::FnCall { .. } | Expr::If { .. } => {
                unimplemented!("calls and `if` expressions are not yet supported")
            }
            Expr::Error(_)
            | Expr::MacroCall { .. }
            | Expr::Array { .. }
            | Expr::Index { .. }
            | Expr::Tuple { .. }
            | Expr::TupleFieldAccess { .. }
            | Expr::Cast { .. }
            | Expr::In { .. }
            | Expr::Range { .. }
            | Expr::Generator { .. } => {
                return Err(handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "Unexpected expression during assembly generation",
                        span: empty_span(),
                    },
                }));
            }
        }
        Ok(())
    }

    /// Compile a path expression. Assumes that each path expressions corresponds to a decision
    /// variable or a state variable.
    fn compile_path(&mut self, asm: &mut Vec<Op>, path: &String, intent: &IntermediateIntent) {
        let var_index = intent.vars.iter().position(|var| &var.1.name == path);
        let state_and_index = intent
            .states
            .iter()
            .enumerate()
            .find(|(_, state)| &state.1.name == path);

        match (var_index, state_and_index) {
            (Some(var_index), None) => {
                for d_var in &self.var_to_d_vars[&var_index] {
                    asm.push(Op::Push(*d_var as i64));
                    asm.push(Op::Access(Access::DecisionVar));
                }
            }
            (None, Some((state_index, state))) => {
                let mut slot_index = 0;
                for (idx, state) in intent.states.iter().enumerate() {
                    if idx < state_index {
                        slot_index += intent.state_types[state.0].size();
                    } else {
                        break;
                    }
                }
                let size = intent.state_types[state.0].size();
                if size == 1 {
                    asm.push(Op::Push(slot_index as i64));
                    asm.push(Op::Push(0)); // 0 means "current state"
                    asm.push(Op::Access(Access::State));
                } else {
                    asm.push(Op::Push(slot_index as i64));
                    asm.push(Op::Push(size as i64)); // 0 means "current state"
                    asm.push(Op::Push(0)); // 0 means "current state"
                    asm.push(Op::Access(Access::StateRange));
                }
            }
            _ => unreachable!("guaranteed by semantic analysis"),
        }
    }

    /// Generates assembly for a given constraint
    fn compile_constraint(
        &mut self,
        handler: &Handler,
        expr: &ExprKey,
        intent: &IntermediateIntent,
    ) -> Result<(), ErrorEmitted> {
        let mut asm = Vec::new();
        self.compile_expr(handler, &mut asm, expr, intent)?;
        self.c_asm.push(asm);
        Ok(())
    }

    /// Generates assembly for a given state read
    fn compile_state(
        &mut self,
        handler: &Handler,
        state: &StateVar,
        slot_idx: &mut u32,
        intent: &IntermediateIntent,
    ) -> Result<(), ErrorEmitted> {
        let data_size = intent.expr_types[state.expr].size();
        let mut s_asm = Vec::new();

        // First, get the storage key
        let is_extern = self.compile_state_key(handler, &mut s_asm, &state.expr, intent)?;

        // Now, using the data size of the accessed type, produce an `Alloc` followed by a
        // `StateReadWordRange` instruction or a `StateReadWordRangeExtern` instruction
        s_asm.extend(vec![
            StateReadOp::Constraint(Op::Push(data_size as i64)),
            StateReadOp::Memory(Memory::Alloc),
            StateReadOp::Constraint(Op::Push(data_size as i64)),
            if is_extern {
                StateReadOp::State(State::StateReadWordRangeExtern)
            } else {
                StateReadOp::State(State::StateReadWordRange)
            },
            StateReadOp::ControlFlow(ControlFlow::Halt),
        ]);
        self.s_asm.push(s_asm);

        // Now add the actual `StateSlot`
        let program_idx = self.s_asm.len() - 1;
        self.s_slots.push(StateSlot {
            index: *slot_idx,
            amount: data_size as u32,
            program_index: program_idx as u16,
        });
        *slot_idx += data_size as u32;
        Ok(())
    }
}

/// Converts a `crate::IntermediateIntent` into a `Intent` which
/// includes generating assembly for the constraints and for state reads.
pub fn intent_to_asm(
    handler: &Handler,
    final_intent: &IntermediateIntent,
) -> Result<Intent, ErrorEmitted> {
    let mut builder = AsmBuilder::default();

    // low level decision variable index
    let mut d_var = 0;
    for (idx, var) in final_intent.vars.iter().enumerate() {
        if matches!(
            final_intent.var_types[var.0],
            Type::Primitive {
                kind: PrimitiveKind::B256,
                ..
            }
        ) {
            // `B256` variables map to 4 separate low level decision variables, 1 word wide each.
            builder
                .var_to_d_vars
                .insert(idx, vec![d_var, d_var + 1, d_var + 2, d_var + 3]);
            d_var += 4;
        } else {
            // All other primitive types (ignoring strings) are 1 word wide.
            builder.var_to_d_vars.insert(idx, vec![d_var]);
            d_var += 1;
        }
    }
    let total_decision_vars = d_var;

    let mut slot_idx = 0;
    for (_, state) in &final_intent.states {
        let _ = builder.compile_state(handler, state, &mut slot_idx, final_intent);
    }

    for (constraint, _) in &final_intent.constraints {
        let _ = builder.compile_constraint(handler, constraint, final_intent);
    }

    if handler.has_errors() {
        return Err(handler.cancel());
    }

    Ok(Intent {
        slots: Slots {
            state: builder.s_slots,
            decision_variables: total_decision_vars as u32,
            permits: 0,
        },
        state_read: builder
            .s_asm
            .iter()
            .map(|s_asm| serde_json::to_vec(s_asm).unwrap())
            .collect(),
        constraints: builder
            .c_asm
            .iter()
            .map(|constraint| serde_json::to_vec(&constraint).unwrap())
            .collect(),
        directive: Directive::Satisfy,
    })
}
