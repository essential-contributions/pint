use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::{BinaryOp, Expr, Immediate, UnaryOp},
    intermediate::{ExprKey, IntermediateIntent, Program, ProgramKind, State as StateVar},
    span::empty_span,
    types::{PrimitiveKind, Type},
};
use constraint_asm::{Access, Alu, Op, Pred};
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
    /// Generates assembly for reading from state and creates a new state slot
    /// This is pretty limited for now and only supports simple integer reads using the syntax
    /// `state x = storage::get(<key>)`
    fn compile_state(
        &mut self,
        handler: &Handler,
        state: &StateVar,
        intent: &IntermediateIntent,
    ) -> Result<(), ErrorEmitted> {
        // If the RHS of `state` variable initialization is a call of the form
        // `::storage::get(..)`, then this is a read from state. This is pretty simplistic and
        // fragile for now and is likely to change in the future.
        if let Expr::FnCall { name, args, .. } = &intent.exprs[state.expr] {
            if name.ends_with("::storage::get") {
                // Expecting a single argument that is an integer
                assert_eq!(args.len(), 1);

                let mut asm = Vec::new();

                // Compile key
                self.compile_expr(handler, &mut asm, &args[0], intent)?;
                let mut s_asm = asm
                    .iter()
                    .map(|op| StateReadOp::Constraint(*op))
                    .collect::<Vec<_>>();

                s_asm.push(StateReadOp::Constraint(Op::Push(1)));
                s_asm.push(StateReadOp::Memory(Memory::Alloc));
                s_asm.push(StateReadOp::Constraint(Op::Push(1)));
                s_asm.push(StateReadOp::State(State::StateReadWordRange));
                s_asm.push(StateReadOp::ControlFlow(ControlFlow::Halt));

                self.s_asm.push(s_asm);

                let slot_idx = self.s_asm.len() - 1;
                self.s_slots.push(StateSlot {
                    index: slot_idx as u32,
                    amount: 1,
                    program_index: slot_idx as u16,
                });
            } else if name.ends_with("::storage::get_extern") {
                // Expecting a single argument that is an integer
                assert_eq!(args.len(), 2);

                let mut asm = Vec::new();

                // persistent intent address followed by key
                self.compile_expr(handler, &mut asm, &args[0], intent)?;
                self.compile_expr(handler, &mut asm, &args[1], intent)?;
                let mut s_asm = asm
                    .iter()
                    .map(|op| StateReadOp::Constraint(*op))
                    .collect::<Vec<_>>();

                s_asm.push(StateReadOp::Constraint(Op::Push(1)));
                s_asm.push(StateReadOp::Memory(Memory::Alloc));
                s_asm.push(StateReadOp::Constraint(Op::Push(1)));
                s_asm.push(StateReadOp::State(State::StateReadWordRangeExtern));
                s_asm.push(StateReadOp::ControlFlow(ControlFlow::Halt));

                self.s_asm.push(s_asm);

                let slot_idx = self.s_asm.len() - 1;
                self.s_slots.push(StateSlot {
                    index: slot_idx as u32,
                    amount: 1,
                    program_index: slot_idx as u16,
                });
            } else {
                unimplemented!("Other calls are currently not supported")
            }
        }

        Ok(())
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
                        // This is pretty hacky. It assumes that the next state operator is applied
                        // on a state var path directly. So, it pops out the idx two instructions
                        // which would have to be `Push(0)` and `Access(Access::State)`, and adds
                        // `Push(1)` and a `Access(Access:State)`. We're basically switching from
                        // reading the current state to reading the next state.
                        assert!(matches!(asm.last(), Some(&Op::Access(Access::State))));
                        asm.pop();

                        assert!(matches!(asm.last(), Some(&Op::Push(0))));
                        asm.pop();

                        asm.push(Op::Push(1)); // 1 means "next state"
                        asm.push(Op::Access(Access::State));
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
            | Expr::ArrayElementAccess { .. }
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
        let state_index = intent.states.iter().position(|state| &state.1.name == path);
        match (var_index, state_index) {
            (Some(var_index), None) => {
                for d_var in &self.var_to_d_vars[&var_index] {
                    asm.push(Op::Push(*d_var as i64));
                    asm.push(Op::Access(Access::DecisionVar));
                }
            }
            (None, Some(state_index)) => {
                asm.push(Op::Push(state_index as i64));
                asm.push(Op::Push(0)); // 0 means "current state"
                asm.push(Op::Access(Access::State));
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

    for (_, state) in &final_intent.states {
        let _ = builder.compile_state(handler, state, final_intent);
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
