use crate::{
    error::CompileError,
    expr::{BinaryOp, Expr, Immediate, UnaryOp},
    intermediate::{ExprKey, IntermediateIntent, State as StateVar},
    span::empty_span,
    types::{PrimitiveKind, Type},
};
use constraint_asm::{Access, Alu, Op, Pred};
use essential_types::{
    intent::{Directive, Intent},
    slots::{Slots, StateSlot},
};
use state_asm::*;
use std::collections::HashMap;

#[cfg(test)]
mod tests;

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
        state: &StateVar,
        intent: &IntermediateIntent,
    ) -> Result<(), CompileError> {
        // If the RHS of `state` variable initialization is a call of the form
        // `::storage::get(..)`, then this is a read from state. This is pretty simplistic and
        // fragile for now and is likely to change in the future.
        if let Expr::FnCall { name, args, .. } = &intent.exprs[state.expr] {
            if name.ends_with("::storage::get") {
                // Expecting a single argument that is an integer
                assert_eq!(args.len(), 1);

                let mut asm = Vec::new();

                // Compile key
                self.compile_expr(&mut asm, &args[0], intent)?;
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
                self.compile_expr(&mut asm, &args[0], intent)?;
                self.compile_expr(&mut asm, &args[1], intent)?;
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
        asm: &mut Vec<Op>,
        expr: &ExprKey,
        intent: &IntermediateIntent,
    ) -> Result<(), CompileError> {
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
                self.compile_expr(asm, lhs, intent)?;
                self.compile_expr(asm, rhs, intent)?;
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
                self.compile_expr(asm, expr, intent)?;
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
            | Expr::ForAll { .. } => {
                return Err(CompileError::Internal {
                    msg: "Unexpected expression during assembly generation",
                    span: empty_span(),
                });
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
        expr: &ExprKey,
        intent: &IntermediateIntent,
    ) -> Result<(), CompileError> {
        let mut asm = Vec::new();
        self.compile_expr(&mut asm, expr, intent)?;
        self.c_asm.push(asm);
        Ok(())
    }
}

/// Converts a `crate::IntermediateIntent` into a `Intent` which
/// includes generating assembly for the constraints and for state reads.
pub fn intent_to_asm(final_intent: &IntermediateIntent) -> Result<Intent, CompileError> {
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
        builder.compile_state(state, final_intent)?;
    }

    for (constraint, _) in &final_intent.constraints {
        builder.compile_constraint(constraint, final_intent)?;
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

/// Given an `Intent`, print the contained assembly. This prints both the constraints assembly as
/// well as the state reads assembly.
pub fn print_asm(intent: &essential_types::intent::Intent) {
    println!("\n;; --- Constraints ---");
    for (idx, constraint) in intent.constraints.iter().enumerate() {
        let ops: Vec<Op> = serde_json::from_str(&String::from_utf8_lossy(constraint)).unwrap();
        println!("\nconstraint {idx}");
        for op in ops {
            println!("  {:?}", op);
        }
    }
    println!("\n;; --- State Reads ---");
    for (idx, state_read) in intent.state_read.iter().enumerate() {
        let ops: Vec<StateReadOp> =
            serde_json::from_str(&String::from_utf8_lossy(state_read)).unwrap();
        println!("\nstate read {idx}");
        for op in ops {
            println!("  {:?}", op);
        }
    }
}
