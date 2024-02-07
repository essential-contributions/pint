use crate::{
    error::CompileError,
    expr::{BinaryOp, Immediate, UnaryOp},
    intent::{Expression, Intent, StateVar},
};
use constraint_asm::*;
use essential_types::slots::*;
use state_asm::*;

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
}

impl AsmBuilder {
    /// Generates assembly for reading from state and creates a new state slot
    /// This is pretty limited for now and only supports simple integer reads using the syntax
    /// `state x = storage::get(<key>)`
    fn compile_state(&mut self, state: &StateVar, idx: usize) -> Result<(), CompileError> {
        // If the RHS of `state` variable initialization is a call of the form
        // `::storage::get(..)`, then this is a read from state. This is pretty simplistic and
        // fragile for now and is likely to change in the future.
        if let Expression::Call { name, args } = &state.expr {
            if name.ends_with("::storage::get") {
                // Expecting a single argument that is an integer
                assert_eq!(args.len(), 1);
                let arg = &args[0];
                let arg = match arg {
                    Expression::Immediate(Immediate::BigInt(val)) => val.clone(),
                    Expression::Immediate(Immediate::Int(val)) => (*val).into(),
                    _ => panic!("bad storage::get argument"),
                };

                // State assembly for reading into a slot
                let mut s_asm = vec![];
                s_asm.push(StateReadOp::Constraint(Op::Push(1)));
                s_asm.push(StateReadOp::Memory(Memory::Alloc));
                let digits = arg.to_u64_digits().1;
                for d in (0..4usize.saturating_sub(digits.len()))
                    .map(|_| 0)
                    .chain(digits.into_iter().rev())
                {
                    s_asm.push(StateReadOp::Constraint(Op::Push(d)));
                }
                s_asm.push(StateReadOp::Constraint(Op::Push(1)));
                s_asm.push(StateReadOp::State(State::StateReadWordRange));
                s_asm.push(StateReadOp::ControlFlow(ControlFlow::Halt));
                self.s_asm.push(s_asm);

                self.s_slots.push(StateSlot {
                    index: idx as u32,
                    amount: 1,
                    program_index: idx as u16,
                });
            }
        }

        Ok(())
    }

    /// Generates assembly for an `Expression`.
    fn compile_expr(&mut self, expr: &Expression, intent: &Intent) -> Result<(), CompileError> {
        // Always push to the vector of ops corresponding to the last constraint, i.e. the current
        // constraint being processed.
        //
        // Assume that there exists at least a single entry in `self.c_asm`.
        assert!(!self.c_asm.is_empty());
        let idx = self.c_asm.len() - 1;
        match expr {
            Expression::Immediate(imm) => match imm {
                Immediate::Int(val) => self.c_asm[idx].push(Op::Push(*val as u64)),
                Immediate::Bool(val) => self.c_asm[idx].push(Op::Push(*val as u64)),
                _ => unimplemented!("other literal types are not yet supported"),
            },
            Expression::BinaryOp { op, lhs, rhs } => {
                self.compile_expr(lhs, intent)?;
                self.compile_expr(rhs, intent)?;
                match op {
                    BinaryOp::Add => self.c_asm[idx].push(Op::Alu(Alu::Add)),
                    BinaryOp::Sub => self.c_asm[idx].push(Op::Alu(Alu::Sub)),
                    BinaryOp::Mul => self.c_asm[idx].push(Op::Alu(Alu::Mul)),
                    BinaryOp::Div => self.c_asm[idx].push(Op::Alu(Alu::Div)),
                    BinaryOp::Mod => self.c_asm[idx].push(Op::Alu(Alu::Mod)),
                    BinaryOp::Equal => self.c_asm[idx].push(Op::Pred(Pred::Eq)),
                    BinaryOp::NotEqual => {
                        self.c_asm[idx].push(Op::Pred(Pred::Eq));
                        self.c_asm[idx].push(Op::Pred(Pred::Not));
                    }
                    BinaryOp::LessThanOrEqual => self.c_asm[idx].push(Op::Pred(Pred::Lte)),
                    BinaryOp::LessThan => self.c_asm[idx].push(Op::Pred(Pred::Lt)),
                    BinaryOp::GreaterThanOrEqual => {
                        self.c_asm[idx].push(Op::Pred(Pred::Gte));
                    }
                    BinaryOp::GreaterThan => self.c_asm[idx].push(Op::Pred(Pred::Gt)),
                    BinaryOp::LogicalAnd => self.c_asm[idx].push(Op::Pred(Pred::And)),
                    BinaryOp::LogicalOr => self.c_asm[idx].push(Op::Pred(Pred::Or)),
                }
            }
            Expression::UnaryOp { op, expr } => {
                self.compile_expr(expr, intent)?;
                match op {
                    UnaryOp::Not => {
                        self.c_asm[idx].push(Op::Pred(Pred::Not));
                    }
                    UnaryOp::NextState => {
                        // This is pretty hacky. It assumes that the next state operator is applied
                        // on a state var path directly. So, it pops out the idx two instructions
                        // which would have to be `Push(0)` and `Access(Access::State)`, and adds
                        // `Push(1)` and a `Access(Access:State)`. We're basically switching from
                        // reading the current state to reading the next state.
                        assert!(matches!(
                            self.c_asm[idx].last(),
                            Some(&Op::Access(Access::State))
                        ));
                        self.c_asm[idx].pop();

                        assert!(matches!(self.c_asm[idx].last(), Some(&Op::Push(0))));
                        self.c_asm[idx].pop();

                        self.c_asm[idx].push(Op::Push(1)); // 1 means "next state"
                        self.c_asm[idx].push(Op::Access(Access::State));
                    }
                    UnaryOp::Neg => unimplemented!("Unary::Neg is not yet supported"),
                    UnaryOp::Error => unreachable!("unexpected Unary::Error"),
                }
            }
            Expression::Path(path) => {
                // Search for a decision variable or a state variable.
                let var_index = intent.vars.iter().position(|var| &var.name == path);
                let state_index = intent.states.iter().position(|state| &state.name == path);
                match (var_index, state_index) {
                    (Some(var_index), None) => {
                        self.c_asm[idx].push(Op::Push(var_index as u64));
                        self.c_asm[idx].push(Op::Access(Access::DecisionVar));
                    }
                    (None, Some(state_index)) => {
                        self.c_asm[idx].push(Op::Push(state_index as u64));
                        self.c_asm[idx].push(Op::Push(0)); // 0 means "current state"
                        self.c_asm[idx].push(Op::Access(Access::State));
                    }
                    _ => unreachable!("guaranteed by semantic analysis"),
                }
            }
            Expression::Call { .. } | Expression::If { .. } => {
                unimplemented!("calls and if expressions are not yet supported")
            }
        }
        Ok(())
    }

    /// Generates assembly for a given constraint
    fn compile_constraint(
        &mut self,
        expr: &Expression,
        intent: &Intent,
    ) -> Result<(), CompileError> {
        self.c_asm.push(vec![]);

        self.compile_expr(expr, intent)
    }
}

/// Converts a `crate::intent::Intent` into a `essential_types::intent::Intent` which includes
/// generating assembly for the constraints and for state reads.
pub fn intent_to_asm(
    final_intent: &Intent,
) -> Result<essential_types::intent::Intent, CompileError> {
    let mut builder = AsmBuilder::default();

    for (idx, state) in final_intent.states.iter().enumerate() {
        builder.compile_state(state, idx)?;
    }

    for constraint in &final_intent.constraints {
        builder.compile_constraint(constraint, final_intent)?;
    }

    Ok(essential_types::intent::Intent {
        slots: Slots {
            state: builder.s_slots,
            decision_variables: final_intent.vars.len() as u32,
            input_message_args: Some(vec![]),
            output_messages_args: vec![],
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
        directive: essential_types::intent::Directive::Satisfy,
    })
}
