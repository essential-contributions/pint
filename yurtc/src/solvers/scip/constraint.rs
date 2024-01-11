use super::invert::invert_expression;
use crate::{error::SolveError, expr, intent::Expression, span::empty_span};
use russcip::{prelude::*, ProblemCreated};

impl<'a> super::Solver<'a, ProblemCreated> {
    pub(super) fn convert_constraint(&mut self, constraint: &Expression) -> Result<(), SolveError> {
        let new_cons_name = self.new_cons_name();
        match constraint {
            Expression::Immediate(imm) => {
                match imm {
                    expr::Immediate::Bool(val) => {
                        if !val {
                            // If the immediate is `false`, then insert a trivially infeasible
                            // constraint: 0 == 1
                            self.model.add_cons(vec![], &[], 1., 1., &new_cons_name);
                        }
                    }
                    _ => return Err(SolveError::Internal {
                        msg:
                            "(scip) attempting to convert a non-Boolean immediate into a constraint",
                        span: empty_span(),
                    }),
                }
            }

            Expression::Path(_) => {
                // Convert the constraint expression itself into a variable and enforce the
                // variable to be equal to 1.
                let var = self.expr_to_var(constraint)?;
                self.model
                    .add_cons(vec![var.clone()], &[1.], 1., 1., &new_cons_name);
            }

            Expression::UnaryOp { op, expr } => match op {
                expr::UnaryOp::Not => {
                    let not_expr = invert_expression(expr)?;
                    self.convert_constraint(&not_expr)?;
                }
                _ => {
                    return Err(SolveError::Internal {
                        msg: "(scip) Non-Boolean binary op expression cannot be constraints",
                        span: empty_span(),
                    })
                }
            },

            Expression::BinaryOp {
                op,
                lhs: lhs_expr,
                rhs: rhs_expr,
            } => {
                // Convert the LHS and RHS into new variables
                let lhs = self.expr_to_var(lhs_expr)?;
                let rhs = self.expr_to_var(rhs_expr)?;
                let lhs_type = lhs.var_type();
                let rhs_type = rhs.var_type();

                match op {
                    expr::BinaryOp::Equal => {
                        // 0 <= lhs - rhs <= 0
                        self.model.add_cons(
                            vec![lhs.clone(), rhs.clone()],
                            &[1., -1.],
                            0.,
                            0.,
                            &new_cons_name,
                        );
                    }

                    expr::BinaryOp::NotEqual => {
                        if lhs_type != VarType::Continuous && rhs_type != VarType::Continuous {
                            // Use indicator constraints here: `lhs != rhs` is equivalent to:
                            // ```
                            // lhs <= rhs - 1 OR lhs >= rhs + 1
                            // ```
                            // Introduce binary variables `bin1` and `bin2` such that:
                            // ```
                            // bin1 *implies* lhs <= rhs - 1
                            // bin2 *implies* lhs >= rhs + 1
                            // ```
                            // Then, ensure that `bin1 + bin2 == 1`, i.e., exactly one of the two
                            // conditions above must be enforced

                            // `bin1 *implies* lhs -rhs <= - 1`
                            let bin1_name = self.new_var_name();
                            let bin1 = self.model.add_var(0., 1., 0., &bin1_name, VarType::Binary);
                            let ind_cons1_name = self.new_cons_name();
                            self.model.add_cons_indicator(
                                bin1.clone(),
                                vec![lhs.clone(), rhs.clone()],
                                &mut [1., -1.],
                                -1.,
                                &ind_cons1_name,
                            );

                            // `bin2 *implies* -lhs + rhs <= -1`
                            let bin2_name = self.new_var_name();
                            let bin2 = self.model.add_var(0., 1., 0., &bin2_name, VarType::Binary);
                            let ind_cons2_name = self.new_cons_name();
                            self.model.add_cons_indicator(
                                bin2.clone(),
                                vec![lhs.clone(), rhs.clone()],
                                &mut [-1., 1.],
                                -1.,
                                &ind_cons2_name,
                            );

                            // `bin1 + bin2 == 1`
                            let card_cons = self.new_cons_name();
                            self.model.add_cons(
                                vec![bin1.clone(), bin2.clone()],
                                &[1., 1.],
                                1.,
                                1.,
                                &card_cons,
                            );
                        } else {
                            return Err(SolveError::Internal {
                                msg:
                                    "(scip) not equal constraint is not supported for non-integers",
                                span: empty_span(),
                            });
                        }
                    }

                    expr::BinaryOp::LessThanOrEqual => {
                        // 0 <= -lhs + rhs <= f64::INFINITY
                        self.model.add_cons(
                            vec![lhs.clone(), rhs.clone()],
                            &[-1., 1.],
                            0.,
                            f64::INFINITY,
                            &new_cons_name,
                        );
                    }

                    expr::BinaryOp::LessThan => {
                        if lhs_type != VarType::Continuous && rhs_type != VarType::Continuous {
                            // 1 <= -lhs + rhs <= f64::INFINITY
                            self.model.add_cons(
                                vec![lhs.clone(), rhs.clone()],
                                &[-1., 1.],
                                1.,
                                f64::INFINITY,
                                &new_cons_name,
                            );
                        } else {
                            return Err(SolveError::Internal {
                                msg: "(scip) strict inequalities not supported for non-integers",
                                span: empty_span(),
                            });
                        }
                    }

                    expr::BinaryOp::GreaterThanOrEqual => {
                        // 0 <= lhs - rhs <= f64::INFINITY
                        self.model.add_cons(
                            vec![lhs.clone(), rhs.clone()],
                            &[1., -1.],
                            0.,
                            f64::INFINITY,
                            &new_cons_name,
                        );
                    }

                    expr::BinaryOp::GreaterThan => {
                        if lhs_type != VarType::Continuous && rhs_type != VarType::Continuous {
                            // 1 <= lhs - rhs <= f64::INFINITY
                            self.model.add_cons(
                                vec![lhs.clone(), rhs.clone()],
                                &[1., -1.],
                                1.,
                                f64::INFINITY,
                                &new_cons_name,
                            );
                        } else {
                            return Err(SolveError::Internal {
                                msg: "(scip) strict inequalities not supported for non-integers",
                                span: empty_span(),
                            });
                        }
                    }

                    expr::BinaryOp::LogicalAnd => {
                        // enforce both the lhs and the rhs as seaprate constraints
                        self.convert_constraint(lhs_expr)?;
                        self.convert_constraint(rhs_expr)?;
                    }

                    expr::BinaryOp::LogicalOr => {
                        // At least one of the operands should be 1
                        // `1 <= lhs + rhs <= 2`
                        self.model.add_cons(
                            vec![lhs.clone(), rhs.clone()],
                            &[1., 1.],
                            1.,
                            2.,
                            &new_cons_name,
                        );
                    }

                    _ => {
                        return Err(SolveError::Internal {
                            msg: "(scip) Non-Boolean unary op expressions cannot be constraints",
                            span: empty_span(),
                        })
                    }
                }
            }

            _ => {
                return Err(SolveError::Internal {
                    msg: "(scip) only binary operators in constraints are expected",
                    span: empty_span(),
                })
            }
        }
        Ok(())
    }
}
