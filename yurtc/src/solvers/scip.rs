use crate::{
    error::SolveError,
    expr,
    intent::{self, Expression, Intent, SolveDirective, Type},
    span::empty_span,
};
use russcip::{prelude::*, ProblemCreated, Solved, Variable};
use std::{fmt::Write, rc::Rc};
use yansi::{Color, Style};

pub struct Solver<'a, State> {
    model: Model<State>,
    intent: &'a Intent,
    unique_var_suffix: usize, // unique suffix for variables introduced by the solver
    unique_cons_suffix: usize, // unique suffix for names of cosntraints
}

impl<'a, State> Solver<'a, State> {
    /// Creates a new instance of `Solver` given an `Intent`.
    pub fn new(intent: &'a Intent) -> Solver<ProblemCreated> {
        Solver {
            model: Model::new()
                .hide_output()
                .include_default_plugins()
                .create_prob("solver")
                .set_obj_sense(match intent.directive {
                    // For constraint satisfaction problems, `ObjSense` does not matter. The
                    // objective function is going to be set to 0 anyways.
                    SolveDirective::Minimize(_) | SolveDirective::Satisfy => ObjSense::Minimize,
                    SolveDirective::Maximize(_) => ObjSense::Maximize,
                }),
            intent,
            unique_var_suffix: 0,
            unique_cons_suffix: 0,
        }
    }

    fn new_var_name(&mut self) -> String {
        let new_name = format!("INTRODUCED{}", self.unique_var_suffix);
        self.unique_var_suffix += 1;
        new_name
    }

    fn new_cons_name(&mut self) -> String {
        let new_name = format!("CONS{}", self.unique_cons_suffix);
        self.unique_cons_suffix += 1;
        new_name
    }
}

impl<'a> Solver<'a, ProblemCreated> {
    pub fn solve(mut self) -> Result<Solver<'a, Solved>, SolveError> {
        // No state variables are allowed
        if !self.intent.states.is_empty() {
            return Err(SolveError::Internal {
                msg: "(scip) no state variables are allowed at this stage",
                span: empty_span(),
            });
        }

        // Convert all variables
        for variable in &self.intent.vars {
            self.convert_variable(variable)?;
        }

        // Convert all constraints
        for constraint in &self.intent.constraints {
            self.convert_constraint(constraint)?;
        }

        Ok(Solver {
            model: self.model.solve(),
            intent: self.intent,
            unique_var_suffix: self.unique_var_suffix,
            unique_cons_suffix: self.unique_cons_suffix,
        })
    }

    fn convert_variable(&mut self, variable: &intent::Variable) -> Result<(), SolveError> {
        self.model.add_var(
            /* Bounds */
            if matches!(variable.ty, Type::Bool) {
                0.
            } else {
                -f64::INFINITY
            }, // TODO: We can do better with a proper static analysis
            if matches!(variable.ty, Type::Bool) {
                1.
            } else {
                f64::INFINITY
            }, // TODO: We can do better with a proper static analysis
            /* Objective coefficient */
            //
            // Assume that the objective function is previously converted into a single path. Now,
            // Set the coefficient to 1 for this variable if it matches that path.
            match &self.intent.directive {
                SolveDirective::Satisfy => 0.,
                SolveDirective::Minimize(path) | SolveDirective::Maximize(path) => {
                    if path == &variable.name {
                        1.
                    } else {
                        0.
                    }
                }
            },
            /* Variable name */
            &variable.name.clone(),
            /* Variable type */
            match variable.ty {
                Type::Bool => VarType::Binary,
                Type::Int => VarType::Integer,
                Type::Real => VarType::Continuous,
                Type::String => {
                    return Err(SolveError::Internal {
                        msg: "(scip) string types are not yet supported",
                        span: empty_span(),
                    })
                }
            },
        );
        Ok(())
    }

    fn convert_constraint(&mut self, constraint: &Expression) -> Result<(), SolveError> {
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

    /// Converts an `intent::Expression` to an `Rc<Variable>`. Works recursively, converting
    /// sub-expressions as needed.
    fn expr_to_var(&mut self, expr: &Expression) -> Result<Rc<Variable>, SolveError> {
        match expr {
            Expression::Immediate(imm) => match imm {
                expr::Immediate::Real(val) => {
                    let new_var_name = self.new_var_name();
                    Ok(self
                        .model
                        .add_var(*val, *val, 0., &new_var_name, VarType::Continuous))
                }
                expr::Immediate::Int(val) => {
                    let new_var_name = self.new_var_name();
                    Ok(self.model.add_var(
                        *val as f64,
                        *val as f64,
                        0.,
                        &new_var_name,
                        VarType::Integer,
                    ))
                }
                expr::Immediate::Bool(val) => {
                    let new_var_name = self.new_var_name();
                    let val = if *val { 1. } else { 0. };
                    Ok(self
                        .model
                        .add_var(val, val, 0., &new_var_name, VarType::Integer))
                }
                expr::Immediate::String(_) => Err(SolveError::Internal {
                    msg: "(scip) strings are not yet currently supported",
                    span: empty_span(),
                }),
                expr::Immediate::BigInt(_) => Err(SolveError::Internal {
                    msg: "(scip) Big integers are not yet supported",
                    span: empty_span(),
                }),
            },

            Expression::Path(path) => {
                match self.model.vars().iter().find(|v| &v.name() == path) {
                    Some(var) => Ok(var.clone()),
                    None => {
                        // If not found, then it's possible that the path starts with `!` and the
                        // rest of it matches an existing variable.
                        //
                        // Paths that start with a `!` represents the inverse of the variable with the
                        // same name but without the `!`.

                        // First, find the var with the same name without the `!`, if available.
                        // Error out otherwise.
                        let var = self
                            .model
                            .vars()
                            .iter()
                            .find(|v| &("!".to_owned() + &v.name()) == path)
                            .ok_or(SolveError::Internal {
                                msg: "(scip) cannot find variable for path expression",
                                span: empty_span(),
                            })
                            .cloned()?;

                        // Then, create the inverse variable which has to be a Binary variable.
                        let bin_name = self.new_var_name();
                        let inverse = self.model.add_var(0., 1., 0., &bin_name, VarType::Binary);

                        // Now, ensure that `var + inverse == 1`.
                        let new_cons_name = self.new_cons_name();
                        self.model.add_cons(
                            vec![var.clone(), inverse.clone()],
                            &[1., 1.],
                            1.,
                            1.,
                            &new_cons_name,
                        );
                        Ok(inverse)
                    }
                }
            }

            Expression::UnaryOp { op, expr } => {
                match op {
                    expr::UnaryOp::Neg => {
                        let expr = self.expr_to_var(expr)?;
                        let new_var_name = self.new_var_name();
                        let new_cons_name = self.new_cons_name();
                        let neg_var = self.model.add_var(
                            -expr.ub(),
                            -expr.lb(),
                            0.,
                            &new_var_name,
                            expr.var_type(),
                        );
                        // neg_var + expr == 0
                        self.model.add_cons(
                            vec![neg_var.clone(), expr.clone()],
                            &[1., 1.],
                            0.,
                            0.,
                            &new_cons_name,
                        );
                        Ok(neg_var)
                    }
                    expr::UnaryOp::Not => {
                        let not_expr = invert_expression(expr)?;
                        Ok(self.expr_to_var(&not_expr)?)
                    }
                    expr::UnaryOp::NextState => Err(SolveError::Internal {
                        msg: "(scip) unsupported next state unary operator",
                        span: empty_span(),
                    }),
                    expr::UnaryOp::Error => Err(SolveError::Internal {
                        msg: "(scip) unexpected error recovery",
                        span: empty_span(),
                    }),
                }
            }

            Expression::BinaryOp {
                op,
                lhs: lhs_expr,
                rhs: rhs_expr,
            } => {
                // Add an auxilliary variable constrain it to be equal to the result of the binary
                // operation.
                let lhs = self.expr_to_var(lhs_expr)?;
                let rhs = self.expr_to_var(rhs_expr)?;
                let lhs_type = lhs.var_type();
                let rhs_type = rhs.var_type();

                let new_var_name = self.new_var_name();
                let new_var_type = match (lhs_type, rhs_type) {
                    (VarType::Integer | VarType::Binary, VarType::Integer | VarType::Binary) => {
                        VarType::Integer
                    }
                    (VarType::Continuous, VarType::Continuous) => VarType::Continuous,
                    _ => {
                        return Err(SolveError::Internal {
                            msg: "(scip) incompatible variable types encountered",
                            span: empty_span(),
                        });
                    }
                };

                let new_cons_name = self.new_cons_name();

                match op {
                    expr::BinaryOp::Add => {
                        let add_var = self.model.add_var(
                            lhs.lb() + rhs.lb(),
                            lhs.ub() + rhs.ub(),
                            0.,
                            &new_var_name,
                            new_var_type,
                        );
                        // lhs + rhs - add_var == 0
                        self.model.add_cons(
                            vec![lhs.clone(), rhs.clone(), add_var.clone()],
                            &[1., 1., -1.],
                            0.,
                            0.,
                            &new_cons_name,
                        );
                        Ok(add_var)
                    }
                    expr::BinaryOp::Sub => {
                        let sub_var = self.model.add_var(
                            lhs.lb() - rhs.ub(),
                            lhs.ub() - rhs.lb(),
                            0.,
                            &new_var_name,
                            new_var_type,
                        );
                        // lhs - rhs - sub_var == 0
                        self.model.add_cons(
                            vec![lhs.clone(), rhs.clone(), sub_var.clone()],
                            &[1., -1., -1.],
                            0.,
                            0.,
                            &new_cons_name,
                        );
                        Ok(sub_var)
                    }
                    expr::BinaryOp::Mul => {
                        let bounds = [
                            lhs.lb() * rhs.lb(),
                            lhs.lb() * rhs.ub(),
                            lhs.ub() * rhs.lb(),
                            lhs.ub() * rhs.ub(),
                        ];
                        let mult_var = self.model.add_var(
                            *bounds
                                .iter()
                                .min_by(|x, y| x.partial_cmp(y).unwrap())
                                .unwrap(),
                            *bounds
                                .iter()
                                .max_by(|x, y| x.partial_cmp(y).unwrap())
                                .unwrap(),
                            0.,
                            &new_var_name,
                            new_var_type,
                        );
                        // lhs * rhs - mult_var == 0
                        self.model.add_cons_quadratic(
                            vec![mult_var.clone()],
                            &mut [-1.],
                            vec![lhs.clone()],
                            vec![rhs.clone()],
                            &mut [1.],
                            0.,
                            0.,
                            &new_cons_name,
                        );
                        Ok(mult_var)
                    }
                    expr::BinaryOp::Div => {
                        // TODO: compute better bounds for this variable
                        let div_var = self.model.add_var(
                            -f64::INFINITY,
                            f64::INFINITY,
                            0.,
                            &new_var_name,
                            new_var_type,
                        );
                        // div_var * rhs - lhs == 0
                        self.model.add_cons_quadratic(
                            vec![lhs.clone()],
                            &mut [-1.],
                            vec![div_var.clone()],
                            vec![rhs.clone()],
                            &mut [1.],
                            0.,
                            0.,
                            &new_cons_name,
                        );
                        Ok(div_var)
                    }
                    expr::BinaryOp::Mod => {
                        if lhs_type != VarType::Continuous && rhs_type != VarType::Continuous {
                            let quotion_name = self.new_var_name();
                            let quotion = self.model.add_var(
                                -f64::INFINITY,
                                f64::INFINITY,
                                0.,
                                &quotion_name,
                                VarType::Integer,
                            );
                            let remainder_name = self.new_var_name();
                            let remainder = self.model.add_var(
                                -f64::INFINITY,
                                f64::INFINITY,
                                0.,
                                &remainder_name,
                                VarType::Integer,
                            );
                            // -lhs + remainder + quotion * rhs == 0
                            self.model.add_cons_quadratic(
                                vec![lhs.clone(), remainder.clone()],
                                &mut [-1., 1.],
                                vec![quotion.clone()],
                                vec![rhs.clone()],
                                &mut [1.],
                                0.,
                                0.,
                                &new_cons_name,
                            );
                            Ok(remainder)
                        } else {
                            // Modulo is not supported for non-integers
                            Err(SolveError::Internal {
                                msg: "(scip) modulo expressions are supported for non-integers",
                                span: empty_span(),
                            })
                        }
                    }
                    expr::BinaryOp::Equal => {
                        // `lhs == rhs` is equivalent to:
                        // ```
                        // lhs <= rhs && lhs >= rhs
                        // ```
                        // Introduce binary variables `bin1` and `bin2` such that:
                        // ```
                        // bin1 *implies* lhs <= rhs
                        // bin2 *implies* lhs >= rhs
                        // ```
                        // Then, add third binary variable `bin3` such that
                        // ```
                        // `bin1 *implies* bin1 + bin2 >= 2`, i.e., both the two conditions above
                        // must be enforced

                        // `bin1 *implies* lhs <= rhs`
                        let bin1_name = self.new_var_name();
                        let bin1 = self.model.add_var(0., 1., 0., &bin1_name, VarType::Binary);
                        let ind_cons1_name = self.new_cons_name();
                        self.model.add_cons_indicator(
                            bin1.clone(),
                            vec![lhs.clone(), rhs.clone()],
                            &mut [1., -1.],
                            0.,
                            &ind_cons1_name,
                        );

                        // `bin2 *implies* rhs <= lhs`
                        let bin2_name = self.new_var_name();
                        let bin2 = self.model.add_var(0., 1., 0., &bin2_name, VarType::Binary);
                        let ind_cons2_name = self.new_cons_name();
                        self.model.add_cons_indicator(
                            bin2.clone(),
                            vec![lhs.clone(), rhs.clone()],
                            &mut [-1., 1.],
                            0.,
                            &ind_cons2_name,
                        );

                        // `bin3 *implies* bin1 + bin2 >= 2`
                        let bin3_name = self.new_var_name();
                        let bin3 = self.model.add_var(0., 1., 0., &bin3_name, VarType::Binary);
                        let ind_cons3_name = self.new_cons_name();
                        self.model.add_cons_indicator(
                            bin3.clone(),
                            vec![bin1.clone(), bin2.clone()],
                            &mut [-1., -1.],
                            -2.,
                            &ind_cons3_name,
                        );
                        Ok(bin3)
                    }
                    expr::BinaryOp::NotEqual => {
                        if lhs_type != VarType::Continuous && rhs_type != VarType::Continuous {
                            // `lhs != rhs` is equivalent to:
                            // ```
                            // lhs <= rhs - 1 OR lhs >= rhs + 1
                            // ```
                            // Introduce binary variables `bin1` and `bin2` such that:
                            // ```
                            // bin1 *implies* lhs <= rhs - 1
                            // bin2 *implies* lhs >= rhs + 1
                            // ```
                            // Then, add third binary variable `bin3` such that
                            // ```
                            // `bin1 *implies* bin1 + bin2 >= 1`, i.e., at least one of the two
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

                            // `bin3 *implies* bin1 + bin2 >= 1`
                            let bin3_name = self.new_var_name();
                            let bin3 = self.model.add_var(0., 1., 0., &bin3_name, VarType::Binary);
                            let ind_cons3_name = self.new_cons_name();
                            self.model.add_cons_indicator(
                                bin3.clone(),
                                vec![bin1.clone(), bin2.clone()],
                                &mut [-1., -1.],
                                -1.,
                                &ind_cons3_name,
                            );
                            Ok(bin3)
                        } else {
                            Err(SolveError::Internal {
                                msg:
                                    "(scip) not equal constraint is not supported for non-integers",
                                span: empty_span(),
                            })
                        }
                    }
                    expr::BinaryOp::LessThanOrEqual => {
                        // `bin *implies* lhs -rhs <= 0`
                        let bin_name = self.new_var_name();
                        let bin = self.model.add_var(0., 1., 0., &bin_name, VarType::Binary);
                        let ind_cons_name = self.new_cons_name();
                        self.model.add_cons_indicator(
                            bin.clone(),
                            vec![lhs.clone(), rhs.clone()],
                            &mut [1., -1.],
                            0.,
                            &ind_cons_name,
                        );
                        Ok(bin)
                    }
                    expr::BinaryOp::LessThan => {
                        if lhs_type != VarType::Continuous && rhs_type != VarType::Continuous {
                            // `bin *implies* lhs -rhs <= -1`
                            let bin_name = self.new_var_name();
                            let bin = self.model.add_var(0., 1., 0., &bin_name, VarType::Binary);
                            let ind_cons_name = self.new_cons_name();
                            self.model.add_cons_indicator(
                                bin.clone(),
                                vec![lhs.clone(), rhs.clone()],
                                &mut [1., -1.],
                                -1.,
                                &ind_cons_name,
                            );
                            Ok(bin)
                        } else {
                            Err(SolveError::Internal {
                                msg: "(scip) strict inequalities not supported for non-integers",
                                span: empty_span(),
                            })
                        }
                    }
                    expr::BinaryOp::GreaterThanOrEqual => {
                        // `bin *implies* -lhs +rhs <= 0`
                        let bin_name = self.new_var_name();
                        let bin = self.model.add_var(0., 1., 0., &bin_name, VarType::Binary);
                        let ind_cons_name = self.new_cons_name();
                        self.model.add_cons_indicator(
                            bin.clone(),
                            vec![lhs.clone(), rhs.clone()],
                            &mut [-1., 1.],
                            0.,
                            &ind_cons_name,
                        );
                        Ok(bin)
                    }
                    expr::BinaryOp::GreaterThan => {
                        if lhs_type != VarType::Continuous && rhs_type != VarType::Continuous {
                            // `bin *implies* -lhs +rhs <= -1`
                            let bin_name = self.new_var_name();
                            let bin = self.model.add_var(0., 1., 0., &bin_name, VarType::Binary);
                            let ind_cons_name = self.new_cons_name();
                            self.model.add_cons_indicator(
                                bin.clone(),
                                vec![lhs.clone(), rhs.clone()],
                                &mut [-1., 1.],
                                -1.,
                                &ind_cons_name,
                            );
                            Ok(bin)
                        } else {
                            Err(SolveError::Internal {
                                msg: "(scip) strict inequalities not supported for non-integers",
                                span: empty_span(),
                            })
                        }
                    }
                    expr::BinaryOp::LogicalAnd => {
                        // `bin *implies* -lhs - rhs <= -2`.
                        // That is, both `lhs` and `rhs` must be 1.
                        let bin_name = self.new_var_name();
                        let bin = self.model.add_var(0., 1., 0., &bin_name, VarType::Binary);
                        let ind_cons_name = self.new_cons_name();
                        self.model.add_cons_indicator(
                            bin.clone(),
                            vec![lhs.clone(), rhs.clone()],
                            &mut [-1., -1.],
                            -2.,
                            &ind_cons_name,
                        );
                        Ok(bin)
                    }
                    expr::BinaryOp::LogicalOr => {
                        // `bin *implies* -lhs - rhs <= -1`.
                        // That is, at least one of `lhs` and `rhs` must be 1.
                        let bin_name = self.new_var_name();
                        let bin = self.model.add_var(0., 1., 0., &bin_name, VarType::Binary);
                        let ind_cons_name = self.new_cons_name();
                        self.model.add_cons_indicator(
                            bin.clone(),
                            vec![lhs.clone(), rhs.clone()],
                            &mut [-1., -1.],
                            -1.,
                            &ind_cons_name,
                        );
                        Ok(bin)
                    }
                }
            }

            Expression::Call { .. } | Expression::If { .. } => Err(SolveError::Internal {
                msg: "(scip) calls and if expressions are not yet supported",
                span: empty_span(),
            }),
        }
    }
}

/// Invert a Boolean expression. Takes an `Expression` and returns another `Expression` that
/// represents its inverse. For example, a `>=` binary op expression becomes a `<` binary op
/// expression.
fn invert_expression(expr: &Expression) -> Result<Expression, SolveError> {
    match expr {
        Expression::Immediate(imm) => match imm {
            expr::Immediate::Bool(val) => Ok(Expression::Immediate(expr::Immediate::Bool(!val))),
            _ => Err(SolveError::Internal {
                msg: "(scip) attempting to invert a non-Boolean immediate value",
                span: empty_span(),
            }),
        },

        Expression::Path(path) => {
            // The inverse of a path expression is another path expression with the same name but
            // with a `!` added as a prefix. This guarantees uniquness of the name.
            //
            // Later, path expressions that start with `!` will be handled by creating a new binary
            // decision variable and constraining it to be the inverse of the variable with the
            // same name but without the `!` prefix.
            Ok(Expression::Path("!".to_owned() + path))
        }

        Expression::UnaryOp { op, expr } => match op {
            expr::UnaryOp::Not => Ok(*expr.clone()),
            _ => Err(SolveError::Internal {
                msg: "(scip) attempting to invert a non-Boolean expression",
                span: empty_span(),
            }),
        },

        Expression::BinaryOp { op, lhs, rhs } => match op {
            expr::BinaryOp::GreaterThanOrEqual => Ok(Expression::BinaryOp {
                op: expr::BinaryOp::LessThan,
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            }),
            expr::BinaryOp::GreaterThan => Ok(Expression::BinaryOp {
                op: expr::BinaryOp::LessThanOrEqual,
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            }),
            expr::BinaryOp::LessThanOrEqual => Ok(Expression::BinaryOp {
                op: expr::BinaryOp::GreaterThan,
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            }),
            expr::BinaryOp::LessThan => Ok(Expression::BinaryOp {
                op: expr::BinaryOp::GreaterThanOrEqual,
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            }),
            expr::BinaryOp::Equal => Ok(Expression::BinaryOp {
                op: expr::BinaryOp::NotEqual,
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            }),
            expr::BinaryOp::NotEqual => Ok(Expression::BinaryOp {
                op: expr::BinaryOp::Equal,
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            }),
            expr::BinaryOp::LogicalAnd => Ok(Expression::BinaryOp {
                op: expr::BinaryOp::LogicalOr,
                lhs: Box::new(invert_expression(lhs)?),
                rhs: Box::new(invert_expression(rhs)?),
            }),
            expr::BinaryOp::LogicalOr => Ok(Expression::BinaryOp {
                op: expr::BinaryOp::LogicalAnd,
                lhs: Box::new(invert_expression(lhs)?),
                rhs: Box::new(invert_expression(rhs)?),
            }),
            _ => Err(SolveError::Internal {
                msg: "(scip) attempting to invert a non-Boolean expression",
                span: empty_span(),
            }),
        },

        Expression::Call { .. } | Expression::If { .. } => Err(SolveError::Internal {
            msg: "(scip) calls and if expressions are not yet supported",
            span: empty_span(),
        }),
    }
}

impl<'a> Solver<'a, Solved> {
    /// Pretty print the output of the solver which includes a valid solution for all the variables
    /// in case of success. Skips any helper variables introduced by the solver.
    pub fn print_solution(&self) {
        match self.model.status() {
            Status::Optimal => {
                println!(
                    "   {}",
                    Style::new(Color::Green)
                        .bold()
                        .paint("Problem is satisfiable")
                );
                println!("    {}", Style::new(Color::Green).bold().paint("Solution:"));
                let sol = self.model.best_sol().unwrap();

                self.model
                    .vars()
                    .iter()
                    .take(self.intent.vars.len())
                    .fold((), |(), var| {
                        println!("     {}: {}", &var.name(), sol.val(var.clone()));
                    });

                if !matches!(self.intent.directive, SolveDirective::Satisfy) {
                    println!(
                        "    {}: {}",
                        Style::new(Color::Green).bold().paint("Objective value:"),
                        self.model.obj_val()
                    );
                }
            }
            Status::Infeasible => println!(
                "   {}",
                Style::new(Color::Red).bold().paint("Problem is infeasible")
            ),
            Status::Unbounded => println!(
                "   {}",
                Style::new(Color::Red).bold().paint("Problem is unbounded")
            ),
            Status::Inforunbd => println!(
                "   {}",
                Style::new(Color::Red)
                    .bold()
                    .paint("Problem is infeasible or unbounded")
            ),
            _ => println!(
                "   {} {:?}",
                Style::new(Color::Red).bold().paint("SCIP status:"),
                Style::new(Color::Red).bold().paint(self.model.status())
            ),
        }
    }

    /// Serialize the raw output of the solver into a `String`. This is mostly meant for testing
    /// purposes. Includes helper variables introduced by the solver.
    pub fn display_solution_raw(&self) -> String {
        match self.model.status() {
            Status::Optimal => {
                let sol = self.model.best_sol().unwrap();
                let solution = self.model.vars().iter().take(self.intent.vars.len()).fold(
                    String::new(),
                    |mut acc, var| {
                        writeln!(&mut acc, "{}: {}", &var.name(), sol.val(var.clone()))
                            .expect("Failed to write solution to string");
                        acc
                    },
                );
                if matches!(self.intent.directive, SolveDirective::Satisfy) {
                    solution
                } else {
                    format!("{solution}objective: {}", self.model.obj_val())
                }
            }
            Status::Infeasible => "infeasible".to_string(),
            Status::Unbounded => "unbounded".to_string(),
            Status::Inforunbd => "infeasible or unbounded".to_string(),
            _ => format!("scip_status({:?})", self.model.status()),
        }
    }
}
