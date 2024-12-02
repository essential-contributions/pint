use crate::{
    error::SolveError,
    flatpint::{BinaryOp, Expr, Immediate, Solve, Type, UnaryOp, Var},
};
use russcip::{prelude::*, ProblemCreated, Variable};
use std::rc::Rc;

// These are the default bounds chosen for all variable, except for bools. We can do better with
// some data flow analysis.
const DEFAULT_LB: f64 = -f64::INFINITY;
const DEFAULT_UB: f64 = f64::INFINITY;

impl super::Solver<'_, ProblemCreated> {
    pub(super) fn convert_variable(&mut self, var: &Var) {
        let ty = &var.ty;
        let name = &var.name;

        self.model.add_var(
            /* Bounds */
            if ty == &Type::Bool { 0. } else { DEFAULT_LB },
            if ty == &Type::Bool { 1. } else { DEFAULT_UB },
            /* Objective coefficient */
            // Set the coefficient to 1 for this variable if it matches that name indicated in the
            // solve directive
            match &self.flatpint.solve {
                Solve::Satisfy => 0.,
                Solve::Minimize(name) | Solve::Maximize(name) => {
                    if var.name == *name {
                        1.
                    } else {
                        0.
                    }
                }
            },
            /* Variable name */
            &name.clone(),
            /* Variable type */
            match ty {
                Type::Bool => VarType::Binary,
                Type::Int => VarType::Integer,
                Type::Real => VarType::Continuous,
            },
        );
    }

    /// Converts an `flatpint::Expr` to an `Rc<Variable>`. Works recursively, converting
    /// sub-expressions as needed.
    pub(super) fn expr_to_var(&mut self, expr: &Expr) -> Result<Rc<Variable>, SolveError> {
        match expr {
            Expr::Immediate(value) => match value {
                Immediate::Real(val) => {
                    let new_var_name = self.new_var_name();
                    Ok(self
                        .model
                        .add_var(*val, *val, 0., &new_var_name, VarType::Continuous))
                }
                Immediate::Int(val) => {
                    let new_var_name = self.new_var_name();
                    Ok(self.model.add_var(
                        *val as f64,
                        *val as f64,
                        0.,
                        &new_var_name,
                        VarType::Integer,
                    ))
                }
                Immediate::Bool(val) => {
                    let new_var_name = self.new_var_name();
                    let val = if *val { 1. } else { 0. };
                    Ok(self
                        .model
                        .add_var(val, val, 0., &new_var_name, VarType::Integer))
                }
            },

            Expr::Path(path) => {
                if let Some(var) = self.model.vars().iter().find(|v| v.name() == *path) {
                    Ok(var.clone())
                } else {
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
                        .find(|v| "!".to_owned() + &v.name() == *path)
                        .ok_or(SolveError::Internal {
                            msg: "(scip) cannot find variable for path expression",
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

            Expr::UnaryOp { op, expr } => {
                match op {
                    UnaryOp::Neg => {
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
                    UnaryOp::Not => {
                        let not_expr = Self::invert_expression(expr)?;
                        Ok(self.expr_to_var(&not_expr)?)
                    }
                }
            }

            Expr::BinaryOp {
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
                        });
                    }
                };

                let new_cons_name = self.new_cons_name();

                match op {
                    BinaryOp::Add => {
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
                    BinaryOp::Sub => {
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
                    BinaryOp::Mul => {
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
                    BinaryOp::Div => {
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
                    BinaryOp::Mod => {
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
                            })
                        }
                    }
                    BinaryOp::Equal => {
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
                    BinaryOp::NotEqual => {
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
                            })
                        }
                    }
                    BinaryOp::LessThanOrEqual => {
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
                    BinaryOp::LessThan => {
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
                            })
                        }
                    }
                    BinaryOp::GreaterThanOrEqual => {
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
                    BinaryOp::GreaterThan => {
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
                            })
                        }
                    }
                    BinaryOp::LogicalAnd => {
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
                    BinaryOp::LogicalOr => {
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
        }
    }
}
