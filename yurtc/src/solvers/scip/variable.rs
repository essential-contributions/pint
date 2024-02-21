use crate::{
    error::SolveError,
    expr,
    intermediate::{ExprKey, SolveFunc, VarKey},
    span::empty_span,
    types::{PrimitiveKind, Type},
};
use russcip::{prelude::*, ProblemCreated, Variable};
use std::rc::Rc;

impl<'a> super::Solver<'a, ProblemCreated> {
    pub(super) fn convert_variable(&mut self, variable: &VarKey) -> Result<(), SolveError> {
        let ty = &self.intent.var_types[*variable];
        let name = &self.intent.vars[*variable].name;
        self.model.add_var(
            /* Bounds */
            if ty.is_bool() { 0. } else { -f64::INFINITY }, // TODO: We can do better with a proper static analysis
            if ty.is_bool() { 1. } else { f64::INFINITY }, // TODO: We can do better with a proper static analysis
            /* Objective coefficient */
            //
            // Assume that the objective function is previously converted into a single path. Now,
            // Set the coefficient to 1 for this variable if it matches that path.
            match &self.intent.directives[0].0 {
                SolveFunc::Satisfy => 0.,
                SolveFunc::Minimize(expr) | SolveFunc::Maximize(expr) => {
                    if match &self.intent.exprs[*expr] {
                        expr::Expr::PathByName(path, _) => path,
                        expr::Expr::PathByKey(var_key, _) => &self.intent.vars[*var_key].name,
                        _ => {
                            return Err(SolveError::Internal {
                                msg: "(scip) objective function must have been converted to a path",
                                span: empty_span(),
                            })
                        }
                    } == name
                    {
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
                Type::Primitive {
                    kind: PrimitiveKind::Bool,
                    ..
                } => VarType::Binary,
                Type::Primitive {
                    kind: PrimitiveKind::Int,
                    ..
                } => VarType::Integer,
                Type::Primitive {
                    kind: PrimitiveKind::Real,
                    ..
                } => VarType::Continuous,
                Type::Primitive { .. }
                | Type::Error(_)
                | Type::Array { .. }
                | Type::Tuple { .. }
                | Type::Custom { .. } => {
                    return Err(SolveError::Internal {
                        msg: "(scip) other types are not supported",
                        span: empty_span(),
                    })
                }
            },
        );
        Ok(())
    }

    /// Converts an `intent::Expression` to an `Rc<Variable>`. Works recursively, converting
    /// sub-expressions as needed.
    pub(super) fn expr_to_var(&mut self, expr: &ExprKey) -> Result<Rc<Variable>, SolveError> {
        match self.intent.exprs[*expr].clone() {
            expr::Expr::Immediate { value, .. } => match value {
                expr::Immediate::Real(val) => {
                    let new_var_name = self.new_var_name();
                    Ok(self
                        .model
                        .add_var(val, val, 0., &new_var_name, VarType::Continuous))
                }
                expr::Immediate::Int(val) => {
                    let new_var_name = self.new_var_name();
                    Ok(self.model.add_var(
                        val as f64,
                        val as f64,
                        0.,
                        &new_var_name,
                        VarType::Integer,
                    ))
                }
                expr::Immediate::Bool(val) => {
                    let new_var_name = self.new_var_name();
                    let val = if val { 1. } else { 0. };
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

            expr::Expr::PathByName(path, _) => {
                if let Some(var) = self.model.vars().iter().find(|v| v.name() == path) {
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
                        .find(|v| "!".to_owned() + &v.name() == path)
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

            expr::Expr::PathByKey(var_key, _) => {
                let path = &self.intent.vars[var_key].name;
                if let Some(var) = self.model.vars().iter().find(|v| &v.name() == path) {
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

            expr::Expr::UnaryOp { op, expr, .. } => {
                match op {
                    expr::UnaryOp::Neg => {
                        let expr = self.expr_to_var(&expr)?;
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
                        let not_expr = self.invert_expression(&expr)?;
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

            expr::Expr::BinaryOp {
                op,
                lhs: lhs_expr,
                rhs: rhs_expr,
                ..
            } => {
                // Add an auxilliary variable constrain it to be equal to the result of the binary
                // operation.
                let lhs = self.expr_to_var(&lhs_expr)?;
                let rhs = self.expr_to_var(&rhs_expr)?;
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

            expr::Expr::If { .. }
            | expr::Expr::FnCall { .. }
            | expr::Expr::Error(_)
            | expr::Expr::MacroCall { .. }
            | expr::Expr::Array { .. }
            | expr::Expr::ArrayElementAccess { .. }
            | expr::Expr::Tuple { .. }
            | expr::Expr::TupleFieldAccess { .. }
            | expr::Expr::Cast { .. }
            | expr::Expr::In { .. }
            | expr::Expr::Range { .. }
            | expr::Expr::ForAll { .. } => Err(SolveError::Internal {
                msg: "(scip) all other exprs are not supported",
                span: empty_span(),
            }),
        }
    }
}
