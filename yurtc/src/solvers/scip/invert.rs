use crate::{error::SolveError, expr, intermediate::ExprKey, span::empty_span};
use russcip::ProblemCreated;

/// Invert a Boolean expression. Takes an `ExprKey` and returns another `ExprKey` that
/// represents its inverse. For example, a `>=` binary op expression becomes a `<` binary op
/// expression.
impl<'a> super::Solver<'a, ProblemCreated> {
    pub(super) fn invert_expression(&mut self, expr: &ExprKey) -> Result<ExprKey, SolveError> {
        let expr_inv = match self.intent.exprs[*expr].clone() {
            expr::Expr::Immediate { value, span } => match value {
                expr::Immediate::Bool(val) => expr::Expr::Immediate {
                    value: expr::Immediate::Bool(!val),
                    span: span.clone(),
                },
                _ => {
                    return Err(SolveError::Internal {
                        msg: "(scip) attempting to invert a non-Boolean immediate value",
                        span: empty_span(),
                    })
                }
            },

            expr::Expr::PathByName(path, span) => {
                // The inverse of a path expression is another path expression with the same name but
                // with a `!` added as a prefix. This guarantees uniquness of the name.
                //
                // Later, path expressions that start with `!` will be handled by creating a new binary
                // decision variable and constraining it to be the inverse of the variable with the
                // same name but without the `!` prefix.
                expr::Expr::PathByName("!".to_owned() + &path, span.clone())
            }
            expr::Expr::PathByKey(var_key, span) => {
                // The inverse of a path expression is another path expression with the same name but
                // with a `!` added as a prefix. This guarantees uniquness of the name.
                //
                // Later, path expressions that start with `!` will be handled by creating a new binary
                // decision variable and constraining it to be the inverse of the variable with the
                // same name but without the `!` prefix.
                let path = &self.intent.vars[var_key].name;
                expr::Expr::PathByName("!".to_owned() + &path, span.clone())
            }
            expr::Expr::UnaryOp { op, expr, .. } => match op {
                expr::UnaryOp::Not => self.intent.exprs[expr].clone(),
                _ => {
                    return Err(SolveError::Internal {
                        msg: "(scip) attempting to invert a non-Boolean expression",
                        span: empty_span(),
                    })
                }
            },

            expr::Expr::BinaryOp { op, lhs, rhs, span } => match op {
                expr::BinaryOp::GreaterThanOrEqual => expr::Expr::BinaryOp {
                    op: expr::BinaryOp::LessThan,
                    lhs,
                    rhs,
                    span: span.clone(),
                },
                expr::BinaryOp::GreaterThan => expr::Expr::BinaryOp {
                    op: expr::BinaryOp::LessThanOrEqual,
                    lhs,
                    rhs,
                    span: span.clone(),
                },
                expr::BinaryOp::LessThanOrEqual => expr::Expr::BinaryOp {
                    op: expr::BinaryOp::GreaterThan,
                    lhs,
                    rhs,
                    span: span.clone(),
                },
                expr::BinaryOp::LessThan => expr::Expr::BinaryOp {
                    op: expr::BinaryOp::GreaterThanOrEqual,
                    lhs,
                    rhs,
                    span: span.clone(),
                },
                expr::BinaryOp::Equal => expr::Expr::BinaryOp {
                    op: expr::BinaryOp::NotEqual,
                    lhs,
                    rhs,
                    span: span.clone(),
                },
                expr::BinaryOp::NotEqual => expr::Expr::BinaryOp {
                    op: expr::BinaryOp::Equal,
                    lhs,
                    rhs,
                    span: span.clone(),
                },
                expr::BinaryOp::LogicalAnd => expr::Expr::BinaryOp {
                    op: expr::BinaryOp::LogicalOr,
                    lhs: self.invert_expression(&lhs)?,
                    rhs: self.invert_expression(&rhs)?,
                    span: span.clone(),
                },
                expr::BinaryOp::LogicalOr => expr::Expr::BinaryOp {
                    op: expr::BinaryOp::LogicalAnd,
                    lhs: self.invert_expression(&lhs)?,
                    rhs: self.invert_expression(&rhs)?,
                    span: span.clone(),
                },
                _ => {
                    return Err(SolveError::Internal {
                        msg: "(scip) attempting to invert a non-Boolean expression",
                        span: empty_span(),
                    })
                }
            },

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
            | expr::Expr::ForAll { .. } => {
                return Err(SolveError::Internal {
                    msg: "(scip) unable to invert expression",
                    span: empty_span(),
                })
            }
        };
        let expr_inv_key = self.intent.exprs.insert(expr_inv);

        // The type of the new expression is exactly the same as the old expr.
        self.intent
            .expr_types
            .insert(expr_inv_key, self.intent.expr_types[*expr].clone());
        Ok(expr_inv_key)
    }
}
