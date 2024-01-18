use crate::{error::SolveError, expr, intent::Expression, span::empty_span};

/// Invert a Boolean expression. Takes an `Expression` and returns another `Expression` that
/// represents its inverse. For example, a `>=` binary op expression becomes a `<` binary op
/// expression.
pub(super) fn invert_expression(expr: &Expression) -> Result<Expression, SolveError> {
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
