use crate::{
    error::SolveError,
    flatpint::{BinaryOp, Expr, Immediate, UnaryOp},
};
use russcip::ProblemCreated;

/// Invert a Boolean expression. Takes an `ExprKey` and returns another `ExprKey` that
/// represents its inverse. For example, a `>=` binary op expression becomes a `<` binary op
/// expression.
impl<'a> super::Solver<'a, ProblemCreated> {
    pub(super) fn invert_expression(expr: &Expr) -> Result<Expr, SolveError> {
        match expr {
            Expr::Immediate(value) => match value {
                Immediate::Bool(val) => Ok(Expr::Immediate(Immediate::Bool(!val))),
                Immediate::Int(val) if *val == 1 => Ok(Expr::Immediate(Immediate::Int(0))),
                Immediate::Int(val) if *val == 0 => Ok(Expr::Immediate(Immediate::Int(1))),
                _ => Err(SolveError::Internal {
                    msg: "(scip) attempting to invert a non-Boolean immediate value",
                }),
            },
            Expr::Path(path) => {
                // The inverse of a path expression is another path expression with the same name but
                // with a `!` added as a prefix. This guarantees uniquness of the name.
                //
                // Later, path expressions that start with `!` will be handled by creating a new binary
                // decision variable and constraining it to be the inverse of the variable with the
                // same name but without the `!` prefix.
                Ok(Expr::Path("!".to_owned() + &path))
            }

            Expr::UnaryOp { op, expr } => match op {
                UnaryOp::Not => Ok(*expr.clone()),
                UnaryOp::Neg => Err(SolveError::Internal {
                    msg: "(scip) attempting to invert a non-Boolean expression",
                }),
            },

            Expr::BinaryOp { op, lhs, rhs } => match op {
                BinaryOp::GreaterThanOrEqual => Ok(Expr::BinaryOp {
                    op: BinaryOp::LessThan,
                    lhs: lhs.clone(),
                    rhs: rhs.clone(),
                }),
                BinaryOp::GreaterThan => Ok(Expr::BinaryOp {
                    op: BinaryOp::LessThanOrEqual,
                    lhs: lhs.clone(),
                    rhs: rhs.clone(),
                }),
                BinaryOp::LessThanOrEqual => Ok(Expr::BinaryOp {
                    op: BinaryOp::GreaterThan,
                    lhs: lhs.clone(),
                    rhs: rhs.clone(),
                }),
                BinaryOp::LessThan => Ok(Expr::BinaryOp {
                    op: BinaryOp::GreaterThanOrEqual,
                    lhs: lhs.clone(),
                    rhs: rhs.clone(),
                }),
                BinaryOp::Equal => Ok(Expr::BinaryOp {
                    op: BinaryOp::NotEqual,
                    lhs: lhs.clone(),
                    rhs: rhs.clone(),
                }),
                BinaryOp::NotEqual => Ok(Expr::BinaryOp {
                    op: BinaryOp::Equal,
                    lhs: lhs.clone(),
                    rhs: rhs.clone(),
                }),
                BinaryOp::LogicalAnd => Ok(Expr::BinaryOp {
                    op: BinaryOp::LogicalOr,
                    lhs: Box::new(Self::invert_expression(lhs)?),
                    rhs: Box::new(Self::invert_expression(rhs)?),
                }),
                BinaryOp::LogicalOr => Ok(Expr::BinaryOp {
                    op: BinaryOp::LogicalAnd,
                    lhs: Box::new(Self::invert_expression(lhs)?),
                    rhs: Box::new(Self::invert_expression(rhs)?),
                }),
                _ => Err(SolveError::Internal {
                    msg: "(scip) attempting to invert a non-Boolean expression",
                }),
            },
        }
    }
}
