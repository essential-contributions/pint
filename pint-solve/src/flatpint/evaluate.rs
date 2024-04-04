use crate::flatpint::{BinaryOp, Expr, Immediate, UnaryOp};
use std::collections::HashMap;

/// Given an `Expr` and a solution, plug in the solution into the `Expr` and evaluate it to an
/// `Immediate`.
#[allow(unused)]
pub(crate) fn evaluate_expr(expr: &Expr, solution: &HashMap<String, Immediate>) -> Immediate {
    /// Compares two floats using a threshold
    fn approx_equal(a: f64, b: f64) -> bool {
        (a - b).abs() < 1e-6 // This threshold is arbitrarily selected for now
    }

    use BinaryOp::*;
    use UnaryOp::*;
    match expr {
        Expr::Immediate(imm) => imm.clone(),
        Expr::Path(name) => solution
            .get(name)
            .cloned()
            .expect("every name must have a value in a solution"),
        Expr::UnaryOp { op, expr } => {
            let expr = evaluate_expr(expr, solution);
            match (expr, op) {
                (Immediate::Bool(val), Not) => Immediate::Bool(!val),
                (Immediate::Int(val), Neg) => Immediate::Int(-val),
                (Immediate::Int(0), Not) => Immediate::Bool(true),
                (Immediate::Int(1), Not) => Immediate::Bool(false),
                (Immediate::Real(val), Neg) => Immediate::Real(-val),
                _ => panic!("type error: invalid unary op for expression"),
            }
        }
        Expr::BinaryOp { op, lhs, rhs } => {
            let lhs = evaluate_expr(lhs, solution);
            let rhs = evaluate_expr(rhs, solution);
            let handle_bool_ops = |op: &BinaryOp, lhs: bool, rhs: bool| {
                match op {
                    // Comparison
                    Equal => Immediate::Bool(lhs == rhs),
                    NotEqual => Immediate::Bool(lhs != rhs),
                    LessThan => Immediate::Bool(!lhs && rhs),
                    LessThanOrEqual => Immediate::Bool(lhs <= rhs),
                    GreaterThan => Immediate::Bool(lhs && !rhs),
                    GreaterThanOrEqual => Immediate::Bool(lhs >= rhs),

                    // Logical
                    LogicalAnd => Immediate::Bool(lhs && rhs),
                    LogicalOr => Immediate::Bool(lhs || rhs),

                    _ => panic!("type error: invalid binary op for bools"),
                }
            };
            match (lhs, rhs) {
                (Immediate::Bool(lhs), Immediate::Bool(rhs)) => handle_bool_ops(op, lhs, rhs),
                (Immediate::Int(lhs), Immediate::Bool(rhs)) if lhs == 0 || lhs == 1 => {
                    handle_bool_ops(op, lhs != 0, rhs)
                }
                (Immediate::Bool(lhs), Immediate::Int(rhs)) if rhs == 0 || rhs == 1 => {
                    handle_bool_ops(op, lhs, rhs != 0)
                }
                (Immediate::Int(lhs), Immediate::Int(rhs)) => match op {
                    // Arithmetic
                    Add => Immediate::Int(lhs + rhs),
                    Sub => Immediate::Int(lhs - rhs),
                    Mul => Immediate::Int(lhs * rhs),
                    Div => Immediate::Int(lhs / rhs),
                    Mod => Immediate::Int(lhs % rhs),

                    // Comparison
                    Equal => Immediate::Bool(lhs == rhs),
                    NotEqual => Immediate::Bool(lhs != rhs),
                    LessThan => Immediate::Bool(lhs < rhs),
                    LessThanOrEqual => Immediate::Bool(lhs <= rhs),
                    GreaterThan => Immediate::Bool(lhs > rhs),
                    GreaterThanOrEqual => Immediate::Bool(lhs >= rhs),

                    // Logical - only if both `lhs` and `rhs` are 0 or 1.
                    LogicalAnd if (lhs == 0 || lhs == 1) && (rhs == 0 || rhs == 1) => {
                        Immediate::Bool(lhs != 0 && rhs != 0)
                    }
                    LogicalOr if (lhs == 0 || lhs == 1) && (rhs == 0 || rhs == 1) => {
                        Immediate::Bool(lhs != 0 || rhs != 0)
                    }

                    _ => panic!("type error: invalid binary op for ints"),
                },
                (Immediate::Real(lhs), Immediate::Real(rhs)) => match op {
                    // Arithmetic
                    Add => Immediate::Real(lhs + rhs),
                    Sub => Immediate::Real(lhs - rhs),
                    Mul => Immediate::Real(lhs * rhs),
                    Div => Immediate::Real(lhs / rhs),

                    // Comparison
                    Equal => Immediate::Bool(approx_equal(lhs, rhs)),
                    LessThanOrEqual => Immediate::Bool(lhs <= rhs || approx_equal(lhs, rhs)),
                    GreaterThanOrEqual => Immediate::Bool(lhs >= rhs || approx_equal(lhs, rhs)),

                    _ => panic!("type error: invalid binary op for reals"),
                },
                _ => panic!("type error: types of lhs and rhs don't match"),
            }
        }
    }
}
