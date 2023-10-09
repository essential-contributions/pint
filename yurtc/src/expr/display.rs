use std::fmt::{Display, Formatter, Result};

use crate::{
    expr,
    util::{write_many, write_many_iter},
};

impl<Path: Display, BlockExpr: Display> Display for super::Expr<Path, BlockExpr> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            super::Expr::Immediate { value, .. } => write!(f, "{value}"),

            super::Expr::Path(p) => write!(f, "{p}"),

            super::Expr::UnaryOp { op, expr, .. } => match op {
                expr::UnaryOp::Pos => write!(f, "+{expr}"),
                expr::UnaryOp::Neg => write!(f, "-{expr}"),
                expr::UnaryOp::Not => write!(f, "!{expr}"),
                expr::UnaryOp::NextState => write!(f, "{expr}'"),
            },

            super::Expr::BinaryOp { op, lhs, rhs, .. } => {
                write!(f, "({lhs} ")?;
                match op {
                    expr::BinaryOp::Mul => write!(f, "*"),
                    expr::BinaryOp::Div => write!(f, "/"),
                    expr::BinaryOp::Add => write!(f, "+"),
                    expr::BinaryOp::Sub => write!(f, "-"),
                    expr::BinaryOp::Mod => write!(f, "%"),
                    expr::BinaryOp::LessThan => write!(f, "<"),
                    expr::BinaryOp::LessThanOrEqual => write!(f, "<="),
                    expr::BinaryOp::GreaterThan => write!(f, ">"),
                    expr::BinaryOp::GreaterThanOrEqual => write!(f, ">="),
                    expr::BinaryOp::Equal => write!(f, "=="),
                    expr::BinaryOp::NotEqual => write!(f, "!="),
                    expr::BinaryOp::LogicalAnd => write!(f, "&&"),
                    expr::BinaryOp::LogicalOr => write!(f, "||"),
                }?;
                write!(f, " {rhs})")
            }

            super::Expr::Call { name, args, .. } => {
                write!(f, "{name}(")?;
                write_many!(f, args, ", ");
                write!(f, ")")
            }

            super::Expr::Block(b) => write!(f, "{b}"),

            super::Expr::If {
                condition,
                then_block,
                else_block,
                ..
            } => write!(f, "if {condition} {then_block} else {else_block}"),

            super::Expr::Cond {
                branches,
                else_result,
                ..
            } => {
                write!(f, "cond {{ ")?;
                for branch in branches {
                    write!(f, "{branch}, ")?;
                }
                write!(f, "else => {else_result} }}")
            }

            super::Expr::Array { elements, .. } => {
                write!(f, "[")?;
                write_many!(f, elements, ", ");
                write!(f, "]")
            }

            super::Expr::ArrayElementAccess { array, index, .. } => {
                write!(f, "{array}[{index}]")
            }

            super::Expr::Tuple { fields, .. } => {
                write!(f, "{{")?;
                let mut i = fields.iter().map(|(name, val)| {
                    // This is the only place where we're building strings.  Could be aleviated
                    // if the named tuple field was a struct which could be Display.
                    format!(
                        "{}{val}",
                        name.as_ref()
                            .map(|name| format!("{}: ", name.name))
                            .unwrap_or(String::new())
                    )
                });
                write_many_iter!(f, i, ", ");
                write!(f, "}}")
            }

            super::Expr::TupleFieldAccess { tuple, field, .. } => {
                write!(f, "{tuple}.")?;
                match field {
                    expr::TupleAccess::Index(n) => write!(f, "{n}"),
                    expr::TupleAccess::Name(i) => write!(f, "{}", i.name),
                }
            }

            super::Expr::Cast { value, ty, .. } => write!(f, "{value} as {ty}"),

            super::Expr::In {
                value, collection, ..
            } => write!(f, "{value} in {collection}"),

            super::Expr::Range { lb, ub, .. } => write!(f, "{lb}..{ub}"),
        }
    }
}

impl<Expr: Display> Display for super::CondBranch<Expr> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{} => {}", self.condition, self.result)
    }
}

impl Display for super::Immediate {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            super::Immediate::Real(n) => write!(f, "{n:e}"),
            super::Immediate::Int(n) => write!(f, "{n}"),
            super::Immediate::BigInt(n) => write!(f, "{n}"),
            super::Immediate::Bool(b) => write!(f, "{b}"),
            super::Immediate::String(s) => write!(f, "{s:?}"),
        }
    }
}
