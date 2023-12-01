use std::fmt::{Display, Formatter, Result};

use crate::{
    expr,
    intent::intermediate::{DisplayWithII, IntermediateIntent},
    util::{write_many_iter, write_many_with_ii},
};

impl DisplayWithII for super::ExprKey {
    fn fmt(&self, f: &mut Formatter, ii: &IntermediateIntent) -> Result {
        write!(f, "{}", ii.with_ii(&ii.exprs[*self]))
    }
}

impl DisplayWithII for &super::Expr {
    fn fmt(&self, f: &mut Formatter, ii: &IntermediateIntent) -> Result {
        match self {
            super::Expr::Error(..) => write!(f, "Error"),
            super::Expr::Immediate { value, .. } => write!(f, "{value}"),

            super::Expr::PathByName(p, _) => write!(f, "{p}"),
            super::Expr::PathByKey(k, _) => write!(f, "{}", ii.with_ii(k)),

            super::Expr::UnaryOp { op, expr, .. } => {
                if matches!(op, expr::UnaryOp::NextState) {
                    write!(f, "{}'", ii.with_ii(expr))
                } else {
                    match op {
                        expr::UnaryOp::Pos => write!(f, "+"),
                        expr::UnaryOp::Neg => write!(f, "-"),
                        expr::UnaryOp::Not => write!(f, "!"),
                        expr::UnaryOp::NextState => unreachable!(),
                    }?;
                    write!(f, "{}", ii.with_ii(expr))
                }
            }

            super::Expr::BinaryOp { op, lhs, rhs, .. } => {
                write!(f, "({} ", ii.with_ii(lhs))?;
                match op {
                    expr::BinaryOp::Add => write!(f, "+"),
                    expr::BinaryOp::Div => write!(f, "/"),
                    expr::BinaryOp::Equal => write!(f, "=="),
                    expr::BinaryOp::GreaterThanOrEqual => write!(f, ">="),
                    expr::BinaryOp::GreaterThan => write!(f, ">"),
                    expr::BinaryOp::LessThanOrEqual => write!(f, "<="),
                    expr::BinaryOp::LessThan => write!(f, "<"),
                    expr::BinaryOp::LogicalAnd => write!(f, "&&"),
                    expr::BinaryOp::LogicalOr => write!(f, "||"),
                    expr::BinaryOp::Mod => write!(f, "%"),
                    expr::BinaryOp::Mul => write!(f, "*"),
                    expr::BinaryOp::NotEqual => write!(f, "!="),
                    expr::BinaryOp::Sub => write!(f, "-"),
                }?;
                write!(f, " {})", ii.with_ii(rhs))
            }

            super::Expr::In {
                value, collection, ..
            } => write!(f, "{} in {}", ii.with_ii(value), ii.with_ii(collection)),

            super::Expr::Cast { value, ty, .. } => {
                write!(f, "{}", ii.with_ii(value))?;
                write!(f, " as ")?;
                write!(f, "{}", ii.with_ii(*ty.clone()))
            }

            super::Expr::TupleFieldAccess { tuple, field, .. } => {
                write!(f, "{}.", ii.with_ii(tuple))?;
                match field {
                    expr::TupleAccess::Error => write!(f, "Error"),
                    expr::TupleAccess::Index(n) => write!(f, "{n}"),
                    expr::TupleAccess::Name(i) => write!(f, "{}", i.name),
                }
            }

            super::Expr::ArrayElementAccess { array, index, .. } => {
                write!(f, "{}[{}]", ii.with_ii(array), ii.with_ii(index))
            }

            super::Expr::Tuple { fields, .. } => {
                write!(f, "{{")?;
                let mut i = fields.iter().map(|(name, val)| {
                    // This is the only place where we're building strings.  Could be aleviated
                    // if the named tuple field was a struct which could be Display.
                    format!(
                        "{}{}",
                        name.as_ref()
                            .map_or(String::new(), |name| format!("{}: ", name.name)),
                        ii.with_ii(val)
                    )
                });
                write_many_iter!(f, i, ", ");
                write!(f, "}}")
            }

            super::Expr::Array { elements, .. } => {
                write!(f, "[")?;
                write_many_with_ii!(f, elements, ", ", ii);
                write!(f, "]")
            }

            super::Expr::MacroCall { call, .. } => {
                write!(
                    f,
                    "{}(...)",
                    ii.calls.get(*call).unwrap_or(&"<CALL NOT FOUND>".to_owned())
                )
            }

            super::Expr::FnCall { name, args, .. } => {
                write!(f, "{name}(")?;
                write_many_with_ii!(f, args, ", ", ii);
                write!(f, ")")
            }

            super::Expr::If {
                condition,
                then_block,
                else_block,
                ..
            } => write!(
                f,
                "if {} {{ {} }} else {{ {} }}",
                ii.with_ii(condition),
                ii.with_ii(then_block),
                ii.with_ii(else_block)
            ),

            super::Expr::Range { lb, ub, .. } => {
                write!(f, "{}..{}", ii.with_ii(lb), ii.with_ii(ub))
            }
        }
    }
}

impl DisplayWithII for super::Immediate {
    fn fmt(&self, f: &mut Formatter, _: &IntermediateIntent) -> Result {
        write!(f, "{self}")
    }
}

impl DisplayWithII for super::Ident {
    fn fmt(&self, f: &mut Formatter, _: &IntermediateIntent) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl Display for super::Ident {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.name)
    }
}

impl Display for super::Immediate {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            super::Immediate::Real(n) => write!(f, "{n:e}"),
            super::Immediate::Int(n) => write!(f, "{n}"),
            super::Immediate::BigInt(n) => write!(f, "{n}"),
            super::Immediate::Bool(b) => write!(f, "{b}"),
            super::Immediate::String(s) => write!(f, "{s:?}"),
        }
    }
}
