use std::fmt::{Display, Formatter, Result};

use crate::{
    expr,
    intermediate::{DisplayWithII, IntermediateIntent},
    util::{write_many_iter, write_many_with_ii},
};

impl DisplayWithII for super::ExprKey {
    fn fmt(&self, f: &mut Formatter, ii: &IntermediateIntent) -> Result {
        if ii.removed_macro_calls.contains_key(*self) {
            write!(f, "<REMOVED MACRO CALL>")
        } else {
            write!(f, "{}", ii.with_ii(&self.get(ii)))
        }
    }
}

impl DisplayWithII for &super::Expr {
    fn fmt(&self, f: &mut Formatter, ii: &IntermediateIntent) -> Result {
        match self {
            super::Expr::Error(..) => write!(f, "Error"),
            super::Expr::Immediate { value, .. } => value.fmt(f, ii),

            super::Expr::PathByName(p, _) => write!(f, "{p}"),
            super::Expr::PathByKey(k, _) => write!(f, "{}", k.get(ii).name),
            super::Expr::StorageAccess(p, _) => write!(f, "storage::{p}"),
            super::Expr::ExternalStorageAccess {
                interface_instance,
                name,
                ..
            } => write!(f, "{interface_instance}::storage::{name}"),

            super::Expr::UnaryOp { op, expr, .. } => {
                if matches!(op, expr::UnaryOp::NextState) {
                    write!(f, "{}'", ii.with_ii(expr))
                } else {
                    match op {
                        expr::UnaryOp::Error => write!(f, "error"),
                        expr::UnaryOp::Neg => write!(f, "-"),
                        expr::UnaryOp::Not => write!(f, "!"),
                        expr::UnaryOp::NextState => unreachable!(),
                    }?;
                    expr.fmt(f, ii)
                }
            }

            super::Expr::BinaryOp { op, lhs, rhs, .. } => {
                write!(f, "({} {} {})", ii.with_ii(lhs), op, ii.with_ii(rhs))
            }

            super::Expr::In {
                value, collection, ..
            } => write!(f, "{} in {}", ii.with_ii(value), ii.with_ii(collection)),

            super::Expr::Cast { value, ty, .. } => {
                write!(f, "{} as {}", ii.with_ii(value), ii.with_ii(ty.as_ref()))
            }

            super::Expr::TupleFieldAccess { tuple, field, .. } => {
                write!(f, "{}.{field}", ii.with_ii(tuple))
            }

            super::Expr::Index { expr, index, .. } => {
                write!(f, "{}[{}]", ii.with_ii(expr), ii.with_ii(index))
            }

            super::Expr::MacroCall { call, .. } => {
                write!(
                    f,
                    "{}(...)",
                    ii.calls
                        .get(*call)
                        .unwrap_or(&"<CALL NOT FOUND>".to_owned())
                )
            }

            super::Expr::IntrinsicCall { name, args, .. } => {
                write!(f, "{name}(")?;
                write_many_with_ii!(f, args, ", ", ii);
                write!(f, ")")
            }

            super::Expr::Select {
                condition,
                then_expr,
                else_expr,
                ..
            } => write!(
                f,
                "({} ? {} : {})",
                ii.with_ii(condition),
                ii.with_ii(then_expr),
                ii.with_ii(else_expr)
            ),

            super::Expr::Range { lb, ub, .. } => {
                write!(f, "{}..{}", ii.with_ii(lb), ii.with_ii(ub))
            }

            super::Expr::Generator {
                kind,
                gen_ranges,
                body,
                conditions,
                ..
            } => {
                write!(f, "{kind}")?;
                for (ident, range) in gen_ranges {
                    write!(f, " {} in {},", ident, ii.with_ii(range))?;
                }
                if !conditions.is_empty() {
                    write!(f, " where ")?;
                    write_many_with_ii!(f, conditions, ", ", ii);
                }
                write!(f, " {{ {} }}", ii.with_ii(body))
            }
        }
    }
}

impl DisplayWithII for super::Immediate {
    fn fmt(&self, f: &mut Formatter, ii: &IntermediateIntent) -> Result {
        match self {
            super::Immediate::Error => write!(f, "Error"),
            super::Immediate::Real(n) => write!(f, "{n:e}"),
            super::Immediate::Int(n) => write!(f, "{n}"),
            super::Immediate::Bool(b) => write!(f, "{b}"),
            super::Immediate::String(s) => write!(f, "{s:?}"),
            super::Immediate::B256(val) => {
                write!(
                    f,
                    "0x{:016X}{:016X}{:016X}{:016X}",
                    val[0], val[1], val[2], val[3]
                )
            }
            super::Immediate::Array { elements, .. } => {
                write!(f, "[")?;
                write_many_with_ii!(f, elements, ", ", ii);
                write!(f, "]")
            }
            super::Immediate::Tuple(fields) => {
                write!(f, "{{")?;
                let mut i = fields.iter().map(|(name, val)| {
                    // This is the only place where we're building strings.  Could be alleviated
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
        }
    }
}

impl DisplayWithII for super::Ident {
    fn fmt(&self, f: &mut Formatter, _: &IntermediateIntent) -> Result {
        write!(f, "{self}")
    }
}

impl Display for super::Ident {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.name)
    }
}

impl Display for super::BinaryOp {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
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
        }
    }
}

impl Display for super::GeneratorKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Self::ForAll => write!(f, "forall"),
            Self::Exists => write!(f, "exists"),
        }
    }
}

impl Display for super::TupleAccess {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            super::TupleAccess::Error => write!(f, "Error"),
            super::TupleAccess::Index(n) => write!(f, "{n}"),
            super::TupleAccess::Name(i) => write!(f, "{}", i.name),
        }
    }
}
