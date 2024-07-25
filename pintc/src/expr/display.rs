use std::fmt::{Display, Formatter, Result};

use crate::{
    expr,
    predicate::{Contract, DisplayWithPred, Predicate},
    util::{write_many_iter, write_many_with_pred},
};

impl DisplayWithPred for super::ExprKey {
    fn fmt(&self, f: &mut Formatter, contract: &Contract, pred: &Predicate) -> Result {
        if contract.is_removed_macro_call(*self) {
            write!(f, "<REMOVED MACRO CALL>")
        } else {
            write!(f, "{}", pred.with_pred(contract, &self.get(contract)))
        }
    }
}

impl DisplayWithPred for &super::Expr {
    fn fmt(&self, f: &mut Formatter, contract: &Contract, pred: &Predicate) -> Result {
        match self {
            super::Expr::Error(..) => write!(f, "Error"),
            super::Expr::Immediate { value, .. } => value.fmt(f, contract, pred),

            super::Expr::Array { elements, .. } => {
                write!(f, "[")?;
                write_many_with_pred!(f, elements, ", ", contract, pred);
                write!(f, "]")
            }

            super::Expr::Tuple { fields, .. } => {
                write!(f, "{{")?;
                let mut i = fields.iter().map(|(name, val)| {
                    // This is the only place where we're building strings.  Could be alleviated
                    // if the named tuple field was a struct which could be Display.
                    format!(
                        "{}{}",
                        name.as_ref()
                            .map_or(String::new(), |name| format!("{}: ", name.name)),
                        pred.with_pred(contract, val)
                    )
                });
                write_many_iter!(f, i, ", ");
                write!(f, "}}")
            }

            super::Expr::PathByName(p, _) => write!(f, "{p}"),
            super::Expr::PathByKey(k, _) => write!(f, "{}", k.get(pred).name),
            super::Expr::StorageAccess(p, _) => write!(f, "storage::{p}"),
            super::Expr::ExternalStorageAccess {
                interface_instance,
                name,
                ..
            } => write!(f, "{interface_instance}::storage::{name}"),

            super::Expr::UnaryOp { op, expr, .. } => {
                if matches!(op, expr::UnaryOp::NextState) {
                    write!(f, "{}'", pred.with_pred(contract, expr))
                } else {
                    match op {
                        expr::UnaryOp::Error => write!(f, "error"),
                        expr::UnaryOp::Neg => write!(f, "-"),
                        expr::UnaryOp::Not => write!(f, "!"),
                        expr::UnaryOp::NextState => unreachable!(),
                    }?;
                    expr.fmt(f, contract, pred)
                }
            }

            super::Expr::BinaryOp { op, lhs, rhs, .. } => {
                write!(
                    f,
                    "({} {} {})",
                    pred.with_pred(contract, lhs),
                    op,
                    pred.with_pred(contract, rhs)
                )
            }

            super::Expr::In {
                value, collection, ..
            } => write!(
                f,
                "{} in {}",
                pred.with_pred(contract, value),
                pred.with_pred(contract, collection)
            ),

            super::Expr::Cast { value, ty, .. } => {
                write!(
                    f,
                    "{} as {}",
                    pred.with_pred(contract, value),
                    pred.with_pred(contract, ty.as_ref())
                )
            }

            super::Expr::TupleFieldAccess { tuple, field, .. } => {
                write!(f, "{}.{field}", pred.with_pred(contract, tuple))
            }

            super::Expr::Index { expr, index, .. } => {
                write!(
                    f,
                    "{}[{}]",
                    pred.with_pred(contract, expr),
                    pred.with_pred(contract, index)
                )
            }

            super::Expr::MacroCall { call, .. } => {
                write!(
                    f,
                    "{}(...)",
                    pred.calls
                        .get(*call)
                        .unwrap_or(&"<CALL NOT FOUND>".to_owned())
                )
            }

            super::Expr::IntrinsicCall { name, args, .. } => {
                write!(f, "{name}(")?;
                write_many_with_pred!(f, args, ", ", contract, pred);
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
                pred.with_pred(contract, condition),
                pred.with_pred(contract, then_expr),
                pred.with_pred(contract, else_expr)
            ),

            super::Expr::Range { lb, ub, .. } => {
                write!(
                    f,
                    "{}..{}",
                    pred.with_pred(contract, lb),
                    pred.with_pred(contract, ub)
                )
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
                    write!(f, " {} in {},", ident, pred.with_pred(contract, range))?;
                }
                if !conditions.is_empty() {
                    write!(f, " where ")?;
                    write_many_with_pred!(f, conditions, ", ", contract, pred);
                }
                write!(f, " {{ {} }}", pred.with_pred(contract, body))
            }
        }
    }
}

impl DisplayWithPred for super::Immediate {
    fn fmt(&self, f: &mut Formatter, contract: &Contract, pred: &Predicate) -> Result {
        match self {
            super::Immediate::Error => write!(f, "Error"),
            super::Immediate::Nil => write!(f, "nil"),
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
            super::Immediate::Array(elements) => {
                write!(f, "[")?;
                write_many_with_pred!(f, elements, ", ", contract, pred);
                write!(f, "]")
            }
            super::Immediate::Tuple(fields) => {
                write!(f, "{{")?;
                let mut i = fields.iter().map(|(name, val)| {
                    format!(
                        "{}{}",
                        name.as_ref()
                            .map_or(String::new(), |name| format!("{}: ", name.name)),
                        pred.with_pred(contract, val)
                    )
                });
                write_many_iter!(f, i, ", ");
                write!(f, "}}")
            }
        }
    }
}

impl DisplayWithPred for super::Ident {
    fn fmt(&self, f: &mut Formatter, _: &Contract, _: &Predicate) -> Result {
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
