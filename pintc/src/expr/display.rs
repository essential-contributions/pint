use std::fmt::{Display, Formatter, Result};

use crate::{
    expr,
    predicate::{Contract, DisplayWithContract},
    util::{write_many_iter, write_many_with_ctrct},
};

impl DisplayWithContract for super::ExprKey {
    fn fmt(&self, f: &mut Formatter, contract: &Contract) -> Result {
        if contract.is_removed_macro_call(*self) {
            write!(f, "<REMOVED MACRO CALL>")
        } else {
            write!(f, "{}", contract.with_ctrct(&self.get(contract)))
        }
    }
}

impl DisplayWithContract for &super::Expr {
    fn fmt(&self, f: &mut Formatter, contract: &Contract) -> Result {
        match self {
            super::Expr::Error(..) => write!(f, "Error"),
            super::Expr::Immediate { value, .. } => value.fmt(f, contract),

            super::Expr::Array { elements, .. } => {
                write!(f, "[")?;
                write_many_with_ctrct!(f, elements, ", ", contract);
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
                        contract.with_ctrct(val)
                    )
                });
                write_many_iter!(f, i, ", ");
                write!(f, "}}")
            }

            super::Expr::UnionVariant { path, value, .. } => {
                write!(f, "{path}")?;
                if let Some(value) = value {
                    write!(f, "({})", contract.with_ctrct(value))?;
                }
                Ok(())
            }

            super::Expr::Path(p, _) => write!(f, "{p}"),
            super::Expr::StorageAccess { name, mutable, .. } => {
                if *mutable {
                    write!(f, "mut ")?;
                }
                write!(f, "storage::{name}")
            }
            super::Expr::ExternalStorageAccess {
                interface_instance,
                name,
                ..
            } => write!(f, "{interface_instance}::storage::{name}"),

            super::Expr::UnaryOp { op, expr, .. } => {
                if matches!(op, expr::UnaryOp::NextState) {
                    write!(f, "{}'", contract.with_ctrct(expr))
                } else {
                    match op {
                        expr::UnaryOp::Error => write!(f, "error"),
                        expr::UnaryOp::Neg => write!(f, "-"),
                        expr::UnaryOp::Not => write!(f, "!"),
                        expr::UnaryOp::NextState => unreachable!(),
                    }?;
                    expr.fmt(f, contract)
                }
            }

            super::Expr::BinaryOp { op, lhs, rhs, .. } => {
                write!(
                    f,
                    "({} {} {})",
                    contract.with_ctrct(lhs),
                    op,
                    contract.with_ctrct(rhs)
                )
            }

            super::Expr::In {
                value, collection, ..
            } => write!(
                f,
                "{} in {}",
                contract.with_ctrct(value),
                contract.with_ctrct(collection)
            ),

            super::Expr::Cast { value, ty, .. } => {
                write!(
                    f,
                    "{} as {}",
                    contract.with_ctrct(value),
                    contract.with_ctrct(ty)
                )
            }

            super::Expr::TupleFieldAccess { tuple, field, .. } => {
                write!(f, "{}.{field}", contract.with_ctrct(tuple))
            }

            super::Expr::Index { expr, index, .. } => {
                write!(
                    f,
                    "{}[{}]",
                    contract.with_ctrct(expr),
                    contract.with_ctrct(index)
                )
            }

            super::Expr::MacroCall { path, .. } => {
                write!(f, "{path}(...)",)
            }

            super::Expr::IntrinsicCall { kind, args, .. } => {
                write!(f, "{}(", kind.0)?;
                write_many_with_ctrct!(f, args, ", ", contract);
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
                contract.with_ctrct(condition),
                contract.with_ctrct(then_expr),
                contract.with_ctrct(else_expr)
            ),

            super::Expr::Match {
                match_expr,
                match_branches,
                else_branch: else_expr,
                ..
            } => {
                write!(f, "match {} {{ ", contract.with_ctrct(match_expr))?;
                for (
                    idx,
                    super::MatchBranch {
                        name,
                        binding,
                        constraints,
                        expr,
                        ..
                    },
                ) in match_branches.iter().enumerate()
                {
                    write!(f, "{}{name}", if idx > 0 { ", " } else { "" })?;
                    if let Some(binding) = binding {
                        write!(f, "({binding})")?;
                    }
                    write!(f, " =>")?;
                    for c_expr in constraints {
                        write!(f, " constraint {};", contract.with_ctrct(c_expr))?;
                    }
                    write!(f, " {}", contract.with_ctrct(expr))?;
                }
                if let Some(super::MatchElse { constraints, expr }) = else_expr {
                    write!(f, ", else =>")?;
                    for c_expr in constraints {
                        write!(f, " constraint {};", contract.with_ctrct(c_expr))?;
                    }
                    write!(f, " {}", contract.with_ctrct(expr))?;
                }
                write!(f, " }}")
            }

            super::Expr::Range { lb, ub, .. } => {
                write!(
                    f,
                    "{}..{}",
                    contract.with_ctrct(lb),
                    contract.with_ctrct(ub)
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
                    write!(f, " {} in {},", ident, contract.with_ctrct(range))?;
                }
                if !conditions.is_empty() {
                    write!(f, " where ")?;
                    write_many_with_ctrct!(f, conditions, ", ", contract);
                }
                write!(f, " {{ {} }}", contract.with_ctrct(body))
            }

            expr::Expr::UnionTag { union_expr, .. } => {
                write!(f, "UnTag({})", contract.with_ctrct(union_expr))
            }

            expr::Expr::UnionValue {
                union_expr,
                variant_ty,
                ..
            } => {
                write!(
                    f,
                    "UnVal({}, {})",
                    contract.with_ctrct(union_expr),
                    contract.with_ctrct(variant_ty)
                )
            }
        }
    }
}

impl DisplayWithContract for super::Immediate {
    fn fmt(&self, f: &mut Formatter, contract: &Contract) -> Result {
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
                write_many_with_ctrct!(f, elements, ", ", contract);
                write!(f, "]")
            }
            super::Immediate::Tuple(fields) => {
                write!(f, "{{")?;
                let mut i = fields.iter().map(|(name, val)| {
                    format!(
                        "{}{}",
                        name.as_ref()
                            .map_or(String::new(), |name| format!("{}: ", name.name)),
                        contract.with_ctrct(val)
                    )
                });
                write_many_iter!(f, i, ", ");
                write!(f, "}}")
            }
        }
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
