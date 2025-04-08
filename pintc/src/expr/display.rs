use std::fmt::{Display, Formatter, Result};

use crate::{
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

            super::Expr::KeyValue { lhs, rhs, .. } => {
                write!(
                    f,
                    "({} := {})",
                    contract.with_ctrct(lhs),
                    contract.with_ctrct(rhs)
                )
            }

            super::Expr::Nil(_) => {
                write!(f, "nil")
            }

            super::Expr::Path(p, _) => write!(f, "{p}"),

            super::Expr::AsmBlock { args, ops, .. } => {
                write!(f, "asm(")?;
                write_many_with_ctrct!(f, args, ", ", contract);
                writeln!(f, ") {{")?;
                for op in ops {
                    // TODO: do not hardcode the indentation here.
                    writeln!(f, "        {op}")?;
                }
                write!(f, "    }}")
            }

            super::Expr::LocalStorageAccess { name, .. } => write!(f, "storage::{name}"),

            super::Expr::ExternalStorageAccess {
                interface,
                address,
                name,
                ..
            } => write!(
                f,
                "{interface}@[{}]::storage::{name}",
                contract.with_ctrct(address)
            ),

            super::Expr::UnaryOp { op, expr, .. } => {
                if matches!(op, super::UnaryOp::NextState) {
                    write!(f, "{}'", contract.with_ctrct(expr))
                } else if matches!(op, super::UnaryOp::Unwrap) {
                    write!(f, "{}!", contract.with_ctrct(expr))
                } else {
                    match op {
                        super::UnaryOp::Error => write!(f, "error"),
                        super::UnaryOp::Neg => write!(f, "-"),
                        super::UnaryOp::Not => write!(f, "!"),
                        super::UnaryOp::NextState | super::UnaryOp::Unwrap => unreachable!(),
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

            super::Expr::LocalPredicateCall {
                predicate, args, ..
            } => {
                write!(f, "{predicate}@[](",)?;
                write_many_with_ctrct!(f, args, ", ", contract);
                write!(f, ")")
            }

            super::Expr::ExternalPredicateCall {
                interface,
                c_addr,
                predicate,
                p_addr,
                args,
                ..
            } => {
                write!(
                    f,
                    "{interface}@[{}]::{predicate}@[{}](",
                    contract.with_ctrct(c_addr),
                    contract.with_ctrct(p_addr)
                )?;
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

            super::Expr::Map {
                param, range, body, ..
            } => {
                write!(
                    f,
                    "map {param} in {} {{ {} }}",
                    contract.with_ctrct(range),
                    contract.with_ctrct(body)
                )
            }

            super::Expr::UnionTag { union_expr, .. } => {
                write!(f, "UnTag({})", contract.with_ctrct(union_expr))
            }

            super::Expr::UnionValue {
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
            super::Immediate::UnionVariant {
                tag_num,
                value,
                decl,
                ..
            } => {
                let union = &contract.unions[*decl];
                write!(
                    f,
                    "{}::{}",
                    union.name.name,
                    union
                        .variants
                        .get(*tag_num as usize)
                        .map(|v| v.variant_name.name.as_str())
                        .unwrap_or("UNKNOWN_VARIANT")
                )?;
                if let Some(value) = value {
                    write!(f, "({})", contract.with_ctrct(value.as_ref()))?;
                }
                Ok(())
            }
        }
    }
}

impl Display for super::Ident {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.name)
    }
}

impl Display for super::AsmOp {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            super::AsmOp::Imm(imm, _) => write!(f, "{imm}"),
            super::AsmOp::Op(op) => write!(f, "{op}"),
        }
    }
}

impl Display for super::BinaryOp {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            super::BinaryOp::Add => write!(f, "+"),
            super::BinaryOp::Div => write!(f, "/"),
            super::BinaryOp::Equal => write!(f, "=="),
            super::BinaryOp::GreaterThanOrEqual => write!(f, ">="),
            super::BinaryOp::GreaterThan => write!(f, ">"),
            super::BinaryOp::LessThanOrEqual => write!(f, "<="),
            super::BinaryOp::LessThan => write!(f, "<"),
            super::BinaryOp::LogicalAnd => write!(f, "&&"),
            super::BinaryOp::LogicalOr => write!(f, "||"),
            super::BinaryOp::Mod => write!(f, "%"),
            super::BinaryOp::Mul => write!(f, "*"),
            super::BinaryOp::NotEqual => write!(f, "!="),
            super::BinaryOp::Sub => write!(f, "-"),
            super::BinaryOp::Concat => write!(f, "++"),
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
