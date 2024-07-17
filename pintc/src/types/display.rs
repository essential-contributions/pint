use crate::predicate::{Contract, DisplayWithPred, Predicate};
use std::fmt::{Formatter, Result};

impl DisplayWithPred for super::Path {
    fn fmt(&self, f: &mut Formatter, _contract: &Contract, _pred: &Predicate) -> Result {
        write!(f, "{self}")
    }
}

impl DisplayWithPred for super::Type {
    fn fmt(&self, f: &mut Formatter, contract: &Contract, pred: &Predicate) -> Result {
        match self {
            super::Type::Error(..) => write!(f, "Error"),

            super::Type::Unknown(..) => write!(f, "Unknown"),

            super::Type::Primitive { kind, .. } => match kind {
                super::PrimitiveKind::Nil => write!(f, "nil"),
                super::PrimitiveKind::Bool => write!(f, "bool"),
                super::PrimitiveKind::Int => write!(f, "int"),
                super::PrimitiveKind::Real => write!(f, "real"),
                super::PrimitiveKind::String => write!(f, "string"),
                super::PrimitiveKind::B256 => write!(f, "b256"),
            },

            super::Type::Array { ty, range, .. } => {
                write!(
                    f,
                    "{}[{}]",
                    pred.with_pred(contract, ty.as_ref()),
                    range
                        .map(|range| pred.with_pred(contract, range).to_string())
                        .unwrap_or("_".to_owned())
                )
            }

            super::Type::Tuple { fields, .. } => {
                macro_rules! write_field {
                    ($f: expr, $field: expr, $comma: expr, $contract: ident, $pred: expr) => {{
                        if $comma {
                            write!($f, ", ")?;
                        };
                        if let Some(name) = &$field.0 {
                            write!($f, "{}: ", name.name)?;
                        }
                        write!($f, "{}", $pred.with_pred($contract, &$field.1))?;
                    }};
                }

                write!(f, "{{")?;
                let mut fields = fields.iter();
                if let Some(first_field) = fields.next() {
                    write_field!(f, first_field, false, contract, pred);
                }
                for field in fields {
                    write_field!(f, field, true, contract, pred);
                }
                write!(f, "}}")
            }

            super::Type::Custom { path, .. } => write!(f, "{path}"),

            super::Type::Alias { path, ty, .. } => {
                write!(f, "{path} ({})", pred.with_pred(contract, ty.as_ref()))
            }

            super::Type::Map { ty_from, ty_to, .. } => {
                write!(
                    f,
                    "( {} => {} )",
                    pred.with_pred(contract, ty_from.as_ref()),
                    pred.with_pred(contract, ty_to.as_ref())
                )
            }
        }
    }
}

impl DisplayWithPred for super::EnumDecl {
    fn fmt(&self, f: &mut Formatter, _contract: &Contract, _pred: &Predicate) -> Result {
        write!(f, "enum {} = ", self.name)?;
        crate::util::write_many!(f, self.variants, " | ");
        Ok(())
    }
}

impl DisplayWithPred for super::NewTypeDecl {
    fn fmt(&self, f: &mut Formatter, contract: &Contract, pred: &Predicate) -> Result {
        write!(
            f,
            "type {} = {}",
            self.name,
            pred.with_pred(contract, &self.ty)
        )
    }
}
