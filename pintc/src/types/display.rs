use crate::predicate::{Contract, DisplayWithContract};
use std::fmt::{Display, Formatter, Result};

impl DisplayWithContract for super::Path {
    fn fmt(&self, f: &mut Formatter, _contract: &Contract) -> Result {
        write!(f, "{self}")
    }
}

impl Display for super::PrimitiveKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            super::PrimitiveKind::Nil => write!(f, "nil"),
            super::PrimitiveKind::Bool => write!(f, "bool"),
            super::PrimitiveKind::Int => write!(f, "int"),
            super::PrimitiveKind::Real => write!(f, "real"),
            super::PrimitiveKind::String => write!(f, "string"),
            super::PrimitiveKind::B256 => write!(f, "b256"),
        }
    }
}

impl DisplayWithContract for super::Type {
    fn fmt(&self, f: &mut Formatter, contract: &Contract) -> Result {
        match self {
            super::Type::Error(..) => write!(f, "Error"),

            super::Type::Unknown(..) => write!(f, "Unknown"),

            super::Type::Any(..) => write!(f, "_"),

            super::Type::Primitive { kind, .. } => write!(f, "{kind}"),

            super::Type::Array { ty, range, .. } => {
                write!(
                    f,
                    "{}[{}]",
                    contract.with_ctrct(ty.as_ref()),
                    range
                        .map(|range| contract.with_ctrct(range).to_string())
                        .unwrap_or("_".to_owned())
                )
            }

            super::Type::Tuple { fields, .. } => {
                macro_rules! write_field {
                    ($f: expr, $field: expr, $comma: expr, $contract: ident) => {{
                        if $comma {
                            write!($f, ", ")?;
                        };
                        if let Some(name) = &$field.0 {
                            write!($f, "{}: ", name.name)?;
                        }
                        write!($f, "{}", $contract.with_ctrct(&$field.1))?;
                    }};
                }

                write!(f, "{{")?;
                let mut fields = fields.iter();
                if let Some(first_field) = fields.next() {
                    write_field!(f, first_field, false, contract);
                }
                for field in fields {
                    write_field!(f, field, true, contract);
                }
                write!(f, "}}")
            }

            super::Type::Custom { path, .. } => write!(f, "{path}"),

            super::Type::Alias { path, ty, .. } => {
                write!(f, "{path} ({})", contract.with_ctrct(ty.as_ref()))
            }

            super::Type::Map { ty_from, ty_to, .. } => {
                write!(
                    f,
                    "( {} => {} )",
                    contract.with_ctrct(ty_from.as_ref()),
                    contract.with_ctrct(ty_to.as_ref())
                )
            }

            super::Type::Vector { ty, .. } => {
                write!(f, "{}[]", contract.with_ctrct(ty.as_ref()),)
            }
        }
    }
}

impl Display for super::EnumDecl {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "enum {} = ", self.name)?;
        crate::util::write_many!(f, self.variants, " | ");
        Ok(())
    }
}

impl DisplayWithContract for super::NewTypeDecl {
    fn fmt(&self, f: &mut Formatter, contract: &Contract) -> Result {
        write!(f, "type {} = {}", self.name, contract.with_ctrct(&self.ty))
    }
}
