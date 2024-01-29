use crate::intent::intermediate::{DisplayWithII, IntermediateIntent};
use std::fmt::{Formatter, Result};

impl DisplayWithII for super::Path {
    fn fmt(&self, f: &mut Formatter<'_>, _ii: &IntermediateIntent) -> Result {
        write!(f, "{self}")
    }
}

impl DisplayWithII for super::Type {
    fn fmt(&self, f: &mut Formatter<'_>, ii: &IntermediateIntent) -> Result {
        match self {
            super::Type::Error(..) => write!(f, "Error"),

            super::Type::Primitive { kind, .. } => match kind {
                super::PrimitiveKind::Bool => write!(f, "bool"),
                super::PrimitiveKind::Int => write!(f, "int"),
                super::PrimitiveKind::Real => write!(f, "real"),
                super::PrimitiveKind::String => write!(f, "string"),
            },

            super::Type::Array { ty, range, .. } => {
                write!(f, "{}[", ii.with_ii(*ty.clone()))?;
                write!(f, "{}]", ii.with_ii(range))
            }

            super::Type::Tuple { fields, .. } => {
                macro_rules! write_field {
                    ($f: expr, $field: expr, $comma: expr, $ii: expr) => {{
                        if $comma {
                            write!($f, ", ")?;
                        };
                        if let Some(name) = &$field.0 {
                            write!($f, "{}: ", name.name)?;
                        }
                        write!($f, "{}", $ii.with_ii($field.1.clone()))?;
                    }};
                }

                write!(f, "{{")?;
                let mut fields = fields.iter();
                if let Some(first_field) = fields.next() {
                    write_field!(f, first_field, false, ii);
                }
                for field in fields {
                    write_field!(f, field, true, ii);
                }
                write!(f, "}}")
            }

            super::Type::Custom { path, .. } => write!(f, "{path}"),
        }
    }
}

impl DisplayWithII for super::FnSig {
    fn fmt(&self, f: &mut Formatter<'_>, ii: &IntermediateIntent) -> Result {
        write!(f, "fn {}(", self.name)?;
        let mut i = self.params.iter();
        if let Some((id, ty)) = i.next() {
            write!(f, "{id}: {}", ii.with_ii(ty))?;
        }
        for (id, ty) in i {
            write!(f, ", {id}: {}", ii.with_ii(ty))?;
        }
        write!(f, ") -> {}", ii.with_ii(self.return_type.clone()))
    }
}

impl DisplayWithII for super::EnumDecl {
    fn fmt(&self, f: &mut Formatter<'_>, _ii: &IntermediateIntent) -> Result {
        write!(f, "enum {} = ", self.name)?;
        crate::util::write_many!(f, self.variants, " | ");
        Ok(())
    }
}

impl DisplayWithII for super::NewTypeDecl {
    fn fmt(&self, f: &mut Formatter<'_>, ii: &IntermediateIntent) -> Result {
        write!(f, "type {} = {}", self.name, ii.with_ii(self.ty.clone()))?;
        Ok(())
    }
}
