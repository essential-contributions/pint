use std::fmt::{Display, Formatter, Result};

impl<Path: Display, Expr: Display> Display for super::Type<Path, Expr> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            super::Type::Primitive { kind, .. } => match kind {
                super::PrimitiveKind::Bool => write!(f, "bool"),
                super::PrimitiveKind::Int => write!(f, "int"),
                super::PrimitiveKind::Real => write!(f, "real"),
                super::PrimitiveKind::String => write!(f, "string"),
            },

            super::Type::Array { ty, range, .. } => write!(f, "{ty}[{range}]"),

            super::Type::Tuple { fields, .. } => {
                macro_rules! write_field {
                    ($f: expr, $field: expr, $comma: expr) => {{
                        if $comma {
                            write!($f, ", ")?;
                        };
                        if let Some(name) = &$field.0 {
                            write!($f, "{}: ", name.name)?;
                        }
                        write!($f, "{}", $field.1)?;
                    }};
                }

                write!(f, "{{")?;
                let mut fields = fields.iter();
                if let Some(first_field) = fields.next() {
                    write_field!(f, first_field, false);
                }
                for field in fields {
                    write_field!(f, field, true);
                }
                write!(f, "}}")
            }

            super::Type::CustomType { path, .. } => write!(f, "{path}"),
        }
    }
}

impl<Type: Display> Display for super::FnSig<Type> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "fn {}(", self.name)?;
        let mut i = self.params.iter();
        if let Some((id, ty)) = i.next() {
            write!(f, "{id}: {ty}")?;
        }
        for (id, ty) in i {
            write!(f, ", {id}: {ty}")?;
        }
        write!(f, ") -> {}", self.return_type)
    }
}

impl Display for super::EnumDecl {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "enum {} = ", self.name)?;
        crate::util::write_many!(f, self.variants, " | ");
        Ok(())
    }
}
