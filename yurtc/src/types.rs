use crate::{
    expr::Ident,
    intermediate::ExprKey,
    span::{Span, Spanned},
};

mod display;

pub type Path = String;

#[derive(Clone, Debug, PartialEq)]
pub enum PrimitiveKind {
    Bool,
    Int,
    Real,
    String,
}

#[derive(Clone, Debug)]
pub enum Type {
    Error(Span),
    Primitive {
        kind: PrimitiveKind,
        span: Span,
    },
    Array {
        ty: Box<Self>,
        range: ExprKey,
        span: Span,
    },
    Tuple {
        fields: Vec<(Option<Ident>, Self)>,
        span: Span,
    },
    Custom {
        path: Path,
        span: Span,
    },
}

macro_rules! is_primitive {
    ($self: ident, $kind: pat) => {
        matches!($self, Type::Primitive { kind: $kind, .. })
    };
}

impl Type {
    pub fn is_bool(&self) -> bool {
        is_primitive!(self, PrimitiveKind::Bool)
    }

    pub fn is_int(&self) -> bool {
        is_primitive!(self, PrimitiveKind::Int)
    }

    pub fn is_real(&self) -> bool {
        is_primitive!(self, PrimitiveKind::Real)
    }

    pub fn is_string(&self) -> bool {
        is_primitive!(self, PrimitiveKind::String)
    }

    pub fn is_num(&self) -> bool {
        self.is_int() || self.is_real()
    }

    pub fn is_any_primitive(&self) -> bool {
        matches!(self, Type::Primitive { .. })
    }

    pub fn is_enum(&self) -> bool {
        matches!(self, Type::Custom { .. })
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Error(_), Self::Error(_)) => true,

            (Self::Primitive { kind: lhs, .. }, Self::Primitive { kind: rhs, .. }) => lhs == rhs,

            // This is sub-optimal; we're saying two arrays of the same element type are
            // equivalent, regardless of their size.
            (Self::Array { ty: lhs_ty, .. }, Self::Array { ty: rhs_ty, .. }) => lhs_ty == rhs_ty,

            (
                Self::Tuple {
                    fields: lhs_fields, ..
                },
                Self::Tuple {
                    fields: rhs_fields, ..
                },
            ) => {
                if lhs_fields.len() != rhs_fields.len() {
                    false
                } else {
                    // If all the fields are named for both tuples then we can compare them
                    // name-wise.
                    if lhs_fields.iter().all(|(name, _)| name.is_some())
                        && rhs_fields.iter().all(|(name, _)| name.is_some())
                    {
                        let lhs_types: std::collections::HashMap<String, &Type> =
                            std::collections::HashMap::from_iter(lhs_fields.iter().map(
                                |(opt_name_id, ty)| {
                                    (
                                        opt_name_id
                                            .as_ref()
                                            .expect("have already checked is Some")
                                            .name
                                            .clone(),
                                        ty,
                                    )
                                },
                            ));
                        rhs_fields.iter().all(|(rhs_name, rhs_ty)| {
                            lhs_types
                                .get(
                                    &rhs_name
                                        .as_ref()
                                        .expect("have already checked is Some")
                                        .name,
                                )
                                .map(|lhs_ty| lhs_ty == &rhs_ty)
                                .unwrap_or(false)
                        })
                    } else {
                        // Otherwise we compare them in declared order.
                        lhs_fields
                            .iter()
                            .zip(rhs_fields.iter())
                            .all(|((_, lhs_ty), (_, rhs_ty))| lhs_ty == rhs_ty)
                    }
                }
            }

            (Self::Custom { path: lhs_path, .. }, Self::Custom { path: rhs_path, .. }) => {
                lhs_path == rhs_path
            }

            _ => false,
        }
    }
}

impl Spanned for Type {
    fn span(&self) -> &Span {
        use Type::*;
        match &self {
            Error(span)
            | Primitive { span, .. }
            | Array { span, .. }
            | Tuple { span, .. }
            | Custom { span, .. } => span,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumDecl {
    pub(super) name: Ident,
    pub(super) variants: Vec<Ident>,
    pub(super) span: Span,
}

impl Spanned for EnumDecl {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct NewTypeDecl {
    pub(super) name: Ident,
    pub(super) ty: Type,
    pub(super) span: Span,
}

impl Spanned for NewTypeDecl {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct EphemeralDecl {
    pub(super) name: String,
    pub(super) ty: Type,
    pub(super) span: Span,
}

impl Spanned for EphemeralDecl {
    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnSig {
    pub(super) name: Ident,
    pub(super) params: Vec<(Ident, Type)>,
    pub(super) return_type: Type,
    pub(super) span: Span,
}

impl Spanned for FnSig {
    fn span(&self) -> &Span {
        &self.span
    }
}
