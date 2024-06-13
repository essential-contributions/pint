use crate::{
    expr::Ident,
    intermediate::ExprKey,
    span::{Span, Spanned},
};

mod display;

pub type Path = String;

#[derive(Clone, Debug, PartialEq)]
pub enum PrimitiveKind {
    Nil,
    Bool,
    Int,
    Real,
    String,
    B256,
}

#[derive(Clone, Debug)]
pub enum Type {
    Error(Span),
    Unknown(Span),
    Primitive {
        kind: PrimitiveKind,
        span: Span,
    },
    Array {
        ty: Box<Self>,
        range: ExprKey,
        size: Option<i64>,
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
    Alias {
        path: Path,
        ty: Box<Self>,
        span: Span,
    },
    Map {
        ty_from: Box<Self>,
        ty_to: Box<Self>,
        span: Span,
    },
}

macro_rules! is_primitive {
    ($self: ident, $kind: pat) => {
        matches!($self, Type::Primitive { kind: $kind, .. })
    };
}

macro_rules! check_alias {
    ($self: ident, $recurse: ident, $otherwise: expr) => {
        $self
            .is_alias()
            .map(|ty| ty.$recurse())
            .unwrap_or_else(|| $otherwise)
    };
    ($self: ident, $recurse: ident, $recurse_arg: expr, $otherwise: expr) => {
        $self
            .is_alias()
            .map(|ty| ty.$recurse($recurse_arg))
            .unwrap_or_else(|| $otherwise)
    };
}

impl Type {
    pub fn is_unknown(&self) -> bool {
        matches!(self, Type::Unknown(_))
    }

    pub fn is_alias(&self) -> Option<&Type> {
        if let Type::Alias { ty, .. } = self {
            Some(ty)
        } else {
            None
        }
    }

    pub fn is_nil(&self) -> bool {
        check_alias!(self, is_nil, is_primitive!(self, PrimitiveKind::Nil))
    }

    pub fn is_bool(&self) -> bool {
        check_alias!(self, is_bool, is_primitive!(self, PrimitiveKind::Bool))
    }

    pub fn is_int(&self) -> bool {
        check_alias!(self, is_int, is_primitive!(self, PrimitiveKind::Int))
    }

    pub fn is_real(&self) -> bool {
        check_alias!(self, is_real, is_primitive!(self, PrimitiveKind::Real))
    }

    pub fn is_string(&self) -> bool {
        check_alias!(self, is_string, is_primitive!(self, PrimitiveKind::String))
    }

    pub fn is_b256(&self) -> bool {
        check_alias!(self, is_b256, is_primitive!(self, PrimitiveKind::B256))
    }

    pub fn is_num(&self) -> bool {
        check_alias!(self, is_num, self.is_int() || self.is_real())
    }

    pub fn is_map(&self) -> bool {
        check_alias!(self, is_map, matches!(self, Type::Map { .. }))
    }

    pub fn is_any_primitive(&self) -> bool {
        check_alias!(
            self,
            is_any_primitive,
            matches!(self, Type::Primitive { .. })
        )
    }

    pub fn is_enum(&self) -> bool {
        check_alias!(self, is_enum, matches!(self, Type::Custom { .. }))
    }

    pub fn get_enum_name(&self) -> Option<&Path> {
        check_alias!(self, get_enum_name, {
            if let Type::Custom { path, .. } = self {
                Some(path)
            } else {
                None
            }
        })
    }

    pub fn is_tuple(&self) -> bool {
        check_alias!(self, is_tuple, matches!(self, Type::Tuple { .. }))
    }

    pub fn is_array(&self) -> bool {
        check_alias!(self, is_array, matches!(self, Type::Array { .. }))
    }

    pub fn get_array_el_type(&self) -> Option<&Type> {
        check_alias!(self, get_array_el_type, {
            if let Type::Array { ty, .. } = self {
                Some(ty)
            } else {
                None
            }
        })
    }

    pub fn get_array_range_expr(&self) -> Option<ExprKey> {
        check_alias!(self, get_array_range_expr, {
            if let Type::Array { range, .. } = self {
                Some(*range)
            } else {
                None
            }
        })
    }

    pub fn get_array_size(&self) -> Option<i64> {
        check_alias!(self, get_array_size, {
            if let Type::Array { size, .. } = self {
                *size
            } else {
                None
            }
        })
    }

    pub fn get_map_ty_from(&self) -> Option<&Type> {
        check_alias!(self, get_map_ty_from, {
            if let Type::Map { ty_from, .. } = self {
                Some(ty_from)
            } else {
                None
            }
        })
    }

    pub fn get_map_ty_to(&self) -> Option<&Type> {
        check_alias!(self, get_map_ty_to, {
            if let Type::Map { ty_to, .. } = self {
                Some(ty_to)
            } else {
                None
            }
        })
    }

    pub fn get_tuple_fields(&self) -> Option<&[(Option<Ident>, Self)]> {
        check_alias!(self, get_tuple_fields, {
            if let Type::Tuple { fields, .. } = self {
                Some(fields)
            } else {
                None
            }
        })
    }

    pub fn get_tuple_field_type_by_idx(&self, idx: usize) -> Option<&Type> {
        check_alias!(self, get_tuple_field_type_by_idx, idx, {
            if let Type::Tuple { fields, .. } = self {
                fields.get(idx).map(|(_, ty)| ty)
            } else {
                None
            }
        })
    }

    pub fn get_tuple_field_type_by_name(&self, name: &Ident) -> Option<&Type> {
        check_alias!(self, get_tuple_field_type_by_name, name, {
            if let Type::Tuple { fields, .. } = self {
                fields.iter().find_map(|(field_name, ty)| {
                    field_name
                        .as_ref()
                        .and_then(|field_name| (field_name.name == name.name).then_some(ty))
                })
            } else {
                None
            }
        })
    }

    pub fn size(&self) -> usize {
        match self {
            Self::Primitive {
                kind: PrimitiveKind::Bool | PrimitiveKind::Int | PrimitiveKind::Real,
                ..
            } => 1,

            Self::Primitive {
                kind: PrimitiveKind::B256,
                ..
            } => 4,

            Self::Tuple { fields, .. } => fields
                .iter()
                .fold(0, |acc, (_, field_ty)| acc + field_ty.size()),

            Self::Array { ty, size, .. } => {
                if let Some(size) = size {
                    ty.size() * *size as usize
                } else {
                    unimplemented!("unable to find type size for array at the moment")
                }
            }

            // The point here is that a `Map` takes up a storage slot, even though it doesn't
            // actually store anything in it. The `Map` type is not really allowed anywhere else,
            // so we can't have a decision variable of type `Map` for example.
            Self::Map { .. } => 1,
            _ => unimplemented!("Size of type is not yet specified"),
        }
    }

    /// Calculate the number of storage slots required for this type. All primitive types fit in a
    /// single slot even if their size is > 1.
    pub fn storage_slots(&self) -> usize {
        match self {
            Self::Primitive { .. } => 1,

            Self::Tuple { fields, .. } => fields
                .iter()
                .fold(0, |acc, (_, field_ty)| acc + field_ty.storage_slots()),

            Self::Array { ty, size, .. } => {
                if let Some(size) = size {
                    ty.storage_slots() * *size as usize
                } else {
                    unimplemented!("unable to find type size for array at the moment")
                }
            }

            // The point here is that a `Map` takes up a storage slot, even though it doesn't
            // actually store anything in it. The `Map` type is not really allowed anywhere else,
            // so we can't have a decision variable of type `Map` for example.
            Self::Map { .. } => 1,
            _ => unimplemented!("Size of type is not yet specified"),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Error(_), Self::Error(_)) => true,
            (Self::Unknown(_), Self::Unknown(_)) => true,

            (Self::Alias { ty: lhs_ty, .. }, rhs) => &**lhs_ty == rhs,
            (lhs, Self::Alias { ty: rhs_ty, .. }) => lhs == &**rhs_ty,

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
                        let lhs_types: fxhash::FxHashMap<String, &Type> =
                            fxhash::FxHashMap::from_iter(lhs_fields.iter().map(
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

            (
                Self::Map {
                    ty_from: lhs_ty_from,
                    ty_to: lhs_ty_to,
                    ..
                },
                Self::Map {
                    ty_from: rhs_ty_from,
                    ty_to: rhs_ty_to,
                    ..
                },
            ) => lhs_ty_from == rhs_ty_from && lhs_ty_to == rhs_ty_to,

            _ => false,
        }
    }
}

impl Spanned for Type {
    fn span(&self) -> &Span {
        use Type::*;
        match &self {
            Error(span)
            | Unknown(span)
            | Primitive { span, .. }
            | Array { span, .. }
            | Tuple { span, .. }
            | Custom { span, .. }
            | Alias { span, .. }
            | Map { span, .. } => span,
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
