use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::{evaluate::Evaluator, Expr, Ident, Immediate},
    predicate::{Contract, ExprKey},
    span::{Span, Spanned},
};
use pint_abi_types::{TupleField, TypeABI};

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
        range: Option<ExprKey>,
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
    Vector {
        ty: Box<Self>,
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

    pub fn is_vector(&self) -> bool {
        check_alias!(self, is_vector, matches!(self, Type::Vector { .. }))
    }

    pub fn is_any_primitive(&self) -> bool {
        check_alias!(
            self,
            is_any_primitive,
            matches!(self, Type::Primitive { .. })
        )
    }

    pub fn is_custom(&self) -> bool {
        check_alias!(self, is_custom, matches!(self, Type::Custom { .. }))
    }

    pub fn get_custom_name(&self) -> Option<&Path> {
        check_alias!(self, get_custom_name, {
            if let Type::Custom { path, .. } = self {
                Some(path)
            } else {
                None
            }
        })
    }

    pub fn is_enum(&self, enums: &[EnumDecl]) -> bool {
        self.get_enum_name(enums).is_some()
    }

    pub fn get_enum_name(&self, enums: &[EnumDecl]) -> Option<&Path> {
        check_alias!(self, get_enum_name, enums, {
            if let Type::Custom { path, .. } = self {
                enums
                    .iter()
                    .find_map(|EnumDecl { name, .. }| (&name.name == path).then_some(path))
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
                *range
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

    pub fn get_array_size_from_range_expr(
        handler: &Handler,
        range_expr: &Expr,
        contract: &Contract,
    ) -> Result<i64, ErrorEmitted> {
        if let Expr::Path(path, _) = range_expr {
            // It's hopefully an enum for the range expression.
            if let Some(size) = contract.enums.iter().find_map(|enum_decl| {
                (&enum_decl.name.name == path).then_some(enum_decl.variants.len() as i64)
            }) {
                Ok(size)
            } else {
                Err(handler.emit_err(Error::Compile {
                    error: CompileError::NonConstArrayLength {
                        span: range_expr.span().clone(),
                    },
                }))
            }
        } else {
            match Evaluator::new(&contract.enums).evaluate(range_expr, handler, contract) {
                Ok(Immediate::Int(size)) if size > 0 => Ok(size),
                Ok(_) => Err(handler.emit_err(Error::Compile {
                    error: CompileError::InvalidConstArrayLength {
                        span: range_expr.span().clone(),
                    },
                })),
                _ => Err(handler.emit_err(Error::Compile {
                    error: CompileError::NonConstArrayLength {
                        span: range_expr.span().clone(),
                    },
                })),
            }
        }
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

    pub fn get_vector_element_ty(&self) -> Option<&Type> {
        check_alias!(self, get_vector_element_ty, {
            if let Type::Vector { ty, .. } = self {
                Some(ty)
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

    pub fn size(&self, handler: &Handler, contract: &Contract) -> Result<usize, ErrorEmitted> {
        match self {
            Self::Primitive {
                kind: PrimitiveKind::Bool | PrimitiveKind::Int | PrimitiveKind::Real,
                ..
            } => Ok(1),

            Self::Primitive {
                kind: PrimitiveKind::B256,
                ..
            } => Ok(4),

            Self::Tuple { fields, .. } => fields.iter().try_fold(0, |acc, (_, field_ty)| {
                field_ty.size(handler, contract).map(|size| acc + size)
            }),

            Self::Array {
                ty, range, size, ..
            } => Ok(ty.size(handler, contract)?
                * size.unwrap_or(
                    Self::get_array_size_from_range_expr(
                        handler,
                        range
                            .as_ref()
                            .and_then(|e| e.try_get(contract))
                            .expect("expr key guaranteed to exist"),
                        contract,
                    )
                    .unwrap(),
                ) as usize),

            // The point here is that a `Map` takes up a storage slot, even though it doesn't
            // actually store anything in it. The `Map` type is not really allowed anywhere else,
            // so we can't have a decision variable of type `Map` for example.
            Self::Map { .. } => Ok(1),

            // `Vector` also takes up a single storage slot that stores the length of the vector
            Self::Vector { .. } => Ok(1),
            _ => unimplemented!("Size of type is not yet specified"),
        }
    }

    /// Calculate the number of storage or transient slots required for this type. All primitive
    /// types fit in a single slot even if their size is > 1. The math is the same for storage and
    /// transient data
    pub fn storage_or_transient_slots(
        &self,
        handler: &Handler,
        contract: &Contract,
    ) -> Result<usize, ErrorEmitted> {
        match self {
            Self::Primitive { .. } => Ok(1),

            Self::Tuple { fields, .. } => fields.iter().try_fold(0, |acc, (_, field_ty)| {
                field_ty
                    .storage_or_transient_slots(handler, contract)
                    .map(|slots| acc + slots)
            }),

            Self::Array {
                ty, range, size, ..
            } => Ok(ty.storage_or_transient_slots(handler, contract)?
                * size.unwrap_or(Self::get_array_size_from_range_expr(
                    handler,
                    range
                        .as_ref()
                        .and_then(|e| e.try_get(contract))
                        .expect("expr key guaranteed to exist"),
                    contract,
                )?) as usize),

            // The point here is that a `Map` takes up a storage slot, even though it doesn't
            // actually store anything in it. The `Map` type is not really allowed anywhere else,
            // so we can't have a decision variable of type `Map` for example.
            Self::Map { .. } => Ok(1),

            // `Vector` also takes up a single storage slot that stores the length of the vector
            Self::Vector { .. } => Ok(1),
            _ => unimplemented!("Size of type is not yet specified"),
        }
    }

    /// Produce a `TypeABI` given a `Type`.
    pub fn abi(&self, handler: &Handler, contract: &Contract) -> Result<TypeABI, ErrorEmitted> {
        match self {
            Type::Primitive { kind, .. } => Ok(match kind {
                PrimitiveKind::Bool => TypeABI::Bool,
                PrimitiveKind::Int => TypeABI::Int,
                PrimitiveKind::Real => TypeABI::Real,
                PrimitiveKind::String => TypeABI::String,
                PrimitiveKind::B256 => TypeABI::B256,
                _ => unimplemented!(),
            }),
            Type::Tuple { fields, .. } => Ok(TypeABI::Tuple(
                fields
                    .iter()
                    .map(|(name, field_ty)| {
                        Ok(TupleField {
                            name: name.as_ref().map(|name| name.name.clone()),
                            ty: field_ty.abi(handler, contract)?,
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            Type::Array {
                ty, range, size, ..
            } => Ok(TypeABI::Array {
                ty: Box::new(ty.abi(handler, contract)?),
                size: size.unwrap_or(Self::get_array_size_from_range_expr(
                    handler,
                    range
                        .as_ref()
                        .and_then(|e| e.try_get(contract))
                        .expect("expr key guaranteed to exist"),
                    contract,
                )?),
            }),
            Type::Map { ty_from, ty_to, .. } => Ok(TypeABI::Map {
                ty_from: Box::new((*ty_from).abi(handler, contract)?),
                ty_to: Box::new((*ty_to).abi(handler, contract)?),
            }),

            // This, of course, is incorrect. It's just a placeholder until we can support ABI gen
            // for storage vectors
            Type::Vector { .. } => Ok(TypeABI::Int),
            _ => unimplemented!("other types are not yet supported"),
        }
    }

    pub fn eq(&self, new_types: &[NewTypeDecl], other: &Self) -> bool {
        match (self, other) {
            (Self::Error(_), Self::Error(_)) => true,
            (Self::Unknown(_), Self::Unknown(_)) => true,

            (Self::Alias { ty: lhs_ty, .. }, rhs) => lhs_ty.eq(new_types, rhs),
            (lhs, Self::Alias { ty: rhs_ty, .. }) => lhs.eq(new_types, rhs_ty.as_ref()),

            (Self::Primitive { kind: lhs, .. }, Self::Primitive { kind: rhs, .. }) => lhs == rhs,

            // This is sub-optimal; we're saying two arrays of the same element type are
            // equivalent, regardless of their size.
            (Self::Array { ty: lhs_ty, .. }, Self::Array { ty: rhs_ty, .. }) => {
                lhs_ty.eq(new_types, rhs_ty)
            }

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
                                .map(|lhs_ty| lhs_ty.eq(new_types, rhs_ty))
                                .unwrap_or(false)
                        })
                    } else {
                        // Otherwise we compare them in declared order.
                        lhs_fields
                            .iter()
                            .zip(rhs_fields.iter())
                            .all(|((_, lhs_ty), (_, rhs_ty))| lhs_ty.eq(new_types, rhs_ty))
                    }
                }
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
            ) => lhs_ty_from.eq(new_types, rhs_ty_from) && lhs_ty_to.eq(new_types, rhs_ty_to),

            (Self::Vector { ty: lhs_ty, .. }, Self::Vector { ty: rhs_ty, .. }) => {
                lhs_ty.eq(new_types, rhs_ty)
            }

            (lhs_ty, rhs_ty) => {
                // Custom types are tricky as they may be either aliases or enums.  Or, at this
                // stage, we might just have two different types.
                let mut lhs_alias_ty = None;
                let mut lhs_enum_path = None;

                if let Self::Custom { path: lhs_path, .. } = lhs_ty {
                    lhs_alias_ty = new_types.iter().find_map(|NewTypeDecl { name, ty, .. }| {
                        (lhs_path == &name.name).then_some(ty)
                    });
                    lhs_enum_path = Some(lhs_path);
                }

                if let Some(lhs_alias_ty) = lhs_alias_ty {
                    // The LHS is an alias; recurse.
                    return lhs_alias_ty.eq(new_types, rhs_ty);
                }

                let mut rhs_alias_ty = None;
                let mut rhs_enum_path = None;

                if let Self::Custom { path: rhs_path, .. } = rhs_ty {
                    rhs_alias_ty = new_types.iter().find_map(|NewTypeDecl { name, ty, .. }| {
                        (rhs_path == &name.name).then_some(ty)
                    });
                    rhs_enum_path = Some(rhs_path);
                }

                if let Some(rhs_alias_ty) = rhs_alias_ty {
                    // The RHS is an alias; recurse.
                    return rhs_alias_ty.eq(new_types, lhs_ty);
                }

                if let (Some(lhs_enum_path), Some(rhs_enum_path)) = (lhs_enum_path, rhs_enum_path) {
                    // Neither are aliases but both are custom types; assume they're both enums.
                    lhs_enum_path == rhs_enum_path
                } else {
                    // OK, they must just be different types.
                    false
                }
            }
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
            | Map { span, .. }
            | Vector { span, .. } => span,
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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
