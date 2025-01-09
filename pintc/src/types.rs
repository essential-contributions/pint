use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::{evaluate::Evaluator, Expr, Ident, Immediate},
    predicate::{Contract, ExprKey, UnionKey},
    span::{empty_span, Span, Spanned},
};
use pint_abi_types::{TupleField, TypeABI};

mod display;

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
    Any(Span),
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
    Union {
        decl: UnionKey,
        span: Span,
    },
    Custom {
        name: String,
        span: Span,
    },
    Alias {
        name: String,
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
    ($self: ident, $recurse: ident, $recurse_arg0: expr, $recurse_arg1: expr, $otherwise: expr) => {
        $self
            .is_alias()
            .map(|ty| ty.$recurse($recurse_arg0, $recurse_arg1))
            .unwrap_or_else(|| $otherwise)
    };
}

impl Type {
    pub fn is_any(&self) -> bool {
        matches!(self, Type::Any(_))
    }

    pub fn is_unknown(&self) -> bool {
        matches!(self, Type::Unknown(_))
    }

    pub fn is_error(&self) -> bool {
        matches!(self, Type::Error(_))
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

    pub fn get_custom_name(&self) -> Option<&String> {
        check_alias!(self, get_custom_name, {
            if let Type::Custom { name, .. } = self {
                Some(name)
            } else {
                None
            }
        })
    }

    pub fn is_union(&self) -> bool {
        check_alias!(self, is_union, matches!(self, Type::Union { .. }))
    }

    /// Check if `self` is an "enumeration" union, meaning none of its variants hold values.
    pub fn is_enumeration_union(&self, contract: &Contract) -> bool {
        check_alias!(self, is_enumeration_union, contract, {
            self.get_union_decl(contract)
                .is_some_and(|union| union.is_enumeration_union())
        })
    }

    pub fn get_union_name<'a>(&self, contract: &'a Contract) -> Option<&'a String> {
        check_alias!(self, get_union_name, contract, {
            self.get_union_decl(contract).map(|ud| &ud.name.name)
        })
    }

    fn get_union_decl<'a>(&self, contract: &'a Contract) -> Option<&'a UnionDecl> {
        if let Type::Union { decl, .. } = self {
            contract.unions.get(*decl)
        } else {
            None
        }
    }

    // This is a little wacky.  It returns Err if the union or variant is unknown, and returns
    // Ok(Some(ty)) if the union variant has a binding, else Ok(None).
    pub fn get_union_variant_ty<'a>(
        &self,
        contract: &'a Contract,
        variant_name: &String,
    ) -> Result<Option<&'a Type>, ()> {
        check_alias!(self, get_union_variant_ty, contract, variant_name, {
            self.get_union_decl(contract).ok_or(()).and_then(
                |UnionDecl {
                     name: union_name,
                     variants,
                     ..
                 }| {
                    // The name prefix has to match the union name first, and it has to have the
                    // '::' separator.
                    let ul = union_name.name.len();
                    if variant_name.len() > ul + 2
                        && variant_name.starts_with(&union_name.name)
                        && &variant_name[ul..(ul + 2)] == "::"
                    {
                        // Then we compare each variant with the rest of the variant name.
                        variants
                            .iter()
                            .find_map(
                                |UnionVariant {
                                     variant_name: union_variant_name,
                                     ty,
                                     ..
                                 }| {
                                    (union_variant_name.name == variant_name[(ul + 2)..])
                                        .then_some(ty.as_ref())
                                },
                            )
                            .ok_or(())
                    } else {
                        // Variant not found.
                        Err(())
                    }
                },
            )
        })
    }

    pub fn get_union_variant_names(&self, contract: &Contract) -> Vec<String> {
        check_alias!(self, get_union_variant_names, contract, {
            self.get_union_decl(contract)
                .map(
                    |UnionDecl {
                         name: union_name,
                         variants,
                         ..
                     }| {
                        variants
                            .iter()
                            .map(|UnionVariant { variant_name, .. }| {
                                union_name.name[2..].to_string() + "::" + variant_name.name.as_str()
                            })
                            .collect()
                    },
                )
                .unwrap_or_default()
        })
    }

    pub fn get_union_variant_types(&self, contract: &Contract) -> Vec<Option<Type>> {
        check_alias!(self, get_union_variant_types, contract, {
            self.get_union_decl(contract)
                .map(|UnionDecl { variants, .. }| {
                    variants
                        .iter()
                        .map(|UnionVariant { ty, .. }| ty.clone())
                        .collect()
                })
                .unwrap_or_default()
        })
    }

    pub fn get_union_variant_count(&self, contract: &Contract) -> Option<usize> {
        check_alias!(self, get_union_variant_count, contract, {
            self.get_union_decl(contract)
                .map(|UnionDecl { variants, .. }| variants.len())
        })
    }

    pub fn get_union_variant_as_num(&self, contract: &Contract, tag: &str) -> Option<usize> {
        self.get_union_variant_names(contract)
            .into_iter()
            .enumerate()
            .find_map(|(idx, variant_name)| (variant_name == tag[2..]).then_some(idx))
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

    /// Returns all array range expressions in given type. For example, given the following
    /// expression `{ int, int[3 + 3], int[a][b] }`, this method a vector of `ExprKey` that point
    /// to the following expressions `3 + 3`, `a`, `b`.
    pub fn get_all_array_range_exprs(&self) -> Vec<ExprKey> {
        let mut range_exprs = Vec::new();
        match self {
            Self::Array { ty, range, .. } => {
                range_exprs.extend(ty.get_all_array_range_exprs());
                if let Some(range) = range {
                    range_exprs.push(*range);
                }
            }
            Self::Tuple { fields, .. } => {
                for field in fields {
                    range_exprs.extend(field.1.get_all_array_range_exprs());
                }
            }
            Self::Alias { ty, .. } => {
                range_exprs.extend(ty.get_all_array_range_exprs());
            }
            Self::Map { ty_from, ty_to, .. } => {
                range_exprs.extend(ty_from.get_all_array_range_exprs());
                range_exprs.extend(ty_to.get_all_array_range_exprs());
            }
            Self::Vector { ty, .. } => {
                range_exprs.extend(ty.get_all_array_range_exprs());
            }
            _ => {}
        }

        range_exprs
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
        // TODO: REMOVE THIS.  WE'RE LOWERING IN A PASS.
        if let Expr::Path(path, _) = range_expr {
            // It's hopefully an enumeration union for the range expression.
            if let Some(size) = contract.unions.iter().find_map(|(_key, union_decl)| {
                (union_decl.is_enumeration_union() && &union_decl.name.name == path)
                    .then_some(union_decl.variants.len() as i64)
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
            match Evaluator::new(contract).evaluate(range_expr, handler, contract) {
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

    // If `self` contains a storage only type `ty`, return `Some(ty)`. Otherwise, return `None`
    pub fn get_storage_only_ty(&self, contract: &Contract) -> Option<Self> {
        match self {
            Type::Map { .. } | Type::Vector { .. } => Some(self.clone()),
            Type::Array { ty, .. } => ty.get_storage_only_ty(contract),
            Type::Tuple { fields, .. } => {
                for (_, field) in fields {
                    let ty = field.get_storage_only_ty(contract);
                    if let Some(ty) = ty {
                        return Some(ty);
                    }
                }
                None
            }
            Type::Alias { ty, .. } => ty.get_storage_only_ty(contract),
            Type::Union { decl, .. } => {
                let union_decl = contract
                    .unions
                    .get(*decl)
                    .expect("union decl guaranteed to exist");
                for variant in &union_decl.variants {
                    if let Some(variant_ty) = &variant.ty {
                        if let Some(ty) = variant_ty.get_storage_only_ty(contract) {
                            return Some(ty);
                        }
                    }
                }
                None
            }
            _ => None,
        }
    }

    // Checks if type `self` is allowed in storage. For now, all types are allowed except for:
    // - Storage maps where the "from" type is not bool, int, nor b256
    // - Storage vectors where the element type is not bool, int, nor b256
    pub fn is_allowed_in_storage(&self, contract: &Contract) -> bool {
        match self {
            Type::Map { ty_from, ty_to, .. } => {
                ty_from.get_storage_only_ty(contract).is_none()
                    && ty_to.is_allowed_in_storage(contract)
            }
            Type::Vector { ty, .. } => {
                // We only support vectors of these types for now
                ty.is_bool() || ty.is_int() || ty.is_b256()
            }
            Type::Array { ty, .. } => ty.is_allowed_in_storage(contract),
            Type::Tuple { fields, .. } => fields.iter().fold(true, |acc, (_, field)| {
                acc && field.is_allowed_in_storage(contract)
            }),
            Type::Alias { ty, .. } => ty.is_allowed_in_storage(contract),
            Type::Custom { .. } => {
                // Disallow custom types since they're ambiguous. Hopefully, by the time we need
                // this method, all custom types are gone.
                false
            }
            _ => true,
        }
    }

    pub fn size(&self, handler: &Handler, contract: &Contract) -> Result<usize, ErrorEmitted> {
        match self {
            Self::Primitive {
                kind: PrimitiveKind::Bool | PrimitiveKind::Int,
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
                * size.unwrap_or(Self::get_array_size_from_range_expr(
                    handler,
                    range
                        .as_ref()
                        .and_then(|e| e.try_get(contract))
                        .expect("expr key guaranteed to exist"),
                    contract,
                )?) as usize),

            Self::Union { decl, .. } => {
                let UnionDecl { variants, .. } = &contract.unions[*decl];

                let mut max_variant_size = 0;
                for variant in variants {
                    if let Some(ty) = &variant.ty {
                        max_variant_size =
                            std::cmp::max(max_variant_size, ty.size(handler, contract)?);
                    }
                }

                // Add 1 for the tag.
                Ok(max_variant_size + 1)
            }

            // The point here is that a `Map` takes up a storage slot, even though it doesn't
            // actually store anything in it. The `Map` type is not really allowed anywhere else,
            // so we can't have a predicate parameter of type `Map` for example.
            Self::Map { .. } => Ok(1),

            // `Vector` also takes up a single storage slot that stores the length of the vector
            Self::Vector { .. } => Ok(1),

            // Not expecting any of these types at this stage. These are either unsupported types
            // (like `String` and `Real`) or types that should have been resolved by the time we
            // need their size (like `Custom` and `Alias`)
            Self::Primitive {
                kind: PrimitiveKind::String | PrimitiveKind::Real | PrimitiveKind::Nil,
                span,
            }
            | Self::Error(span)
            | Self::Unknown(span)
            | Self::Any(span)
            | Self::Custom { span, .. }
            | Self::Alias { span, .. } => {
                Err(handler.emit_internal_err("unexpected type when getting size", span.clone()))
            }
        }
    }

    /// Calculate the number of storage keys required for this type. All primitive types and union
    /// types require a single key even if their size is > 1.
    pub fn storage_keys(
        &self,
        handler: &Handler,
        contract: &Contract,
    ) -> Result<usize, ErrorEmitted> {
        match self {
            Self::Primitive {
                kind: PrimitiveKind::Bool | PrimitiveKind::Int | PrimitiveKind::B256,
                ..
            } => Ok(1),

            Self::Tuple { fields, .. } => fields.iter().try_fold(0, |acc, (_, field_ty)| {
                field_ty
                    .storage_keys(handler, contract)
                    .map(|slots| acc + slots)
            }),

            Self::Array {
                ty, range, size, ..
            } => Ok(ty.storage_keys(handler, contract)?
                * size.unwrap_or(Self::get_array_size_from_range_expr(
                    handler,
                    range
                        .as_ref()
                        .and_then(|e| e.try_get(contract))
                        .expect("expr key guaranteed to exist"),
                    contract,
                )?) as usize),

            // Unions fit in a single slot since we can't access within a union without a `match`
            // first. So, might as well store the whole thing in a single key
            Self::Union { .. } => Ok(1),

            // The point here is that a `Map` takes up a storage slot, even though it doesn't
            // actually store anything in it. The `Map` type is not really allowed anywhere else,
            // so we can't have a predicate parameter of type `Map` for example.
            Self::Map { .. } => Ok(1),

            // `Vector` also takes up a single storage slot that stores the length of the vector
            Self::Vector { .. } => Ok(1),

            // Not expecting any of these types at this stage. These are either unsupported types
            // (like `String` and `Real`) or types that should have been resolved by the time we
            // need their size (like `Custom` and `Alias`)
            Self::Primitive {
                kind: PrimitiveKind::String | PrimitiveKind::Real | PrimitiveKind::Nil,
                span,
            }
            | Self::Error(span)
            | Self::Unknown(span)
            | Self::Any(span)
            | Self::Custom { span, .. }
            | Self::Alias { span, .. } => Err(handler.emit_internal_err(
                "unexpected type when calculating storage slots",
                span.clone(),
            )),
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

            Type::Union { decl, .. } => Ok(TypeABI::Union {
                name: contract.unions[*decl].name.name.clone(),
                variants: self
                    .get_union_variant_names(contract)
                    .into_iter()
                    .zip(self.get_union_variant_types(contract))
                    .map(|(name, ty)| {
                        Ok(pint_abi_types::UnionVariant {
                            name: name.to_string(),
                            ty: ty
                                .as_ref()
                                .map(|ty| ty.abi(handler, contract))
                                .transpose()?,
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            }),

            Type::Map { ty_from, ty_to, .. } => Ok(TypeABI::Map {
                ty_from: Box::new((*ty_from).abi(handler, contract)?),
                ty_to: Box::new((*ty_to).abi(handler, contract)?),
            }),

            // This, of course, is incorrect. It's just a placeholder until we can support ABI gen
            // for vectors, which is non-trivial.
            Type::Vector { .. } => Ok(TypeABI::Int),

            _ => unimplemented!("other types are not yet supported"),
        }
    }

    pub fn eq(&self, contract: &Contract, other: &Self) -> bool {
        match (self, other) {
            (Self::Error(_), Self::Error(_)) => true,
            (Self::Unknown(_), Self::Unknown(_)) => true,

            // Type::Any is equal to anything!
            (Self::Any(_), _) => true,
            (_, Self::Any(_)) => true,

            (Self::Alias { ty: lhs_ty, .. }, rhs) => lhs_ty.eq(contract, rhs),
            (lhs, Self::Alias { ty: rhs_ty, .. }) => lhs.eq(contract, rhs_ty.as_ref()),

            (Self::Primitive { kind: lhs, .. }, Self::Primitive { kind: rhs, .. }) => lhs == rhs,

            // This is sub-optimal; we're saying two arrays of the same element type are
            // equivalent, regardless of their size.
            (Self::Array { ty: lhs_ty, .. }, Self::Array { ty: rhs_ty, .. }) => {
                lhs_ty.eq(contract, rhs_ty)
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
                                .map(|lhs_ty| lhs_ty.eq(contract, rhs_ty))
                                .unwrap_or(false)
                        })
                    } else {
                        // Otherwise we compare them in declared order.
                        lhs_fields
                            .iter()
                            .zip(rhs_fields.iter())
                            .all(|((_, lhs_ty), (_, rhs_ty))| lhs_ty.eq(contract, rhs_ty))
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
            ) => lhs_ty_from.eq(contract, rhs_ty_from) && lhs_ty_to.eq(contract, rhs_ty_to),

            (Self::Vector { ty: lhs_ty, .. }, Self::Vector { ty: rhs_ty, .. }) => {
                lhs_ty.eq(contract, rhs_ty)
            }

            (Self::Union { decl: lhs_decl, .. }, Self::Union { decl: rhs_decl, .. }) => {
                lhs_decl == rhs_decl
            }

            // TODO: remove Type::Custom as the very first thing we do so we never need to compare
            // them.  We probably wouldn't need `contract` passed then.
            (lhs_ty, rhs_ty) => {
                // Custom types are tricky as they may be either aliases or unions. Or, at this
                // stage, we might just have two different types.
                let mut lhs_alias_ty = None;
                let mut lhs_custom_path = None;

                if let Self::Custom { name: lhs_path, .. } = lhs_ty {
                    lhs_alias_ty =
                        contract
                            .new_types
                            .iter()
                            .find_map(|NewTypeDecl { name, ty, .. }| {
                                (lhs_path == &name.name).then_some(ty)
                            });
                    lhs_custom_path = Some(lhs_path);
                }

                if let Some(lhs_alias_ty) = lhs_alias_ty {
                    // The LHS is an alias; recurse.
                    return lhs_alias_ty.eq(contract, rhs_ty);
                }

                let mut rhs_alias_ty = None;
                let mut rhs_custom_path = None;

                if let Self::Custom { name: rhs_path, .. } = rhs_ty {
                    rhs_alias_ty =
                        contract
                            .new_types
                            .iter()
                            .find_map(|NewTypeDecl { name, ty, .. }| {
                                (rhs_path == &name.name).then_some(ty)
                            });
                    rhs_custom_path = Some(rhs_path);
                }

                if let Some(rhs_alias_ty) = rhs_alias_ty {
                    // The RHS is an alias; recurse.
                    return rhs_alias_ty.eq(contract, lhs_ty);
                }

                match (lhs_custom_path, rhs_custom_path) {
                    (Some(lhs_custom_path), Some(rhs_custom_path)) => {
                        // Neither are aliases but both are custom types
                        lhs_custom_path == rhs_custom_path
                    }

                    (Some(lhs_custom_path), None) => {
                        if let Type::Union { decl, .. } = rhs_ty {
                            // The LHS is a Custom and the RHS is a Union.  Same path?
                            &contract.unions[*decl].name.name == lhs_custom_path
                        } else {
                            false
                        }
                    }

                    (None, Some(rhs_custom_path)) => {
                        if let Type::Union { decl, .. } = lhs_ty {
                            // The LHS is a Union and the RHS is a Custom.  Same path?
                            &contract.unions[*decl].name.name == rhs_custom_path
                        } else {
                            false
                        }
                    }

                    // OK, they must just be different types.
                    _ => false,
                }
            }
        }
    }

    pub fn replace_type_expr(&mut self, old_expr: ExprKey, new_expr: ExprKey) {
        match self {
            Type::Array { ty, range, .. } => {
                // Arrays are the only type which have an expr key.
                if let Some(range) = range {
                    if *range == old_expr {
                        *range = new_expr;
                    }
                }

                ty.replace_type_expr(old_expr, new_expr);
            }

            Type::Tuple { fields, .. } => {
                fields
                    .iter_mut()
                    .for_each(|(_, field_ty)| field_ty.replace_type_expr(old_expr, new_expr));
            }

            Type::Alias { ty, .. } => ty.replace_type_expr(old_expr, new_expr),

            Type::Map { ty_from, ty_to, .. } => {
                ty_from.replace_type_expr(old_expr, new_expr);
                ty_to.replace_type_expr(old_expr, new_expr);
            }

            Type::Vector { ty, .. } => ty.replace_type_expr(old_expr, new_expr),

            Type::Error(_)
            | Type::Unknown(_)
            | Type::Any(_)
            | Type::Primitive { .. }
            | Type::Custom { .. }
            | Type::Union { .. } => {}
        }
    }
}

impl Spanned for Type {
    fn span(&self) -> &Span {
        use Type::*;
        match &self {
            Error(span)
            | Unknown(span)
            | Any(span)
            | Primitive { span, .. }
            | Array { span, .. }
            | Tuple { span, .. }
            | Union { span, .. }
            | Custom { span, .. }
            | Alias { span, .. }
            | Map { span, .. }
            | Vector { span, .. } => span,
        }
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
pub struct UnionDecl {
    pub(super) name: Ident,
    pub(super) variants: Vec<UnionVariant>,
    pub(super) span: Span,
}

impl UnionDecl {
    /// Check if `self` is an "enumeration" union, meaning none of its variants hold values.
    pub fn is_enumeration_union(&self) -> bool {
        self.variants.iter().all(|v| v.ty.is_none())
    }
}

#[derive(Clone, Debug)]
pub struct UnionVariant {
    pub(super) variant_name: Ident,
    pub(super) ty: Option<Type>,
}

impl Spanned for UnionDecl {
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

////////////////////////////////////////////////////////////////
// Helper functions that produce specific types without spans //
////////////////////////////////////////////////////////////////

pub fn error() -> Type {
    Type::Error(empty_span())
}

pub fn any() -> Type {
    Type::Any(empty_span())
}

pub fn r#bool() -> Type {
    Type::Primitive {
        kind: PrimitiveKind::Bool,
        span: empty_span(),
    }
}

pub fn int() -> Type {
    Type::Primitive {
        kind: PrimitiveKind::Int,
        span: empty_span(),
    }
}

pub fn b256() -> Type {
    Type::Primitive {
        kind: PrimitiveKind::B256,
        span: empty_span(),
    }
}

pub fn string() -> Type {
    Type::Primitive {
        kind: PrimitiveKind::String,
        span: empty_span(),
    }
}

pub fn tuple(fields: Vec<Type>) -> Type {
    Type::Tuple {
        fields: fields.into_iter().map(|ty| (None, ty)).collect::<Vec<_>>(),
        span: empty_span(),
    }
}

pub fn vector(ty: Type) -> Type {
    Type::Vector {
        ty: Box::new(ty),
        span: empty_span(),
    }
}
