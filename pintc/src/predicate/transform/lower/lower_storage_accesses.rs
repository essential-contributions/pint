use crate::{
    error::{ErrorEmitted, Handler},
    expr::{
        BinaryOp, Expr, ExternalIntrinsic, InternalIntrinsic, IntrinsicKind, TupleAccess, UnaryOp,
    },
    predicate::{ConstraintDecl, Contract, ExprKey, PredKey},
    span::empty_span,
    types::{int, optional, r#bool, Type},
};
use fxhash::FxHashSet;

/// Lower all storage accesses in a contract into `__pre_state`, `__post_state`,
/// `__pre_state_extern` and `__post_state_extern` intrinsics`
pub(crate) fn lower_storage_accesses(
    handler: &Handler,
    contract: &mut Contract,
) -> Result<(), ErrorEmitted> {
    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        lower_storage_accesses_in_predicate(handler, contract, pred_key)?;
    }

    Ok(())
}

/// Lower all storage accesses in a predicate into `__pre_state`, `__post_state`,
/// `__pre_state_extern` and `__post_state_extern` intrinsics`
fn lower_storage_accesses_in_predicate(
    handler: &Handler,
    contract: &mut Contract,
    pred_key: PredKey,
) -> Result<(), ErrorEmitted> {
    let (storage_accesses, write_only_storage_accesses): (FxHashSet<_>, FxHashSet<_>) = contract
        .preds
        .get(pred_key)
        .map(|pred| {
            pred.variables()
                .map(|(_, variable)| variable.expr)
                .chain(
                    pred.constraints
                        .iter()
                        .map(|ConstraintDecl { expr, .. }| *expr),
                )
                .map(|expr| expr.collect_storage_accesses(contract))
                .fold(
                    (FxHashSet::default(), FxHashSet::default()),
                    |(mut acc_sa, mut acc_tw), (sa, tw)| {
                        acc_sa.extend(sa);
                        acc_tw.extend(tw);
                        (acc_sa, acc_tw)
                    },
                )
        })
        .unwrap_or_default();

    for expr in storage_accesses {
        let expr_ty = expr.get_ty(contract).clone();
        let (addr, next_state, key, vec_len_keys) = get_base_storage_key(handler, &expr, contract)?;

        // Type of this key is a tuple of all the elements of this key
        let key_ty = Type::Tuple {
            fields: key
                .iter()
                .map(|k| (None, k.get_ty(contract).clone()))
                .collect::<Vec<_>>(),
            span: empty_span(),
        };

        // Insert a tuple expr containing all the key elements
        let key_expr = contract.exprs.insert(
            Expr::Tuple {
                fields: key.iter().map(|k| (None, *k)).collect::<Vec<_>>(),
                span: empty_span(),
            },
            key_ty.clone(),
        );

        // This is the storage intrinsic we're lowering the storage access to
        let storage_intrinsic = contract.exprs.insert(
            if !next_state {
                if let Some(addr) = addr {
                    Expr::IntrinsicCall {
                        kind: (
                            IntrinsicKind::Internal(InternalIntrinsic::PreStateExtern),
                            empty_span(),
                        ),
                        args: vec![addr, key_expr],
                        span: empty_span(),
                    }
                } else {
                    Expr::IntrinsicCall {
                        kind: (
                            IntrinsicKind::Internal(InternalIntrinsic::PreState),
                            empty_span(),
                        ),
                        args: vec![key_expr],
                        span: empty_span(),
                    }
                }
            } else if let Some(addr) = addr {
                Expr::IntrinsicCall {
                    kind: (
                        IntrinsicKind::Internal(InternalIntrinsic::PostStateExtern),
                        empty_span(),
                    ),
                    args: vec![addr, key_expr],
                    span: empty_span(),
                }
            } else {
                Expr::IntrinsicCall {
                    kind: (
                        IntrinsicKind::Internal(InternalIntrinsic::PostState),
                        empty_span(),
                    ),
                    args: vec![key_expr],
                    span: empty_span(),
                }
            },
            expr_ty.clone(),
        );

        // Legalize accesses to unsized arrays in storage. We do this using a select. For example,
        //
        //    storage::v[x]
        //
        // becomes
        //
        //    x < __post_state({2})! ? __post_state({2, x}) : __panic_if(true)
        //
        // assuming that the storage index for `v` is `2` and so, its length would be stored at
        // `__post_state({2})`
        //
        if !vec_len_keys.is_empty() && !write_only_storage_accesses.contains(&expr) {
            let bool_true_expr = contract.exprs.insert_bool(true);
            let mut condition = bool_true_expr;

            // This actually supports multi-d vectors though those are not really
            // supported in the parser at the moment.
            // TODO: enable multi-d vectors in the parser and test this out properly
            for (index, len_key) in vec_len_keys {
                // Type of this key is a tuple of all the elements of this key
                let len_key_ty = Type::Tuple {
                    fields: len_key
                        .iter()
                        .map(|k| (None, k.get_ty(contract).clone()))
                        .collect::<Vec<_>>(),
                    span: empty_span(),
                };

                // Insert a tuple expr containing all the key elements
                let len_key_expr = contract.exprs.insert(
                    Expr::Tuple {
                        fields: len_key.iter().map(|k| (None, *k)).collect::<Vec<_>>(),
                        span: empty_span(),
                    },
                    len_key_ty.clone(),
                );

                let len_expr = contract.exprs.insert(
                    if !next_state {
                        if let Some(addr) = addr {
                            Expr::IntrinsicCall {
                                kind: (
                                    IntrinsicKind::Internal(InternalIntrinsic::PreStateExtern),
                                    empty_span(),
                                ),
                                args: vec![addr, len_key_expr],
                                span: empty_span(),
                            }
                        } else {
                            Expr::IntrinsicCall {
                                kind: (
                                    IntrinsicKind::Internal(InternalIntrinsic::PreState),
                                    empty_span(),
                                ),
                                args: vec![len_key_expr],
                                span: empty_span(),
                            }
                        }
                    } else if let Some(addr) = addr {
                        Expr::IntrinsicCall {
                            kind: (
                                IntrinsicKind::Internal(InternalIntrinsic::PostStateExtern),
                                empty_span(),
                            ),
                            args: vec![addr, len_key_expr],
                            span: empty_span(),
                        }
                    } else {
                        Expr::IntrinsicCall {
                            kind: (
                                IntrinsicKind::Internal(InternalIntrinsic::PostState),
                                empty_span(),
                            ),
                            args: vec![len_key_expr],
                            span: empty_span(),
                        }
                    },
                    optional(int()),
                );

                let unwrapped_len = contract.exprs.insert(
                    Expr::UnaryOp {
                        op: UnaryOp::Unwrap,
                        expr: len_expr,
                        span: empty_span(),
                    },
                    int(),
                );

                let index_less_than_vec_len = contract.exprs.insert(
                    Expr::BinaryOp {
                        op: BinaryOp::LessThan,
                        lhs: index,
                        rhs: unwrapped_len,
                        span: empty_span(),
                    },
                    r#bool(),
                );

                condition = contract.exprs.insert(
                    Expr::BinaryOp {
                        op: BinaryOp::LogicalAnd,
                        lhs: condition,
                        rhs: index_less_than_vec_len,
                        span: empty_span(),
                    },
                    r#bool(),
                );
            }

            let bool_true_expr = contract.exprs.insert_bool(true);
            let panic_if_expr = contract.exprs.insert(
                Expr::IntrinsicCall {
                    kind: (
                        IntrinsicKind::External(ExternalIntrinsic::PanicIf),
                        empty_span(),
                    ),
                    args: vec![bool_true_expr],
                    span: empty_span(),
                },
                expr_ty.clone(),
            );

            let select_expr = contract.exprs.insert(
                Expr::Select {
                    condition,
                    then_expr: storage_intrinsic,
                    else_expr: panic_if_expr,
                    span: empty_span(),
                },
                expr_ty.clone(),
            );
            contract.replace_exprs(Some(pred_key), expr, select_expr);
        } else {
            contract.replace_exprs(Some(pred_key), expr, storage_intrinsic);
        }
    }

    Ok(())
}

/// Given a predicate in a contract and an `ExprKey`, produce the following:
/// 1. An optional external predicate address, if `ExprKey` is an external storage access.
/// 2. A `bool` indicating whether the access is a post-state access.
/// 3. The key as a vector of `ExprKey`.
/// 4. The key where each unsized array length is stored along with the accessed vector index. This
///    is used to legalize storage vector accesses by checking that each accessed index is within
///    bounds.
///
/// TODO: clean this up a bit. Should avoid `type_complexity` here.
#[allow(clippy::type_complexity)]
fn get_base_storage_key(
    handler: &Handler,
    expr: &ExprKey,
    contract: &mut Contract,
) -> Result<
    (
        Option<ExprKey>,
        bool,
        Vec<ExprKey>,
        Vec<(
            ExprKey,      /* accessed vector index */
            Vec<ExprKey>, /* key */
        )>,
    ),
    ErrorEmitted,
> {
    let expr_ty = expr.get_ty(contract).clone();
    match &expr.get(contract).clone() {
        Expr::UnaryOp {
            op: UnaryOp::NextState,
            expr,
            ..
        } => get_base_storage_key(handler, expr, contract).map(|(addr, _, key, vec_len_keys)| {
            (addr, true /* post-state */, key, vec_len_keys)
        }),

        Expr::IntrinsicCall { kind, args, .. } => {
            if let (IntrinsicKind::External(ExternalIntrinsic::ArrayLen), _) = kind {
                // TODO: add checks an stuff
                get_base_storage_key(handler, &args[0], contract)
            } else {
                Err(handler.emit_internal_err("Invalid storage intrinsic", empty_span()))
            }
        }
        Expr::LocalStorageAccess { name, .. } => {
            let (storage_index, storage_var) = contract.storage_var(name);

            Ok((
                None,  // local storage
                false, // pre-state
                if storage_var.ty.is_any_primitive()
                    || storage_var.ty.is_optional()
                    || storage_var.ty.is_union()
                    || storage_var.ty.is_map()
                    || storage_var.ty.is_unsized_array()
                {
                    vec![contract.exprs.insert_int(storage_index as i64)]
                } else {
                    vec![
                        contract.exprs.insert_int(storage_index as i64),
                        contract.exprs.insert_int(0), // placeholder for offsets
                    ]
                },
                vec![],
            ))
        }

        Expr::ExternalStorageAccess {
            interface,
            address,
            name,
            ..
        } => {
            let (storage_index, storage_var) = contract.external_storage_var(interface, name);

            // This is the key. It's either the `storage_index` if the storage type primitive
            // or a map, or it's `[storage_index, 0]`. The `0` here is a placeholder for
            // offsets.
            Ok((
                Some(*address), // interface instance address
                // mutability is not relevant for external accesses. External keys are
                // constrained by their own contracts.
                false, // pre-state
                if storage_var.ty.is_any_primitive()
                    || storage_var.ty.is_optional()
                    || storage_var.ty.is_union()
                    || storage_var.ty.is_map()
                    || storage_var.ty.is_unsized_array()
                {
                    vec![contract.exprs.insert_int(storage_index as i64)]
                } else {
                    vec![
                        contract.exprs.insert_int(storage_index as i64),
                        contract.exprs.insert_int(0), // placeholder for offsets
                    ]
                },
                vec![],
            ))
        }

        Expr::Index { expr, index, .. } => {
            let (addr, next_state, mut key, mut vec_len_keys) =
                get_base_storage_key(handler, expr, contract)?;

            // Extract the wrapped type
            let Some(inner_expr_ty) = expr.get_ty(contract).get_optional_ty() else {
                return Err(handler
                    .emit_internal_err("storage accesses must be of type optional", empty_span()));
            };

            match inner_expr_ty {
                ty if ty.is_map() || ty.is_unsized_array() => {
                    if ty.is_unsized_array() {
                        vec_len_keys.push((*index, key.clone()));
                    }

                    key.push(*index);

                    let Some(expr_ty) = expr_ty.get_optional_ty() else {
                        return Err(handler.emit_internal_err(
                            "3. storage accesses must be of type optional",
                            empty_span(),
                        ));
                    };

                    if !(expr_ty.is_any_primitive()
                        || expr_ty.is_optional()
                        || expr_ty.is_union()
                        || expr_ty.is_map()
                        || expr_ty.is_unsized_array())
                    {
                        key.push(contract.exprs.insert_int(0)); // placeholder for offsets
                    }
                }

                Type::FixedArray { ty, .. } => {
                    if let Some(last) = key.last_mut() {
                        let el_size = ty.storage_keys(handler, contract)?;
                        let el_size_expr = contract.exprs.insert_int(el_size as i64);
                        let mul = contract.exprs.insert(
                            Expr::BinaryOp {
                                op: BinaryOp::Mul,
                                lhs: *index,
                                rhs: el_size_expr,
                                span: empty_span(),
                            },
                            int(),
                        );
                        let add = contract.exprs.insert(
                            Expr::BinaryOp {
                                op: BinaryOp::Add,
                                lhs: *last,
                                rhs: mul,
                                span: empty_span(),
                            },
                            int(),
                        );
                        *last = add;
                    }
                }

                _ => {
                    return Err(handler
                        .emit_internal_err("type must exist and be an array type", empty_span()));
                }
            }
            Ok((addr, next_state, key, vec_len_keys))
        }

        Expr::TupleFieldAccess { tuple, field, .. } => {
            let (addr, next_state, mut key, vec_len_keys) =
                get_base_storage_key(handler, tuple, contract)?;

            // Extract the wrapped type
            let Some(inner_expr_ty) = tuple.get_ty(contract).get_optional_ty() else {
                return Err(handler
                    .emit_internal_err("storage accesses must be of type optional", empty_span()));
            };

            // Grab the fields of the tuple
            let Type::Tuple { ref fields, .. } = inner_expr_ty else {
                return Err(
                    handler.emit_internal_err("type must exist and be a tuple type", empty_span())
                );
            };

            // The field index is based on the type definition
            let field_idx = match field {
                TupleAccess::Index(idx) => *idx,
                TupleAccess::Name(ident) => fields
                    .iter()
                    .position(|(field_name, _)| {
                        field_name
                            .as_ref()
                            .is_some_and(|name| name.name == ident.name)
                    })
                    .expect("field name must exist, this was checked in type checking"),
                TupleAccess::Error => {
                    return Err(
                        handler.emit_internal_err("unexpected TupleAccess::Error", empty_span())
                    )
                }
            };

            // This is the offset from the base key where the full tuple is stored.
            let offset: usize = fields.iter().take(field_idx).try_fold(0, |acc, (_, ty)| {
                ty.storage_keys(handler, contract).map(|slots| acc + slots)
            })?;

            // Increment the last element of the key by `offset`
            if let Some(last) = key.last_mut() {
                let offset = contract.exprs.insert_int(offset as i64);
                let add = contract.exprs.insert(
                    Expr::BinaryOp {
                        op: BinaryOp::Add,
                        lhs: *last,
                        rhs: offset,
                        span: empty_span(),
                    },
                    int(),
                );
                *last = add;
            }
            Ok((addr, next_state, key, vec_len_keys))
        }

        _ => Err(handler.emit_internal_err(
            "unexpected expression in a storage access expression",
            empty_span(),
        )),
    }
}
