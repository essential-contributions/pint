use crate::{
    error::{ErrorEmitted, Handler},
    expr::{
        BinaryOp, Expr, ExternalIntrinsic, InternalIntrinsic, IntrinsicKind, TupleAccess, UnaryOp,
    },
    predicate::{ConstraintDecl, Contract, ExprKey, PredKey},
    span::empty_span,
    types::{int, r#bool, Type},
};
use fxhash::FxHashSet;

/// Lower all storage accesses in a contract into `__storage_get` and `__storage_get_extern`
/// intrinsics. Also insert constraints on mutable keys.
pub(crate) fn lower_storage_accesses(
    handler: &Handler,
    contract: &mut Contract,
) -> Result<(), ErrorEmitted> {
    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        lower_storage_accesses_in_predicate(handler, contract, pred_key)?;
    }

    Ok(())
}

/// Lower all storage accesses in a predicate into `__storage_get` and `__storage_get_extern`
/// intrinsics. Also insert a constraint on the mutable keys in the predicate.
fn lower_storage_accesses_in_predicate(
    handler: &Handler,
    contract: &mut Contract,
    pred_key: PredKey,
) -> Result<(), ErrorEmitted> {
    let storage_accesses: FxHashSet<_> = contract
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
                .flat_map(|expr| expr.collect_storage_accesses(contract))
                .collect::<FxHashSet<_>>()
        })
        .unwrap_or_default();

    let mut keys_set_field_types = vec![];
    let mut keys_set_fields = vec![];
    let mut keys_set_size = 0;

    for expr in storage_accesses {
        let expr_ty = expr.get_ty(contract).clone();
        let (addr, mutable, next_state, key) = get_base_storage_key(handler, &expr, contract)?;

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
        let storage_get_intrinsic = contract.exprs.insert(
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
        contract.replace_exprs(Some(pred_key), expr, storage_get_intrinsic);

        // Now, if this key is mutable, then collect it along with the _next_ few keys. The number
        // of the keys to collect here is equal to the number of stoage slots that `expr_ty`
        // requires.
        if mutable {
            // Extract the wrapped type
            let num_keys = if let Some(ty) = expr_ty.get_optional_ty() {
                ty.storage_keys(handler, contract)?
            } else {
                return Err(handler
                    .emit_internal_err("storage accesses must be of type optional", empty_span()));
            };

            // Push the base key and its type
            keys_set_field_types.push((None, key_ty.clone()));
            keys_set_fields.push((None, key_expr));

            // Push the key size and `Type::Int`
            keys_set_field_types.push((None, int()));
            let key_size = key_ty.size(handler, contract)?;
            keys_set_fields.push((None, contract.exprs.insert_int(key_size as i64)));

            // Total size of the set of keys so far
            keys_set_size += 1 + key_size;

            for idx in 1..num_keys {
                let mut next_key = key.clone();
                let next_key_ty = Type::Tuple {
                    fields: next_key
                        .iter()
                        .map(|k| (None, k.get_ty(contract).clone()))
                        .collect::<Vec<_>>(),
                    span: empty_span(),
                };

                if let Some(last) = next_key.last_mut() {
                    // Now create the actual type expression that contains all the key elements.
                    let idx = contract.exprs.insert_int(idx as i64);
                    let add = contract.exprs.insert(
                        Expr::BinaryOp {
                            op: BinaryOp::Add,
                            lhs: *last,
                            rhs: idx,
                            span: empty_span(),
                        },
                        int(),
                    );
                    *last = add;

                    // Insert the tuple into the `exprs` slotmap
                    let next_key_expr = contract.exprs.insert(
                        Expr::Tuple {
                            fields: next_key.iter().map(|k| (None, *k)).collect::<Vec<_>>(),
                            span: empty_span(),
                        },
                        next_key_ty.clone(),
                    );

                    // Push the next key and its type
                    keys_set_field_types.push((None, next_key_ty.clone()));
                    keys_set_fields.push((None, next_key_expr));

                    // Push the next key size and `Type::Int`
                    keys_set_field_types.push((None, int()));
                    let key_size = next_key_ty.size(handler, contract)?;
                    keys_set_fields.push((None, contract.exprs.insert_int(key_size as i64)));

                    // Total size of the set of keys so far
                    keys_set_size += 1 + key_size;
                }
            }
        }
    }

    keys_set_field_types.push((None, int()));
    keys_set_fields.push((None, contract.exprs.insert_int(keys_set_size as i64)));

    let keys_set_ty = Type::Tuple {
        fields: keys_set_field_types,
        span: empty_span(),
    };

    // Insert the tuple into the `exprs` slotmap
    let key_set_expr = contract.exprs.insert(
        Expr::Tuple {
            fields: keys_set_fields,
            span: empty_span(),
        },
        keys_set_ty.clone(),
    );

    // Insert `__mut_keys` intrinsic to obtain the full set of mutable keys
    let mut_keys_intrinsic = contract.exprs.insert(
        Expr::IntrinsicCall {
            kind: (
                IntrinsicKind::Internal(InternalIntrinsic::MutKeys),
                empty_span(),
            ),
            args: vec![],
            span: empty_span(),
        },
        // The type here is wrong but not relevant. We don't really know what the type is. All we
        // know that we're returning a "set" but we don't have a set type. This is fine for now
        // since it's fully internal to the compiler.
        Type::Tuple {
            fields: vec![],
            span: empty_span(),
        },
    );

    // Now insert a `__eq_set` intrinsic to compare the generated set of keys above and the result
    // of `__mut_keys`
    let eq_set_intrinsic = contract.exprs.insert(
        Expr::IntrinsicCall {
            kind: (
                IntrinsicKind::Internal(InternalIntrinsic::EqSet),
                empty_span(),
            ),
            args: vec![mut_keys_intrinsic, key_set_expr],
            span: empty_span(),
        },
        r#bool(),
    );

    // Finally, insert the `__eq_set` expression above in a new constraint
    if let Some(pred) = contract.preds.get_mut(pred_key) {
        pred.constraints.push(ConstraintDecl {
            expr: eq_set_intrinsic,
            span: empty_span(),
        });
    }

    Ok(())
}

/// Given a predicate in a contract and an `ExprKey`, produce the following:
/// 1. An optional external predicate address, if `ExprKey` is an external storage access.
/// 2. A `bool` indicating whether the key is mutable (i.e. should belong to the set of mutable
///    keys).
/// 3. A `bool` indicating whether the access is a post-state access.
/// 3. The key as a vector of `ExprKey`.
fn get_base_storage_key(
    handler: &Handler,
    expr: &ExprKey,
    contract: &mut Contract,
) -> Result<(Option<ExprKey>, bool, bool, Vec<ExprKey>), ErrorEmitted> {
    let expr_ty = expr.get_ty(contract).clone();
    match &expr.get(contract).clone() {
        Expr::UnaryOp {
            op: UnaryOp::NextState,
            expr,
            ..
        } => get_base_storage_key(handler, expr, contract)
            .map(|(addr, mutable, _, key)| (addr, mutable, true /* post-state */, key)),

        Expr::IntrinsicCall { kind, args, .. } => {
            if let (IntrinsicKind::External(ExternalIntrinsic::VecLen), _) = kind {
                assert_eq!(args.len(), 1);
                match args[0].try_get(contract) {
                    Some(Expr::LocalStorageAccess { name, .. }) => {
                        if !contract.storage_var(name).1.ty.is_vector() {
                            return Err(handler.emit_internal_err(
                                "argument to __vec_len must be of type storage vector",
                                empty_span(),
                            ));
                        }
                    }
                    Some(Expr::ExternalStorageAccess {
                        interface, name, ..
                    }) => {
                        if !contract
                            .external_storage_var(interface, name)
                            .1
                            .ty
                            .is_vector()
                        {
                            return Err(handler.emit_internal_err(
                                "argument to __vec_len must be of type storage vector",
                                empty_span(),
                            ));
                        }
                    }
                    _ => {
                        return Err(handler.emit_internal_err(
                            "argument to __vec_len must be storage access",
                            empty_span(),
                        ));
                    }
                };

                get_base_storage_key(handler, &args[0], contract)
            } else {
                Err(handler.emit_internal_err("Invalid storage intrinsic", empty_span()))
            }
        }
        Expr::LocalStorageAccess { name, mutable, .. } => {
            let (storage_index, storage_var) = contract.storage_var(name);

            Ok((
                None, // local storage
                *mutable,
                false, // pre-state
                if storage_var.ty.is_any_primitive()
                    || storage_var.ty.is_optional()
                    || storage_var.ty.is_union()
                    || storage_var.ty.is_map()
                    || storage_var.ty.is_vector()
                {
                    vec![contract.exprs.insert_int(storage_index as i64)]
                } else {
                    vec![
                        contract.exprs.insert_int(storage_index as i64),
                        contract.exprs.insert_int(0), // placeholder for offsets
                    ]
                },
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
                false, // non-mutable (not owned by this contract)
                false, // pre-state
                if storage_var.ty.is_any_primitive()
                    || storage_var.ty.is_optional()
                    || storage_var.ty.is_union()
                    || storage_var.ty.is_map()
                    || storage_var.ty.is_vector()
                {
                    vec![contract.exprs.insert_int(storage_index as i64)]
                } else {
                    vec![
                        contract.exprs.insert_int(storage_index as i64),
                        contract.exprs.insert_int(0), // placeholder for offsets
                    ]
                },
            ))
        }

        Expr::Index { expr, index, .. } => {
            let (addr, mutable, next_state, mut key) =
                get_base_storage_key(handler, expr, contract)?;

            // Extract the wrapped type
            let Some(inner_expr_ty) = expr.get_ty(contract).get_optional_ty() else {
                return Err(handler
                    .emit_internal_err("storage accesses must be of type optional", empty_span()));
            };

            if inner_expr_ty.is_map() || inner_expr_ty.is_vector() {
                // next key element is the index itself
                key.push(*index);

                // Extract the wrapped type
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
                    || expr_ty.is_vector())
                {
                    key.push(contract.exprs.insert_int(0)); // placeholder for offsets
                }
            } else {
                let Type::FixedArray { ty, .. } = inner_expr_ty else {
                    return Err(handler
                        .emit_internal_err("type must exist and be an array type", empty_span()));
                };

                // Increment the last element of the key by `index * array element size`
                if let Some(last) = key.last_mut() {
                    let el_size = ty.storage_keys(handler, contract)?;
                    let el_size = contract.exprs.insert_int(el_size as i64);
                    let mul = contract.exprs.insert(
                        Expr::BinaryOp {
                            op: BinaryOp::Mul,
                            lhs: *index,
                            rhs: el_size,
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
            Ok((addr, mutable, next_state, key))
        }

        Expr::TupleFieldAccess { tuple, field, .. } => {
            let (addr, mutable, next_state, mut key) =
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
            Ok((addr, mutable, next_state, key))
        }

        _ => Err(handler.emit_internal_err(
            "unexpected expression in a storage access expression",
            empty_span(),
        )),
    }
}
