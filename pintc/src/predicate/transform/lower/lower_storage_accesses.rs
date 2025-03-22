use crate::{
    error::{ErrorEmitted, Handler},
    expr::{BinaryOp, Expr, ExternalIntrinsic, InternalIntrinsic, IntrinsicKind, TupleAccess},
    predicate::{ConstraintDecl, Contract, ExprKey, PredKey},
    span::empty_span,
    types::{int, Type},
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

    for expr in storage_accesses {
        let expr_ty = expr.get_ty(contract).clone();
        let (addr, key) = get_base_storage_key(handler, &expr, contract)?;

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
            if let Some(addr) = addr {
                Expr::IntrinsicCall {
                    kind: (
                        IntrinsicKind::Internal(InternalIntrinsic::StateExtern),
                        empty_span(),
                    ),
                    args: vec![addr, key_expr],
                    span: empty_span(),
                }
            } else {
                Expr::IntrinsicCall {
                    kind: (
                        IntrinsicKind::Internal(InternalIntrinsic::State),
                        empty_span(),
                    ),
                    args: vec![key_expr],
                    span: empty_span(),
                }
            },
            expr_ty.clone(),
        );
        contract.replace_exprs(Some(pred_key), expr, storage_get_intrinsic);
    }

    Ok(())
}

/// Given a predicate in a contract and an `ExprKey`, produce the following:
/// 1. An optional external predicate address, if `ExprKey` is an external storage access.
/// 3. The key as a vector of `ExprKey`.
fn get_base_storage_key(
    handler: &Handler,
    expr: &ExprKey,
    contract: &mut Contract,
) -> Result<(Option<ExprKey>, Vec<ExprKey>), ErrorEmitted> {
    let expr_ty = expr.get_ty(contract).clone();
    match &expr.get(contract).clone() {
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
        Expr::LocalStorageAccess { name, .. } => {
            let (storage_index, storage_var) = contract.storage_var(name);

            Ok((
                None, // local storage
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
            let (addr, mut key) = get_base_storage_key(handler, expr, contract)?;

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
            Ok((addr, key))
        }

        Expr::TupleFieldAccess { tuple, field, .. } => {
            let (addr, mut key) = get_base_storage_key(handler, tuple, contract)?;

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
            Ok((addr, key))
        }

        _ => Err(handler.emit_internal_err(
            "unexpected expression in a storage access expression",
            empty_span(),
        )),
    }
}
