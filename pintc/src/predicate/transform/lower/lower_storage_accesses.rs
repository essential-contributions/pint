use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::{BinaryOp, Expr, Ident, Immediate, TupleAccess},
    predicate::{ConstraintDecl, Contract, ExprKey, PredKey},
    span::empty_span,
    types::{PrimitiveKind, Type},
};

pub(crate) fn lower_storage_accesses(
    handler: &Handler,
    contract: &mut Contract,
) -> Result<(), ErrorEmitted> {
    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        lower_storage_accesses_in_predicate(handler, contract, pred_key)?;
    }

    Ok(())
}

fn lower_storage_accesses_in_predicate(
    handler: &Handler,
    contract: &mut Contract,
    pred_key: PredKey,
) -> Result<(), ErrorEmitted> {
    let state_exprs = contract
        .preds
        .get(pred_key)
        .map(|pred| {
            pred.states()
                .map(|(_, state)| state.expr)
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();

    let int_ty = Type::Primitive {
        kind: PrimitiveKind::Int,
        span: empty_span(),
    };

    let bool_ty = Type::Primitive {
        kind: PrimitiveKind::Bool,
        span: empty_span(),
    };

    let int = |i: i64, contract: &mut Contract| {
        contract.exprs.insert(
            Expr::Immediate {
                value: Immediate::Int(i),
                span: empty_span(),
            },
            int_ty.clone(),
        )
    };

    let mut ty_fields = vec![];
    let mut fields = vec![];
    let mut total_size = 0;
    for expr in state_exprs {
        let expr_ty = expr.get_ty(contract).clone();
        let (addr, mutable, key) = get_base_storage_key(handler, &expr, contract, pred_key)?;

        let tuple_ty = Type::Tuple {
            fields: key
                .iter()
                .map(|k| (None, k.get_ty(contract).clone()))
                .collect::<Vec<_>>(),
            span: empty_span(),
        };

        // Now create the actual type expression that contains all the key elements.
        let tuple = Expr::Tuple {
            fields: key.iter().map(|k| (None, *k)).collect::<Vec<_>>(),
            span: empty_span(),
        };

        // Insert the tuple into the `exprs` slotmap
        let tuple_key = contract.exprs.insert(tuple, tuple_ty.clone());

        if mutable {
            ty_fields.push((None, tuple_ty.clone()));
            fields.push((None, tuple_key));

            ty_fields.push((None, int_ty.clone()));
            let key_size = tuple_ty.size(handler, contract)?;
            fields.push((None, int(key_size as i64, contract)));

            total_size += 1 + key_size;

            for idx in 1..expr_ty.storage_or_transient_slots(handler, contract)? {
                let mut key = key.clone();
                let tuple_ty = Type::Tuple {
                    fields: key
                        .iter()
                        .map(|k| (None, k.get_ty(contract).clone()))
                        .collect::<Vec<_>>(),
                    span: empty_span(),
                };

                // Now create the actual type expression that contains all the key elements.
                let idx = int(idx as i64, contract);
                let add = contract.exprs.insert(
                    Expr::BinaryOp {
                        op: BinaryOp::Add,
                        lhs: key[key.len() - 1], // TODO: fix
                        rhs: idx,
                        span: empty_span(),
                    },
                    int_ty.clone(),
                );
                key.pop();
                key.push(add);

                let tuple = Expr::Tuple {
                    fields: key.iter().map(|k| (None, *k)).collect::<Vec<_>>(),
                    span: empty_span(),
                };

                // Insert the tuple into the `exprs` slotmap
                let tuple_key = contract.exprs.insert(tuple, tuple_ty.clone());

                ty_fields.push((None, tuple_ty.clone()));
                fields.push((None, tuple_key));

                ty_fields.push((None, int_ty.clone()));
                let key_size = tuple_ty.size(handler, contract)?;
                fields.push((None, int(key_size as i64, contract)));

                total_size += 1 + key_size;
            }
        }

        if !key.is_empty() {
            let storage_get_intrinsic = contract.exprs.insert(
                if let Some(addr) = addr {
                    Expr::IntrinsicCall {
                        name: Ident {
                            name: "__storage_get_extern".to_string(),
                            hygienic: false,
                            span: empty_span(),
                        },
                        args: vec![addr, tuple_key],
                        span: empty_span(),
                    }
                } else {
                    Expr::IntrinsicCall {
                        name: Ident {
                            name: "__storage_get".to_string(),
                            hygienic: false,
                            span: empty_span(),
                        },
                        args: vec![tuple_key],
                        span: empty_span(),
                    }
                },
                expr_ty.clone(),
            );

            contract.replace_exprs(Some(pred_key), expr, storage_get_intrinsic);
        }
    }

    let mut_keys_intrinsic = contract.exprs.insert(
        Expr::IntrinsicCall {
            name: Ident {
                name: "__mut_keys".to_string(),
                hygienic: false,
                span: empty_span(),
            },
            args: vec![],
            span: empty_span(),
        },
        Type::Tuple {
            fields: vec![],
            span: empty_span(),
        },
    );

    ty_fields.push((None, int_ty.clone()));
    fields.push((None, int(total_size as i64, contract)));

    let tuple_ty = Type::Tuple {
        fields: ty_fields,
        span: empty_span(),
    };

    // Now create the actual type expression that contains all the key elements.
    let tuple = Expr::Tuple {
        fields,
        span: empty_span(),
    };

    // Insert the tuple into the `exprs` slotmap
    let tuple_key = contract.exprs.insert(tuple, tuple_ty.clone());

    let eq_set_intrinsic = contract.exprs.insert(
        Expr::IntrinsicCall {
            name: Ident {
                name: "__eq_set".to_string(),
                hygienic: false,
                span: empty_span(),
            },
            args: vec![mut_keys_intrinsic, tuple_key],
            span: empty_span(),
        },
        bool_ty,
    );

    if let Some(pred) = contract.preds.get_mut(pred_key) {
        pred.constraints.push(ConstraintDecl {
            expr: eq_set_intrinsic,
            span: empty_span(),
        });
    }

    Ok(())
}

fn get_base_storage_key(
    handler: &Handler,
    expr: &ExprKey,
    contract: &mut Contract,
    pred_key: PredKey,
) -> Result<(Option<ExprKey>, bool, Vec<ExprKey>), ErrorEmitted> {
    let int_ty = Type::Primitive {
        kind: PrimitiveKind::Int,
        span: empty_span(),
    };

    let int = |i: i64, contract: &mut Contract| {
        contract.exprs.insert(
            Expr::Immediate {
                value: Immediate::Int(i),
                span: empty_span(),
            },
            int_ty.clone(),
        )
    };

    let expr_ty = expr.get_ty(contract).clone();
    match &expr.get(contract).clone() {
        Expr::IntrinsicCall { name, args, .. } => {
            if name.name == "__vec_len" {
                assert_eq!(args.len(), 1);
                match args[0].try_get(contract) {
                    Some(Expr::StorageAccess { name, .. }) => {
                        if !contract.storage_var(name).1.ty.is_vector() {
                            return Err(handler.emit_err(Error::Compile {
                                error: CompileError::Internal {
                                    msg: "argument to __state_len must be a storage vector",
                                    span: empty_span(),
                                },
                            }));
                        }
                    }
                    Some(Expr::ExternalStorageAccess {
                        interface_instance,
                        name,
                        ..
                    }) => {
                        let pred = contract.preds.get(pred_key).unwrap();
                        let interface_instance = &pred
                            .interface_instances
                            .iter()
                            .find(|e| e.name.to_string() == *interface_instance)
                            .expect("missing interface instance");

                        if !contract
                            .external_storage_var(&interface_instance.interface, name)
                            .1
                            .ty
                            .is_vector()
                        {
                            return Err(handler.emit_err(Error::Compile {
                                error: CompileError::Internal {
                                    msg: "argument to __state_len must be a storage vector",
                                    span: empty_span(),
                                },
                            }));
                        }
                    }
                    _ => {
                        return Err(handler.emit_err(Error::Compile {
                            error: CompileError::Internal {
                                msg: "argument to __state_len must be a storage vector",
                                span: empty_span(),
                            },
                        }));
                    }
                };

                get_base_storage_key(handler, &args[0], contract, pred_key)
            } else {
                Err(handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "unrecognized storage intrinsic",
                        span: empty_span(),
                    },
                }))
            }
        }
        Expr::StorageAccess { name, mutable, .. } => {
            let (storage_index, storage_var) = contract.storage_var(name);

            Ok((
                None,
                *mutable,
                if storage_var.ty.is_any_primitive()
                    || storage_var.ty.is_map()
                    || storage_var.ty.is_vector()
                {
                    vec![int(storage_index as i64, contract)]
                } else {
                    vec![int(storage_index as i64, contract), int(0, contract)]
                },
            ))
        }

        Expr::ExternalStorageAccess {
            interface_instance,
            name,
            ..
        } => {
            // Get the `interface_instance` declaration that the storage access refers to
            let pred = contract.preds.get(pred_key).unwrap();
            let interface_instance = &pred
                .interface_instances
                .iter()
                .find(|e| e.name.to_string() == *interface_instance)
                .expect("missing interface instance");

            let (storage_index, storage_var) =
                contract.external_storage_var(&interface_instance.interface, name);

            // This is the key. It's either the `storage_index` if the storage type primitive
            // or a map, or it's `[storage_index, 0]`. The `0` here is a placeholder for
            // offsets.
            Ok((
                Some(interface_instance.address),
                false,
                if storage_var.ty.is_any_primitive()
                    || storage_var.ty.is_map()
                    || storage_var.ty.is_vector()
                {
                    vec![int(storage_index as i64, contract)]
                } else {
                    vec![int(storage_index as i64, contract), int(0, contract)]
                },
            ))
        }

        Expr::Index { expr, index, .. } => {
            let inner_expr_ty = expr.get_ty(contract).clone();
            let (addr, mutable, mut key) = get_base_storage_key(handler, expr, contract, pred_key)?;
            if inner_expr_ty.is_map() || inner_expr_ty.is_vector() {
                key.push(*index);
                if !(expr_ty.is_any_primitive() || expr_ty.is_map() || expr_ty.is_vector()) {
                    // Placeholder zero for offsets
                    key.push(int(0, contract));
                }
            } else {
                let Type::Array { ty, .. } = expr.get_ty(contract) else {
                    return Err(handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "type must exist and be an array type",
                            span: empty_span(),
                        },
                    }));
                };

                if let Some(last) = key.last_mut() {
                    let el_size = ty.storage_or_transient_slots(handler, contract)?;
                    let el_size = int(el_size as i64, contract);
                    let mul = contract.exprs.insert(
                        Expr::BinaryOp {
                            op: BinaryOp::Mul,
                            lhs: *index,
                            rhs: el_size,
                            span: empty_span(),
                        },
                        int_ty.clone(),
                    );
                    let add = contract.exprs.insert(
                        Expr::BinaryOp {
                            op: BinaryOp::Add,
                            lhs: *last,
                            rhs: mul,
                            span: empty_span(),
                        },
                        int_ty.clone(),
                    );
                    *last = add;
                }
            }
            Ok((addr, mutable, key))
        }

        Expr::TupleFieldAccess { tuple, field, .. } => {
            let (addr, mutable, mut key) =
                get_base_storage_key(handler, tuple, contract, pred_key)?;

            // Grab the fields of the tuple
            let Type::Tuple { ref fields, .. } = tuple.get_ty(contract) else {
                return Err(handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "type must exist and be a tuple type",
                        span: empty_span(),
                    },
                }));
            };

            // The field index is based on the type definition
            let field_idx = match field {
                TupleAccess::Index(idx) => *idx,
                TupleAccess::Name(ident) => fields
                    .iter()
                    .position(|(field_name, _)| {
                        field_name
                            .as_ref()
                            .map_or(false, |name| name.name == ident.name)
                    })
                    .expect("field name must exist, this was checked in type checking"),
                TupleAccess::Error => {
                    return Err(handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "unexpected TupleAccess::Error",
                            span: empty_span(),
                        },
                    }))
                }
            };

            // This is the offset from the base key where the full tuple is stored.
            let offset: usize = fields.iter().take(field_idx).try_fold(0, |acc, (_, ty)| {
                ty.storage_or_transient_slots(handler, contract)
                    .map(|slots| acc + slots)
            })?;

            if let Some(last) = key.last_mut() {
                let offset = int(offset as i64, contract);
                let add = contract.exprs.insert(
                    Expr::BinaryOp {
                        op: BinaryOp::Add,
                        lhs: *last,
                        rhs: offset,
                        span: empty_span(),
                    },
                    int_ty.clone(),
                );
                *last = add;
            }
            Ok((addr, mutable, key))
        }

        _ => todo!(),
    }
}
