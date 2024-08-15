use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::{BinaryOp, Expr, Ident},
    predicate::{ConstraintDecl, Contract, ExprKey, PredKey, PredicateInstance},
    span::empty_span,
    types::{PrimitiveKind, Type},
};

/// In a given contract, lower all accesses to pub vars into `__transient` intrinsic call. Also,
/// ensure that the addresses used in interface and predicate instances are correct (i.e. match
/// what's in the solution)
pub(crate) fn lower_pub_var_accesses(
    handler: &Handler,
    contract: &mut Contract,
) -> Result<(), ErrorEmitted> {
    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        lower_pub_var_accesses_in_predicate(handler, contract, pred_key)?;
        check_pathway_addresses_in_predicate(handler, contract, pred_key)?;
    }

    Ok(())
}

/// In a given predicate, lower all accesses to pub vars into `__transient` intrinsic call. Also,
/// ensure that the addresses used in interface and predicate instances are correct (i.e. match
/// what's in the solution)
pub(crate) fn lower_pub_var_accesses_in_predicate(
    handler: &Handler,
    contract: &mut Contract,
    pred_key: PredKey,
) -> Result<(), ErrorEmitted> {
    let int_ty = Type::Primitive {
        kind: PrimitiveKind::Int,
        span: empty_span(),
    };
    let zero = contract.exprs.insert_int(0);

    let pred = contract
        .preds
        .get(pred_key)
        .expect("predicate key is expected to exist");

    // Collect the `VarKey`s, `ExprKey`s, and indices of path expressions that correspond to
    // *local* pub vars. The indices we're collecting are the indices in the list of pub vars only,
    // in the order they are declared in the predicate.
    let local_pub_vars = contract
        .exprs(pred_key)
        .filter_map(|expr_key| {
            if let Some(Expr::Path(name, _)) = expr_key.try_get(contract) {
                pred.vars()
                    .filter(|(_, var)| var.is_pub)
                    .enumerate()
                    .find(|(_, (_, var))| &var.name == name)
                    .map(|(pub_var_index, (var_key, _))| (var_key, expr_key, pub_var_index))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    // A list of pub var access expressions to replace with `__transient` intrinsic calls.
    let mut replacements: Vec<(
        ExprKey, /* `pub var` access expr */
        ExprKey, /* `__transient` intrinsic call */
    )> = Vec::new();

    for (var_key, expr_key, pub_var_index) in &local_pub_vars {
        let var_ty = var_key.get_ty(pred);

        // First argument is the key which depends on whether the type of the `pub var` is
        // primitive or not
        let pub_var_index = contract.exprs.insert_int(*pub_var_index as i64);
        let key = contract.exprs.insert(
            Expr::Tuple {
                fields: if var_ty.is_any_primitive() {
                    vec![(None, pub_var_index)]
                } else {
                    vec![(None, pub_var_index), (None, zero)]
                },
                span: empty_span(),
            },
            Type::Tuple {
                fields: if var_ty.is_any_primitive() {
                    vec![(None, int_ty.clone())]
                } else {
                    vec![(None, int_ty.clone()), (None, int_ty.clone())]
                },
                span: empty_span(),
            },
        );

        // Second argument is the key length which is either 1 or 2
        let key_len = contract
            .exprs
            .insert_int(if var_ty.is_any_primitive() { 1 } else { 2 });

        // Third argument is the pathway, which is just a call to `__this_pathway` when accessing
        // local `pub var`s
        let pathway = contract.exprs.insert(
            Expr::IntrinsicCall {
                name: Ident {
                    name: "__this_pathway".to_string(),
                    hygienic: false,
                    span: empty_span(),
                },
                args: vec![],
                span: empty_span(),
            },
            int_ty.clone(),
        );

        // This is the `__transient` intrinsic we want to replace the `pub var` access with
        let transient_intrinsic = contract.exprs.insert(
            Expr::IntrinsicCall {
                name: Ident {
                    name: "__transient".to_string(),
                    hygienic: false,
                    span: empty_span(),
                },
                args: vec![key, key_len, pathway],
                span: empty_span(),
            },
            expr_key.get_ty(contract).clone(),
        );

        replacements.push((*expr_key, transient_intrinsic));
    }

    // Collect the names and `ExprKey`s for path expressions that correspond to external pub vars.
    // These are path expressions that are *not* local decision or state vars.
    let extern_pub_vars = contract
        .exprs(pred_key)
        .filter_map(|expr_key| {
            if let Some(Expr::Path(name, _)) = expr_key.try_get(contract) {
                (pred.vars().all(|(_, var)| &var.name != name)
                    && pred.states().all(|(_, state)| &state.name != name))
                .then(|| (name.clone(), expr_key))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    for (path, expr_key) in extern_pub_vars {
        for PredicateInstance {
            name,
            interface_instance,
            predicate: predicate_name,
            ..
        } in &pred.predicate_instances
        {
            // First, find the `interface` that this predicate instance belongs to
            let Some(interface_instance) = pred
                .interface_instances
                .iter()
                .find(|e| e.name.to_string() == *interface_instance)
            else {
                return Err(handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "interface instance cannot be found",
                        span: empty_span(),
                    },
                }));
            };

            let Some(interface) = contract
                .interfaces
                .iter()
                .find(|e| e.name.to_string() == *interface_instance.interface)
            else {
                return Err(handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "interface cannot be found",
                        span: empty_span(),
                    },
                }));
            };

            // Now find the predicate interface
            let Some(predicate_interface) = interface
                .predicate_interfaces
                .iter()
                .find(|e| e.name.to_string() == *predicate_name.to_string())
            else {
                return Err(handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "predicate interface cannot be found",
                        span: empty_span(),
                    },
                }));
            };

            // Now look for the `pub var` in the predicate interface. If it doesn't exist, then
            // look through other predicate instances.
            let Some((pub_var_index, var)) = predicate_interface
                .vars
                .iter()
                .enumerate()
                .find(|(_, var)| name.to_string() + "::" + &var.name.name == *path)
            else {
                continue;
            };

            // First argument is the key which depends on whether the type of the `pub var` is
            // primitive or not
            let pub_var_index = contract.exprs.insert_int(pub_var_index as i64);
            let key = contract.exprs.insert(
                Expr::Tuple {
                    fields: if var.ty.is_any_primitive() {
                        vec![(None, pub_var_index)]
                    } else {
                        vec![(None, pub_var_index), (None, zero)]
                    },
                    span: empty_span(),
                },
                Type::Tuple {
                    fields: if var.ty.is_any_primitive() {
                        vec![(None, int_ty.clone())]
                    } else {
                        vec![(None, int_ty.clone()), (None, int_ty.clone())]
                    },
                    span: empty_span(),
                },
            );

            // Second argument is the key length which is either 1 or 2
            let key_len = contract
                .exprs
                .insert_int(if var.ty.is_any_primitive() { 1 } else { 2 });

            // Third argument is the pathway, which is unique to the particular predicate instance
            // we found above and refers to a decision variable that have the name
            // `__<pred_instance_name>_pathway`. This variable is added in the parser.
            let pathway = contract.exprs.insert(
                Expr::Path(
                    "__".to_owned() + &name.to_string() + "_pathway",
                    empty_span(),
                ),
                int_ty.clone(),
            );

            // This is the `__transient` intrinsic we want to replace the `pub var` access with
            let transient_intrinsic = contract.exprs.insert(
                Expr::IntrinsicCall {
                    name: Ident {
                        name: "__transient".to_string(),
                        hygienic: false,
                        span: empty_span(),
                    },
                    args: vec![key, key_len, pathway],
                    span: empty_span(),
                },
                var.ty.clone(),
            );

            replacements.push((expr_key, transient_intrinsic));
        }
    }

    // Now, make all repalcements
    for (expr_key, intrinsic) in replacements {
        contract.replace_exprs(Some(pred_key), expr_key, intrinsic);
    }

    Ok(())
}

pub(crate) fn check_pathway_addresses_in_predicate(
    handler: &Handler,
    contract: &mut Contract,
    pred_key: PredKey,
) -> Result<(), ErrorEmitted> {
    let int_ty = Type::Primitive {
        kind: PrimitiveKind::Int,
        span: empty_span(),
    };
    let bool_ty = Type::Primitive {
        kind: PrimitiveKind::Bool,
        span: empty_span(),
    };
    let b256_ty = Type::Primitive {
        kind: PrimitiveKind::B256,
        span: empty_span(),
    };

    let pred = contract
        .preds
        .get(pred_key)
        .expect("predicate key is expected to exist");

    let mut constraints_on_addresses = vec![];
    for PredicateInstance {
        name,
        interface_instance,
        address,
        ..
    } in &pred.predicate_instances
    {
        // First, find the `interface` that this predicate instance belongs to
        let Some(interface_instance) = pred
            .interface_instances
            .iter()
            .find(|e| e.name.to_string() == *interface_instance)
        else {
            return Err(handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "interface instance cannot be found",
                    span: empty_span(),
                },
            }));
        };

        let pathway = contract.exprs.insert(
            Expr::Path(
                "__".to_owned() + &name.to_string() + "_pathway",
                empty_span(),
            ),
            int_ty.clone(),
        );

        // A full addresses is two `b256`s. One for the contract address and another for the
        // predicate address
        let addresses_ty = Type::Tuple {
            fields: vec![(None, b256_ty.clone()), (None, b256_ty.clone())],
            span: empty_span(),
        };

        let addresses = contract.exprs.insert(
            Expr::Tuple {
                fields: vec![(None, interface_instance.address), (None, *address)],
                span: empty_span(),
            },
            addresses_ty.clone(),
        );

        // Insert the `__predicate_at` intrinsic which takes the pathway as an argument
        let predicate_at_intrinsic = contract.exprs.insert(
            Expr::IntrinsicCall {
                name: Ident {
                    name: "__predicate_at".to_string(),
                    hygienic: false,
                    span: empty_span(),
                },
                args: vec![pathway],
                span: empty_span(),
            },
            addresses_ty,
        );

        // This is the constraint we want to insert:
        // ```
        // constraint __predicate_at(<pathway) == <addresses>
        // ```
        constraints_on_addresses.push(ConstraintDecl {
            expr: contract.exprs.insert(
                Expr::BinaryOp {
                    op: BinaryOp::Equal,
                    lhs: predicate_at_intrinsic,
                    rhs: addresses,
                    span: empty_span(),
                },
                bool_ty.clone(),
            ),
            span: empty_span(),
        });
    }

    // Now, actually insert all the constraints
    if let Some(pred) = contract.preds.get_mut(pred_key) {
        for constraint in constraints_on_addresses {
            pred.constraints.push(constraint);
        }
    }

    Ok(())
}
