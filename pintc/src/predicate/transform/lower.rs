use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::{evaluate::Evaluator, BinaryOp, Expr, Ident, Immediate, TupleAccess, UnaryOp},
    predicate::{
        BlockStatement, Const, ConstraintDecl, Contract, ExprKey, IfDecl, PredKey, StorageVar, Var,
    },
    span::{empty_span, Spanned},
    types::{EnumDecl, NewTypeDecl, PrimitiveKind, Type},
};

use fxhash::FxHashMap;

pub(crate) fn lower_enums(handler: &Handler, contract: &mut Contract) -> Result<(), ErrorEmitted> {
    // Each enum has its variants indexed from 0.  Gather all the enum declarations and create a
    // map from path to integer index.
    let mut variant_map = FxHashMap::default();
    let mut variant_count_map = FxHashMap::default();

    let mut add_variants = |e: &EnumDecl, name: &String| {
        for (i, v) in e.variants.iter().enumerate() {
            let full_path = name.clone() + "::" + v.name.as_str();
            variant_map.insert(full_path, i);
        }
        variant_count_map.insert(e.name.name.clone(), e.variants.len());
    };

    for e in &contract.enums {
        // Add all variants for the enum.
        add_variants(e, &e.name.name);

        // We need to do exactly the same for any newtypes which are aliasing this enum.
        if let Some(alias_name) =
            contract
                .new_types
                .iter()
                .find_map(|NewTypeDecl { name, ty, .. }| {
                    (ty.get_enum_name(&contract.enums) == Some(&e.name.name)).then_some(&name.name)
                })
        {
            add_variants(e, alias_name);
        }
    }

    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        let mut enum_replacements = Vec::default();
        let mut variant_replacements = Vec::default();
        let mut enum_var_keys = Vec::default();

        let int_ty = Type::Primitive {
            kind: PrimitiveKind::Int,
            span: empty_span(),
        };

        let bool_ty = Type::Primitive {
            kind: PrimitiveKind::Bool,
            span: empty_span(),
        };

        let pred = contract.preds.get(pred_key).unwrap();

        // Find all the expressions referring to the variants and save them in a list.
        for old_expr_key in contract.exprs(pred_key) {
            if let Some(Expr::Path(path, _span)) = old_expr_key.try_get(contract) {
                if let Some(idx) = variant_map.get(path) {
                    variant_replacements.push((old_expr_key, idx));
                }
                if let Some(count) = variant_count_map.get(path) {
                    enum_replacements.push((old_expr_key, *count));
                }
            }
        }

        // Gather all vars which have an enum type, along with that enum type and the variant
        // count.  Clippy says .filter(..).map(..) is cleaner than .filter_map(.. bool.then(..)).
        enum_var_keys.extend(
            pred.vars()
                .filter(|(var_key, _)| var_key.get_ty(pred).is_enum(&contract.enums))
                .map(|(var_key, Var { name, .. })| {
                    let enum_ty = var_key.get_ty(pred).clone();
                    let variant_count = variant_count_map
                        .get(enum_ty.get_enum_name(&contract.enums).unwrap())
                        .copied();
                    (var_key, name.clone(), enum_ty, variant_count)
                }),
        );

        // Not sure at this stage if we'll ever allow state to be an enum.
        for (state_key, _) in pred.states() {
            if state_key.get_ty(pred).is_enum(&contract.enums) {
                return Err(handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "found state with an enum type",
                        span: empty_span(),
                    },
                }));
            }
        }

        // Replace the variant expressions with literal int equivalents.
        for (old_expr_key, idx) in variant_replacements {
            let new_expr_key = contract.exprs.insert(
                Expr::Immediate {
                    value: Immediate::Int(*idx as i64),
                    span: empty_span(),
                },
                int_ty.clone(),
            );
            contract.replace_exprs(pred_key, old_expr_key, new_expr_key);
        }

        // Replace any var or state enum type with int.  Also add constraints to disallow vars or state
        // to have values outside of the enum.
        for (_, var_name, enum_ty, variant_count) in &enum_var_keys {
            // Add the constraint.  Get the variant max for this enum first.
            let variant_max = match variant_count {
                Some(c) => *c as i64 - 1,
                None => {
                    return Err(handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "unable to get enum variant count",
                            span: empty_span(),
                        },
                    }))
                }
            };

            let var_expr_key = contract.exprs.insert(
                Expr::Path(var_name.to_string(), empty_span()),
                int_ty.clone(),
            );

            let lower_bound_key = contract.exprs.insert(
                Expr::Immediate {
                    value: Immediate::Int(0),
                    span: empty_span(),
                },
                int_ty.clone(),
            );

            let upper_bound_key = contract.exprs.insert(
                Expr::Immediate {
                    value: Immediate::Int(variant_max),
                    span: empty_span(),
                },
                int_ty.clone(),
            );

            let lower_bound_cmp_key = contract.exprs.insert(
                Expr::BinaryOp {
                    op: BinaryOp::GreaterThanOrEqual,
                    lhs: var_expr_key,
                    rhs: lower_bound_key,
                    span: empty_span(),
                },
                bool_ty.clone(),
            );

            let upper_bound_cmp_key = contract.exprs.insert(
                Expr::BinaryOp {
                    op: BinaryOp::LessThanOrEqual,
                    lhs: var_expr_key,
                    rhs: upper_bound_key,
                    span: empty_span(),
                },
                bool_ty.clone(),
            );

            if let Some(pred) = contract.preds.get_mut(pred_key) {
                pred.constraints.push(ConstraintDecl {
                    expr: lower_bound_cmp_key,
                    span: empty_span(),
                });
                pred.constraints.push(ConstraintDecl {
                    expr: upper_bound_cmp_key,
                    span: empty_span(),
                });
            };

            // Replace the type.
            let exprs_with_enum_ty = contract
                .exprs(pred_key)
                .filter(|expr_key| expr_key.get_ty(contract).eq(&contract.new_types, enum_ty))
                .collect::<Vec<_>>();

            for enum_expr_key in exprs_with_enum_ty {
                enum_expr_key.set_ty(int_ty.clone(), contract);
            }
        }

        // Now do the actual type update from enum to int
        for (enum_var_key, _, _, _) in enum_var_keys {
            enum_var_key.set_ty(int_ty.clone(), contract.preds.get_mut(pred_key).unwrap());
        }

        // Array types can use enums as the range.  Recursively replace a range known to be an enum
        // with an immediate integer equivalent.
        fn replace_array_range(enum_expr_map: &FxHashMap<ExprKey, ExprKey>, ty: &mut Type) {
            if let Type::Array {
                ty: el_ty,
                range: Some(range_key),
                ..
            } = ty
            {
                // If the range for this array is an enum (i.e., we found it in our map) then replace
                // it with an immediate int.
                if let Some(count_expr_key) = enum_expr_map.get(range_key) {
                    *range_key = *count_expr_key;
                }

                // Recurse for multi-dimensional arrays.
                replace_array_range(enum_expr_map, el_ty);
            }
        }

        // Build a map from expr-key-of-path-to-enum to immediate-int-of-variant-count.
        let enum_count_exprs =
            FxHashMap::from_iter(enum_replacements.into_iter().map(|(expr_key, count)| {
                let imm_expr_key = contract.exprs.insert(
                    Expr::Immediate {
                        value: Immediate::Int(count as i64),
                        span: empty_span(),
                    },
                    int_ty.clone(),
                );
                (expr_key, imm_expr_key)
            }));

        // Update all the array types.
        contract
            .preds
            .get_mut(pred_key)
            .unwrap()
            .vars
            .update_types(|_var_key, ty| replace_array_range(&enum_count_exprs, ty));
        contract
            .exprs
            .update_types(|_expr_key, ty| replace_array_range(&enum_count_exprs, ty));
    }

    Ok(())
}

pub(crate) fn lower_casts(handler: &Handler, contract: &mut Contract) -> Result<(), ErrorEmitted> {
    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        let mut replacements = Vec::new();

        for old_expr_key in contract.exprs(pred_key) {
            if let Some(Expr::Cast {
                value,
                ty: to_ty,
                span,
            }) = old_expr_key.try_get(contract)
            {
                let from_ty = value.get_ty(contract);
                if from_ty.is_unknown() {
                    handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "expression type is `Unknown` while lowering casts",
                            span: span.clone(),
                        },
                    });
                }

                // The type checker will have already rejected bad cast types.
                if !(from_ty.is_int() && to_ty.is_real()) {
                    replacements.push((old_expr_key, *value));
                }
            }
        }

        for (old_expr_key, new_expr_key) in replacements {
            contract.replace_exprs(pred_key, old_expr_key, new_expr_key);
        }
    }

    Ok(())
}

pub(crate) fn lower_aliases(contract: &mut Contract) {
    use std::borrow::BorrowMut;

    let new_types = FxHashMap::from_iter(
        contract
            .new_types
            .iter()
            .map(|NewTypeDecl { name, ty, .. }| (name.name.clone(), ty.clone())),
    );

    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        fn replace_alias(new_types_map: &FxHashMap<String, Type>, old_ty: &mut Type) {
            match old_ty {
                Type::Alias { ty, .. } => {
                    *old_ty = *ty.clone();
                    replace_alias(new_types_map, old_ty);
                }

                Type::Custom { path, .. } => {
                    if let Some(ty) = new_types_map.get(path) {
                        *old_ty = ty.clone();
                    }
                }

                Type::Array { ty, .. } => replace_alias(new_types_map, ty),
                Type::Tuple { fields, .. } => fields
                    .iter_mut()
                    .for_each(|(_, ty)| replace_alias(new_types_map, ty)),
                Type::Map { ty_from, ty_to, .. } => {
                    replace_alias(new_types_map, ty_from);
                    replace_alias(new_types_map, ty_to);
                }

                Type::Error(_) | Type::Unknown(_) | Type::Primitive { .. } => {}
            }
        }

        // Replace aliases with the actual type.
        if let Some(pred) = contract.preds.get_mut(pred_key) {
            pred.vars
                .update_types(|_, var_ty| replace_alias(&new_types, var_ty));
            pred.states
                .update_types(|_, state_ty| replace_alias(&new_types, state_ty));
        };

        if let Some((storage_vars, _)) = &mut contract.storage {
            for StorageVar { ty, .. } in storage_vars {
                replace_alias(&new_types, ty);
            }
        }

        contract
            .exprs
            .update_types(|_, expr_ty| replace_alias(&new_types, expr_ty));

        contract.exprs.update_exprs(|_, expr| {
            if let Expr::Cast { ty, .. } = expr {
                replace_alias(&new_types, ty.borrow_mut());
            }
        });
    }
}

pub(crate) fn lower_imm_accesses(
    handler: &Handler,
    contract: &mut Contract,
) -> Result<(), ErrorEmitted> {
    let pred_keys = contract.preds.keys().collect::<Vec<_>>();

    let mut replace_direct_accesses = |pred_key: PredKey| {
        let candidates = contract
            .exprs(pred_key)
            .filter_map(|expr_key| match expr_key.get(contract) {
                Expr::Index { expr, index, .. } => expr.try_get(contract).and_then(|array_expr| {
                    let is_array_expr = matches!(array_expr, Expr::Array { .. });
                    let is_array_imm = matches!(
                        array_expr,
                        Expr::Immediate {
                            value: Immediate::Array(_),
                            ..
                        }
                    );

                    (is_array_expr || is_array_imm).then_some((
                        expr_key,
                        Some((*expr, *index)),
                        None,
                    ))
                }),

                Expr::TupleFieldAccess { tuple, field, .. } => {
                    tuple.try_get(contract).and_then(|tuple_expr| {
                        let is_tuple_expr = matches!(tuple_expr, Expr::Tuple { .. });
                        let is_tuple_imm = matches!(
                            tuple_expr,
                            Expr::Immediate {
                                value: Immediate::Tuple(_),
                                ..
                            }
                        );

                        (is_tuple_expr || is_tuple_imm)
                            .then(|| (expr_key, None, Some((*tuple, field.clone()))))
                    })
                }

                _ => None,
            })
            .collect::<Vec<_>>();

        let evaluator = Evaluator::new(&contract.enums);
        let mut replacements = Vec::new();
        for (old_expr_key, array_idx, field_idx) in candidates {
            assert!(
                (array_idx.is_some() && field_idx.is_none())
                    || (array_idx.is_none() && field_idx.is_some())
            );

            if let Some((array_key, array_idx_key)) = array_idx {
                // We have an array access into an immediate.  Evaluate the index.
                let Some(idx_expr) = array_idx_key.try_get(contract) else {
                    return Err(handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "missing array index expression in lower_imm_accesses()",
                            span: empty_span(),
                        },
                    }));
                };

                match evaluator.evaluate(idx_expr, handler, contract) {
                    Ok(Immediate::Int(idx_val)) if idx_val >= 0 => {
                        match array_key.try_get(contract) {
                            Some(Expr::Array { elements, .. }) => {
                                // Simply replace the index with the element expr.
                                replacements.push((old_expr_key, elements[idx_val as usize]));
                            }

                            Some(Expr::Immediate {
                                value: Immediate::Array(elements),
                                ..
                            }) => {
                                if idx_val as usize >= elements.len() {
                                    return Err(handler.emit_err(Error::Compile {
                                        error: CompileError::ArrayIndexOutOfBounds {
                                            span: idx_expr.span().clone(),
                                        },
                                    }));
                                }

                                // Create a new immediate expr and replace the index with it.
                                let el_imm = elements[idx_val as usize].clone();
                                let el_ty = el_imm.get_ty(None);

                                let el_expr = contract.exprs.insert(
                                    Expr::Immediate {
                                        value: el_imm,
                                        span: empty_span(),
                                    },
                                    el_ty,
                                );

                                replacements.push((old_expr_key, el_expr));
                            }

                            _ => unreachable!("candidate must be an array"),
                        }
                    }

                    Ok(_) => {
                        return Err(handler.emit_err(Error::Compile {
                            error: CompileError::InvalidConstArrayLength {
                                span: idx_expr.span().clone(),
                            },
                        }))
                    }

                    _ => {
                        return Err(handler.emit_err(Error::Compile {
                            error: CompileError::NonConstArrayLength {
                                span: idx_expr.span().clone(),
                            },
                        }))
                    }
                }
            }

            if let Some((tuple_key, tuple_field_key)) = field_idx {
                // We have a tuple access into an immediate.
                match tuple_key.try_get(contract) {
                    Some(Expr::Tuple { fields, .. }) => {
                        let new_expr_key = match tuple_field_key {
                            TupleAccess::Index(idx_val) => fields[idx_val].1,

                            TupleAccess::Name(access_name) => fields
                                .iter()
                                .find_map(|(opt_name, field_key)| {
                                    opt_name.as_ref().and_then(|field_name| {
                                        (field_name.name == access_name.name).then_some(*field_key)
                                    })
                                })
                                .expect("missing tuple field"),

                            TupleAccess::Error => unreachable!(),
                        };

                        // Save the original access expression key and the field it referred to.
                        replacements.push((old_expr_key, new_expr_key));
                    }

                    Some(Expr::Immediate {
                        value: Immediate::Tuple(fields),
                        ..
                    }) => {
                        let fld_imm = match tuple_field_key {
                            TupleAccess::Index(idx_val) => fields[idx_val].1.clone(),

                            TupleAccess::Name(access_name) => fields
                                .iter()
                                .find_map(|(opt_name, field_imm)| {
                                    opt_name.as_ref().and_then(|field_name| {
                                        (field_name.name == access_name.name)
                                            .then_some(field_imm.clone())
                                    })
                                })
                                .expect("missing tuple field"),

                            TupleAccess::Error => unreachable!(),
                        };

                        // Create a new immediate expr and replace the index with it.
                        let fld_ty = fld_imm.get_ty(None);

                        let fld_expr = contract.exprs.insert(
                            Expr::Immediate {
                                value: fld_imm,
                                span: empty_span(),
                            },
                            fld_ty,
                        );

                        replacements.push((old_expr_key, fld_expr));
                    }

                    _ => unreachable!("candidate must be a tuple"),
                }
            }
        }

        let modified = !replacements.is_empty();

        // Iterate for each replacement without borrowing.
        while let Some((old_expr_key, new_expr_key)) = replacements.pop() {
            // Replace the old with the new throughout the Pred.
            contract.replace_exprs(pred_key, old_expr_key, new_expr_key);
            contract.exprs.remove(old_expr_key);

            // But _also_ replace the old within `replacements` in case any of our new keys is now
            // stale.
            for (_, stale_expr_key) in &mut replacements {
                if *stale_expr_key == old_expr_key {
                    *stale_expr_key = new_expr_key;
                }
            }
        }

        Ok(modified)
    };

    // Replace all the direct array and tuple accesses until there are none left.  This will loop
    // for all the nested aggregates.
    for pred_key in pred_keys {
        for loop_check in 0.. {
            if !replace_direct_accesses(pred_key)? {
                break;
            }

            if loop_check > 10_000 {
                return Err(handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "infinite loop in lower_imm_accesses()",
                        span: empty_span(),
                    },
                }));
            }
        }
    }

    Ok(())
}

pub(crate) fn lower_ins(handler: &Handler, contract: &mut Contract) -> Result<(), ErrorEmitted> {
    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        let mut in_range_collections = Vec::new();
        let mut array_collections = Vec::new();

        // Collect all the `in` expressions which need to be replaced.  (Copy them out of the Pred.)
        for in_expr_key in contract.exprs(pred_key) {
            if let Some(Expr::In {
                value,
                collection,
                span,
            }) = in_expr_key.try_get(contract)
            {
                if let Some(collection_expr) = collection.try_get(contract) {
                    match collection_expr {
                        Expr::Range { lb, ub, span } => {
                            in_range_collections.push((
                                in_expr_key,
                                *value,
                                *lb,
                                *ub,
                                span.clone(),
                            ));
                        }

                        Expr::Immediate {
                            value: Immediate::Array(elements),
                            span,
                        } => {
                            array_collections.push((
                                in_expr_key,
                                *value,
                                *collection,
                                elements.len(),
                                collection
                                    .get_ty(contract)
                                    .get_array_el_type()
                                    .cloned()
                                    .expect("array must have array type"),
                                span.clone(),
                            ));
                        }

                        Expr::Array { elements, span, .. } => {
                            array_collections.push((
                                in_expr_key,
                                *value,
                                *collection,
                                elements.len(),
                                collection
                                    .get_ty(contract)
                                    .get_array_el_type()
                                    .cloned()
                                    .expect("array must have array type"),
                                span.clone(),
                            ));
                        }

                        _ => {
                            handler.emit_err(Error::Compile {
                                error: CompileError::Internal {
                                    msg: "invalid collection (not range or array) in `in` expr",
                                    span: span.clone(),
                                },
                            });
                        }
                    }
                } else {
                    handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "missing collection in `in` expr",
                            span: span.clone(),
                        },
                    });
                }
            }
        }

        let bool_ty = Type::Primitive {
            kind: PrimitiveKind::Bool,
            span: empty_span(),
        };

        // Replace the range expressions first. `x in l..u` becomes `(x >= l) && (x <= u)`.
        for (in_expr_key, value_key, lower_bounds_key, upper_bounds_key, span) in
            in_range_collections
        {
            let lb_cmp_key = contract.exprs.insert(
                Expr::BinaryOp {
                    op: BinaryOp::GreaterThanOrEqual,
                    lhs: value_key,
                    rhs: lower_bounds_key,
                    span: span.clone(),
                },
                bool_ty.clone(),
            );

            let ub_cmp_key = contract.exprs.insert(
                Expr::BinaryOp {
                    op: BinaryOp::LessThanOrEqual,
                    lhs: value_key,
                    rhs: upper_bounds_key,
                    span: span.clone(),
                },
                bool_ty.clone(),
            );

            let and_key = contract.exprs.insert(
                Expr::BinaryOp {
                    op: BinaryOp::LogicalAnd,
                    lhs: lb_cmp_key,
                    rhs: ub_cmp_key,
                    span: span.clone(),
                },
                bool_ty.clone(),
            );

            contract.replace_exprs(pred_key, in_expr_key, and_key);
        }

        let int_ty = Type::Primitive {
            kind: PrimitiveKind::Int,
            span: empty_span(),
        };

        // Replace the array expressions.
        // `x in ary` becomes `(x == ary[0]) || (x == ary[1]) || (x == ary[2]) || ...`.
        for (in_expr_key, value_key, array_key, element_count, element_ty, span) in
            array_collections
        {
            let or_key = (0..(element_count as i64))
                .map(|el_idx| {
                    let el_idx_val_key = contract.exprs.insert(
                        Expr::Immediate {
                            value: Immediate::Int(el_idx),
                            span: empty_span(),
                        },
                        int_ty.clone(),
                    );

                    let el_idx_expr_key = contract.exprs.insert(
                        Expr::Index {
                            expr: array_key,
                            index: el_idx_val_key,
                            span: empty_span(),
                        },
                        element_ty.clone(),
                    );

                    contract.exprs.insert(
                        Expr::BinaryOp {
                            op: BinaryOp::Equal,
                            lhs: value_key,
                            rhs: el_idx_expr_key,
                            span: span.clone(),
                        },
                        bool_ty.clone(),
                    )
                })
                .collect::<Vec<_>>() // Collect into Vec to avoid borrowing contract.exprs conflict.
                .into_iter()
                .reduce(|lhs, rhs| {
                    contract.exprs.insert(
                        Expr::BinaryOp {
                            op: BinaryOp::LogicalOr,
                            lhs,
                            rhs,
                            span: span.clone(),
                        },
                        bool_ty.clone(),
                    )
                })
                .expect("can't have empty array expressions");

            contract.replace_exprs(pred_key, in_expr_key, or_key);
        }
    }

    if handler.has_errors() {
        Err(handler.cancel())
    } else {
        Ok(())
    }
}

/// Convert all comparisons to `nil` to comparisons between the intrinsic `__state_len` and 0.
/// For example:
///
/// state x = storage::x;
/// state y = storage::x;
/// constraint x == nil;
/// constraint y != nil;
///
/// becomes:
///
/// state x = storage::x;
/// state y = storage::x;
/// constraint __state_len(x) == 0;
/// constraint __state_len(y) != 0;
///
pub(crate) fn lower_compares_to_nil(contract: &mut Contract) {
    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        let compares_to_nil = contract
            .exprs(pred_key)
            .filter_map(|expr_key| match expr_key.try_get(contract) {
                Some(Expr::BinaryOp { op, lhs, rhs, span })
                    if (*op == BinaryOp::Equal || *op == BinaryOp::NotEqual)
                        && (lhs.get(contract).is_nil() || rhs.get(contract).is_nil()) =>
                {
                    Some((expr_key, *op, *lhs, *rhs, span.clone()))
                }
                _ => None,
            })
            .collect::<Vec<_>>();

        let convert_to_state_len_compare =
            |contract: &mut Contract, op: &BinaryOp, expr: &ExprKey, span: &crate::span::Span| {
                let state_len = contract.exprs.insert(
                    Expr::IntrinsicCall {
                        name: Ident {
                            name: "__state_len".to_string(),
                            hygienic: false,
                            span: span.clone(),
                        },
                        args: vec![*expr],
                        span: span.clone(),
                    },
                    Type::Primitive {
                        kind: PrimitiveKind::Int,
                        span: empty_span(),
                    },
                );

                let zero = contract.exprs.insert(
                    Expr::Immediate {
                        value: Immediate::Int(0),
                        span: empty_span(),
                    },
                    Type::Primitive {
                        kind: PrimitiveKind::Bool,
                        span: empty_span(),
                    },
                );

                // New binary op: `__state_len(expr) == 0`
                contract.exprs.insert(
                    Expr::BinaryOp {
                        op: *op,
                        lhs: state_len,
                        rhs: zero,
                        span: span.clone(),
                    },
                    Type::Primitive {
                        kind: PrimitiveKind::Bool,
                        span: empty_span(),
                    },
                )
            };

        for (old_bin_op, op, lhs, rhs, span) in compares_to_nil.iter() {
            let new_bin_op = match (lhs.get(contract).is_nil(), rhs.get(contract).is_nil()) {
                (false, true) => convert_to_state_len_compare(contract, op, lhs, span),
                (true, false) => convert_to_state_len_compare(contract, op, rhs, span),
                (true, true) => contract.exprs.insert(
                    // Comparing two `nil`s should always return `false` regardless of whether this is
                    // an `Equal` or a `NotEqual`.
                    Expr::Immediate {
                        value: Immediate::Bool(false),
                        span: empty_span(),
                    },
                    Type::Primitive {
                        kind: PrimitiveKind::Bool,
                        span: empty_span(),
                    },
                ),
                _ => unreachable!("both operands cannot be non-nil simultaneously at this stage"),
            };

            contract.replace_exprs(pred_key, *old_bin_op, new_bin_op);
        }
    }
}

/// Convert all `if` declarations into individual constraints that are pushed to `pred.constraints`.
/// For example:
///
/// if c {
///     if d {
///         constraint x == y;
///     } else {
///         constraint 2 * x != y;
///     }
/// } else {
///     constraint 3 * x != y;
/// }
///
/// is equivalent to
///
/// constraint (!::c || (!::d || (::x == ::y)));
/// constraint (!::c || (::d || ((2 * ::x) != ::y)));
/// constraint (::c || ((3 * ::x) != ::y));
///
/// The transformation is recursive and uses the Boolean principle:
/// `a ==> b` is equivalent to `!a || b`.
///
pub(crate) fn lower_ifs(contract: &mut Contract) {
    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        let mut all_exprs = Vec::default();

        // Ideally we'd refactor this to not require cloning the IfDecl.
        for if_decl in &contract.preds.get(pred_key).unwrap().if_decls.clone() {
            all_exprs.extend(&convert_if(contract, pred_key, if_decl));
        }

        let mut_pred = contract.preds.get_mut(pred_key).unwrap();

        for expr in all_exprs {
            mut_pred.constraints.push(ConstraintDecl {
                expr,
                span: empty_span(),
            });
        }

        // Remove all `if_decls`. We don't need them anymore.
        mut_pred.if_decls.clear();
    }
}

// Given an `IfDecl`, convert all of its statements to Boolean expressions and return all of
// them in a `Vec<ExprKey>`. This follows the principle that `a ==> b` is equivalent to `!a || b`.
fn convert_if(
    contract: &mut Contract,
    pred_key: PredKey,
    IfDecl {
        condition,
        then_block,
        else_block,
        ..
    }: &IfDecl,
) -> Vec<ExprKey> {
    let condition_inverse = contract.exprs.insert(
        Expr::UnaryOp {
            op: UnaryOp::Not,
            expr: *condition,
            span: empty_span(),
        },
        Type::Primitive {
            kind: PrimitiveKind::Bool,
            span: empty_span(),
        },
    );

    let mut all_exprs = vec![];

    for statement in then_block {
        // `condition => statement` i.e. `!condition || statement`
        all_exprs.extend(convert_if_block_statement(
            contract,
            pred_key,
            statement,
            condition_inverse,
        ));
    }

    if let Some(else_block) = else_block {
        // `!condition => statement` i.e. `!!condition || statement`
        for statement in else_block {
            all_exprs.extend(convert_if_block_statement(
                contract, pred_key, statement,
                *condition, // use condition is here since it's the inverse of the inverse,
            ));
        }
    }

    all_exprs
}

// Given a if block statement and the inverse of a condition, produce a list of `ExprKey`s that
// contain all the converted statements.
//
// If `statement` is a `ConstraintDecl`, then the converted expression is `condition_inverse || expr`
// where `expr` is the expression inside the constraint.  This is the Boolean equivalent of
// `condition ==> expr`.
//
// If `statement` is an `IfDecl`, then recurse by calling `convert_if`.
//
fn convert_if_block_statement(
    contract: &mut Contract,
    pred_key: PredKey,
    statement: &BlockStatement,
    condition_inverse: ExprKey,
) -> Vec<ExprKey> {
    let bool_ty = Type::Primitive {
        kind: PrimitiveKind::Bool,
        span: empty_span(),
    };

    let mut converted_exprs = vec![];
    match statement {
        BlockStatement::Constraint(constraint_decl) => {
            converted_exprs.push(contract.exprs.insert(
                Expr::BinaryOp {
                    op: BinaryOp::LogicalOr,
                    lhs: condition_inverse,
                    rhs: constraint_decl.expr,
                    span: empty_span(),
                },
                bool_ty.clone(),
            ));
        }
        BlockStatement::If(if_decl) => {
            for inner_expr in convert_if(contract, pred_key, if_decl) {
                converted_exprs.push(contract.exprs.insert(
                    Expr::BinaryOp {
                        op: BinaryOp::LogicalOr,
                        lhs: condition_inverse,
                        rhs: inner_expr,
                        span: empty_span(),
                    },
                    bool_ty.clone(),
                ));
            }
        }
    }

    converted_exprs
}

pub(super) fn replace_const_refs(contract: &mut Contract) {
    let consts = contract
        .consts
        .iter()
        .map(|(path, Const { expr, decl_ty })| {
            (
                path.clone(),
                *expr,
                expr.get(contract).clone(),
                decl_ty.clone(),
            )
        })
        .collect::<Vec<_>>();

    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        // Find all the paths which refer to a const and link them.
        let const_refs = contract
            .exprs(pred_key)
            .filter_map(|path_expr_key| {
                if let Expr::Path(path, _span) = path_expr_key.get(contract) {
                    consts
                        .iter()
                        .find_map(|(const_path, const_expr_key, const_expr, const_ty)| {
                            (path == const_path).then_some((
                                path_expr_key,
                                *const_expr_key,
                                const_expr,
                                const_ty,
                            ))
                        })
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        // Replace all paths to consts with the consts themselves.
        if pred_key == contract.root_pred_key() {
            // This is the root Pred, meaning we already have the ExprKeys for the consts available.
            contract.replace_exprs_by_map(
                pred_key,
                &FxHashMap::from_iter(const_refs.into_iter().map(|refs| (refs.0, refs.1))),
            );
        } else {
            // This is NOT the root Pred, so we need to inject these const expressions into the
            // Pred before we can replace the paths.
            for (path_expr_key, _, const_expr, const_ty) in const_refs {
                let const_expr_key = contract.exprs.insert(const_expr.clone(), const_ty.clone());
                contract.replace_exprs(pred_key, path_expr_key, const_expr_key);
            }
        }
    }
}

pub(super) fn coalesce_prime_ops(contract: &mut Contract) {
    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        // Gather up all the keys to any NextState ops in this predicate.
        let mut work_list: Vec<(ExprKey, ExprKey)> = contract
            .exprs(pred_key)
            .filter_map(|op_key| {
                if let Expr::UnaryOp {
                    op: UnaryOp::NextState,
                    expr: arg_key,
                    ..
                } = op_key.get(contract)
                {
                    Some((op_key, *arg_key))
                } else {
                    None
                }
            })
            .collect();

        // We want to merge any ops which are applied to other ops.  And we want to push ops on index
        // or field access expressions up to the array or tuple that they're indexing.  In turn that
        // could create new prime ops applied to prime ops or indices/accesses.
        //
        // Doing this efficiently is non-trivial.  Whenever we replace an expression key in the
        // predicate we need to update the work list at the same time.

        fn replace_exprs(
            contract: &mut Contract,
            pred_key: PredKey,
            work_list: &mut Vec<(ExprKey, ExprKey)>,
            old_key: ExprKey,
            new_key: ExprKey,
        ) {
            contract.replace_exprs(pred_key, old_key, new_key);

            for (ref mut op_key, ref mut arg_key) in work_list {
                if *op_key == old_key {
                    *op_key = new_key;
                }
                if *arg_key == old_key {
                    *arg_key = new_key;
                }
            }
        }

        // The different transforms we may perform to coalesce prime ops.  Needed to avoid borrow
        // violations when we update the expressions in-place.
        enum Coalescence {
            None,
            MergeOps,
            LowerIndex(ExprKey),
            LowerAccess(ExprKey),
        }

        // Pop the next candidate (prime op key and its argument key) until the list is empty.
        while let Some((op_key, arg_key)) = work_list.pop() {
            let coalescence = match arg_key.get(contract) {
                Expr::UnaryOp {
                    op: UnaryOp::NextState,
                    ..
                } => Coalescence::MergeOps,

                Expr::Index { expr, .. } => Coalescence::LowerIndex(*expr),

                Expr::TupleFieldAccess { tuple, .. } => Coalescence::LowerAccess(*tuple),

                // The only other valid expressions will be PathByKey and PathByName, and for either
                // there's nothing to do -- they're popped off the work list.
                Expr::Error(_)
                | Expr::Immediate { .. }
                | Expr::Array { .. }
                | Expr::Tuple { .. }
                | Expr::Path(..)
                | Expr::StorageAccess(..)
                | Expr::ExternalStorageAccess { .. }
                | Expr::UnaryOp { .. }
                | Expr::BinaryOp { .. }
                | Expr::MacroCall { .. }
                | Expr::IntrinsicCall { .. }
                | Expr::Select { .. }
                | Expr::Cast { .. }
                | Expr::In { .. }
                | Expr::Range { .. }
                | Expr::Generator { .. } => Coalescence::None,
            };

            match coalescence {
                Coalescence::None => {}

                Coalescence::MergeOps => {
                    // E.g., a''.

                    // Replace any reference to the outer op expression with the inner arg op.
                    replace_exprs(contract, pred_key, &mut work_list, op_key, arg_key);
                }

                Coalescence::LowerIndex(indexed_key) => {
                    // E.g., a[i]' -> a'[i].

                    // Update any reference the the prime op to instead be to the index expression.
                    replace_exprs(contract, pred_key, &mut work_list, op_key, arg_key);

                    // Update the prime op to refer to the indexed expression and put this 'new' op
                    // back onto the list.
                    let Expr::UnaryOp {
                        expr: arg_key_ref, ..
                    } = op_key.get_mut(contract)
                    else {
                        unreachable!("op_key must be to a Unary::NextState")
                    };

                    *arg_key_ref = indexed_key;
                    work_list.push((op_key, indexed_key));

                    // Update the index expression to refer to the prime op.
                    let Expr::Index {
                        expr: indexed_key_ref,
                        ..
                    } = arg_key.get_mut(contract)
                    else {
                        unreachable!("arg_key must be to an Expr::Index")
                    };

                    *indexed_key_ref = op_key;
                }

                Coalescence::LowerAccess(accessed_key) => {
                    // E.g., a.x' -> a'.x.

                    // Update any reference to the prime op to instead be to the access expression.
                    replace_exprs(contract, pred_key, &mut work_list, op_key, arg_key);

                    // Update the prime op to refer to the accessed expression.  Also update the list
                    // to reflect the same.
                    let Expr::UnaryOp {
                        expr: old_arg_key, ..
                    } = op_key.get_mut(contract)
                    else {
                        unreachable!("op_key must be to a Unary::NextState")
                    };

                    *old_arg_key = accessed_key;
                    work_list.push((op_key, accessed_key));

                    // Update the access expression to refer to the prime op.
                    let Expr::TupleFieldAccess {
                        tuple: old_accessed_key,
                        ..
                    } = arg_key.get_mut(contract)
                    else {
                        unreachable!("arg_key must be to an Expr::TupleFieldAccess")
                    };

                    *old_accessed_key = op_key;
                }
            }
        }
    }
}
