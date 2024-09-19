use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::{
        evaluate::Evaluator, BinaryOp, Expr, ExternalIntrinsic, Immediate, IntrinsicKind,
        MatchBranch, TupleAccess, UnaryOp,
    },
    predicate::{
        BlockStatement, Const, ConstraintDecl, Contract, ExprKey, ExprsIter, Ident, IfDecl,
        Interface, InterfaceVar, MatchDecl, MatchDeclBranch, PredKey, StorageVar, Var, VisitorKind,
    },
    span::{empty_span, Span, Spanned},
    types::{self, EnumDecl, NewTypeDecl, Path, PrimitiveKind, Type, UnionDecl},
};

use fxhash::FxHashMap;

use std::collections::VecDeque;

mod lower_pub_var_accesses;
mod lower_storage_accesses;
pub(crate) use lower_pub_var_accesses::lower_pub_var_accesses;
pub(crate) use lower_storage_accesses::lower_storage_accesses;

pub(crate) fn lower_enums(handler: &Handler, contract: &mut Contract) -> Result<(), ErrorEmitted> {
    // Each enum has its variants indexed from 0. Gather all the enum declarations and create a
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
            if state_key.get_ty(pred).has_nested_enum() {
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
            contract.replace_exprs(Some(pred_key), old_expr_key, new_expr_key);
        }

        // Replace any var or state enum type with int.  Also add constraints to disallow vars or state
        // to have values outside of the enum.
        for (_, var_name, _, variant_count) in &enum_var_keys {
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
        }

        // Now do the actual type update from enum to int
        let pred = contract.preds.get_mut(pred_key).unwrap();

        let var_keys: Vec<_> = pred.vars().map(|(var_key, _)| var_key).collect();
        for var_key in var_keys {
            var_key.get_ty_mut(pred).replace_nested_enum_with_int();
        }

        let expr_keys: Vec<_> = contract.exprs(pred_key).collect();
        for expr_key in expr_keys {
            expr_key.get_ty_mut(contract).replace_nested_enum_with_int();
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
            contract.replace_exprs(Some(pred_key), old_expr_key, new_expr_key);
        }
    }

    Ok(())
}

/// Lowers every `Type::Alias` to a concrete type
pub(crate) fn lower_aliases(contract: &mut Contract) {
    use std::borrow::BorrowMut;

    let new_types_map = FxHashMap::from_iter(
        contract
            .new_types
            .iter()
            .map(|NewTypeDecl { name, ty, .. }| (name.name.clone(), ty.clone())),
    );

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

            Type::Vector { ty, .. } => replace_alias(new_types_map, ty),

            Type::Error(_)
            | Type::Unknown(_)
            | Type::Any(_)
            | Type::Primitive { .. }
            | Type::Union { .. } => {}
        }
    }

    // Replace `Type::Alias` in every var and state decl type in the contract
    contract
        .preds
        .keys()
        .collect::<Vec<_>>()
        .iter()
        .for_each(|pred_key| {
            if let Some(pred) = contract.preds.get_mut(*pred_key) {
                pred.vars
                    .update_types(|_, var_ty| replace_alias(&new_types_map, var_ty));
                pred.states
                    .update_types(|_, state_ty| replace_alias(&new_types_map, state_ty));
            }
        });

    // Replace `Type::Alias` in every expression type in the contract
    contract
        .exprs
        .update_types(|_, expr_ty| replace_alias(&new_types_map, expr_ty));

    // Replace `Type::Alias` in every cast expression in the contract
    contract.exprs.update_exprs(|_, expr| {
        if let Expr::Cast { ty, .. } = expr {
            replace_alias(&new_types_map, ty.borrow_mut());
        }

        if let Expr::UnionValue { variant_ty, .. } = expr {
            replace_alias(&new_types_map, variant_ty);
        }
    });

    // Replace `Type::Alias` in every storage variable in the contract
    if let Some((storage_vars, _)) = contract.storage.as_mut() {
        storage_vars.iter_mut().for_each(|StorageVar { ty, .. }| {
            replace_alias(&new_types_map, ty);
        })
    }

    // Replace `Type::Alias` in every constant declaration in the contract
    contract
        .consts
        .values_mut()
        .for_each(|Const { decl_ty, .. }| replace_alias(&new_types_map, decl_ty));

    // Replace `Type::Alias` in every union declaration in the contract.
    for UnionDecl { variants, .. } in &mut contract.unions {
        for variant in variants {
            if let Some(variant_ty) = &mut variant.ty {
                replace_alias(&new_types_map, variant_ty);
            }
        }
    }

    // Replace `Type::Alias` in every interface in the contract
    contract.interfaces.iter_mut().for_each(
        |Interface {
             storage,
             predicate_interfaces,
             ..
         }| {
            // Replace `Type::Alias` in every storage variable in the interface
            if let Some((storage_vars, _)) = storage.as_mut() {
                storage_vars
                    .iter_mut()
                    .for_each(|StorageVar { ty, .. }| replace_alias(&new_types_map, ty))
            }

            // Replace `Type::Alias` in every decision variable in the interface
            predicate_interfaces
                .iter_mut()
                .for_each(|predicate_interface| {
                    predicate_interface
                        .vars
                        .iter_mut()
                        .for_each(|InterfaceVar { ty, .. }| replace_alias(&new_types_map, ty));
                });
        },
    );
}

pub(crate) fn lower_array_ranges(
    handler: &Handler,
    contract: &mut Contract,
) -> Result<(), ErrorEmitted> {
    // The type checker will have confirmed that every array range expression has an integer (or
    // enum) _type_. Find every single array range expression which isn't already an int primitive.
    // (One day we'll have `var` ranges, but we'll ignore them.)

    // Check if an expr key is to an immediate.
    let is_not_immediate = |contract: &Contract, expr_key: ExprKey| {
        !matches!(expr_key.get(contract), Expr::Immediate { .. })
    };

    // Get an array range expression from a type iff it's not already an immediate.
    let ty_non_int_range_expr = |contract: &Contract, pred_key: Option<PredKey>, ty: &Type| {
        ty.get_array_range_expr().and_then(|range_expr_key| {
            is_not_immediate(contract, range_expr_key).then_some((pred_key, range_expr_key))
        })
    };

    // Get all the non-immediate array range exprs from the contract root exprs.
    let mut array_range_expr_keys: Vec<(Option<PredKey>, ExprKey)> = contract
        .root_exprs()
        .filter_map(|range_expr_key| {
            is_not_immediate(contract, range_expr_key).then_some((None, range_expr_key))
        })
        .collect();

    for pred_key in contract.preds.keys() {
        array_range_expr_keys.extend(contract.exprs(pred_key).filter_map(|expr_key| {
            ty_non_int_range_expr(contract, Some(pred_key), expr_key.get_ty(contract))
        }));

        let pred = &contract.preds[pred_key];

        array_range_expr_keys.extend(pred.vars.vars().filter_map(|(var_key, _var)| {
            ty_non_int_range_expr(contract, Some(pred_key), var_key.get_ty(pred))
        }));

        array_range_expr_keys.extend(pred.states.states().filter_map(|(state_key, _state)| {
            ty_non_int_range_expr(contract, Some(pred_key), state_key.get_ty(pred))
        }));
    }

    // Now evaluate them all.  This pass should be after const decls have been resolved/replaced
    // and enums have been lowered, so the evaluator should be fairly simple.
    // TODO: It seems lower_enums() isn't lowering within array ranges, so we need to included them
    // here.
    let evaluator = Evaluator::new(&contract.enums);
    let mut eval_memos: FxHashMap<ExprKey, ExprKey> = FxHashMap::default();

    let int_ty = Type::Primitive {
        kind: PrimitiveKind::Int,
        span: empty_span(),
    };

    for (pred_key, old_range_expr_key) in array_range_expr_keys {
        let new_range_expr_key = match eval_memos.get(&old_range_expr_key) {
            Some(key) => *key,
            None => {
                // The type checker should already ensure that our immediate value returned is an int.
                let value = evaluator.evaluate_key(&old_range_expr_key, handler, contract)?;
                if !matches!(value, Immediate::Int(_)) {
                    return Err(handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "array range expression evaluates to non int immediate",
                            span: contract.expr_key_to_span(old_range_expr_key),
                        },
                    }));
                }

                // Create a new Primitive expr for the new range.
                let new_expr_key = contract.exprs.insert(
                    Expr::Immediate {
                        value,
                        span: contract.expr_key_to_span(old_range_expr_key),
                    },
                    int_ty.clone(),
                );

                eval_memos.insert(old_range_expr_key, new_expr_key);
                new_expr_key
            }
        };

        contract.replace_exprs(pred_key, old_range_expr_key, new_range_expr_key);
    }

    Ok(())
}

pub(crate) fn lower_imm_accesses(
    handler: &Handler,
    contract: &mut Contract,
) -> Result<(), ErrorEmitted> {
    let pred_keys = contract.preds.keys().collect::<Vec<_>>();

    let mut replace_direct_accesses = |pred_key: PredKey| {
        let candidates = ExprsIter::new_by_expr_set(contract, Some(pred_key), false, true)
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
            contract.replace_exprs(Some(pred_key), old_expr_key, new_expr_key);
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

            contract.replace_exprs(Some(pred_key), in_expr_key, and_key);
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

            contract.replace_exprs(Some(pred_key), in_expr_key, or_key);
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
                        kind: (
                            IntrinsicKind::External(ExternalIntrinsic::StateLen),
                            empty_span(),
                        ),
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

            contract.replace_exprs(Some(pred_key), *old_bin_op, new_bin_op);
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
        for if_decl in &contract.preds[pred_key].if_decls.clone() {
            all_exprs.extend(convert_if(contract, pred_key, if_decl));
        }

        let mut_pred = contract.preds.get_mut(pred_key).unwrap();

        for (expr, original_expr_span) in all_exprs {
            mut_pred.constraints.push(ConstraintDecl {
                expr,
                span: original_expr_span,
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
) -> Vec<(ExprKey, Span)> {
    let condition_inverse = contract.exprs.insert(
        Expr::UnaryOp {
            op: UnaryOp::Not,
            expr: *condition,
            span: condition.get(contract).span().clone(),
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
) -> Vec<(ExprKey, Span)> {
    let bool_ty = Type::Primitive {
        kind: PrimitiveKind::Bool,
        span: empty_span(),
    };

    let mut converted_exprs = vec![];
    match statement {
        BlockStatement::Constraint(constraint_decl) => {
            converted_exprs.push((
                contract.exprs.insert(
                    Expr::BinaryOp {
                        op: BinaryOp::LogicalOr,
                        lhs: condition_inverse,
                        rhs: constraint_decl.expr,
                        span: constraint_decl.span.clone(),
                    },
                    bool_ty.clone(),
                ),
                constraint_decl.span.clone(),
            ));
        }

        BlockStatement::If(if_decl) => {
            for inner_expr in convert_if(contract, pred_key, if_decl) {
                converted_exprs.push((
                    contract.exprs.insert(
                        Expr::BinaryOp {
                            op: BinaryOp::LogicalOr,
                            lhs: condition_inverse,
                            rhs: inner_expr.0,
                            span: if_decl.span.clone(),
                        },
                        bool_ty.clone(),
                    ),
                    if_decl.span.clone(),
                ));
            }
        }

        BlockStatement::Match(_match_decl) => {
            unreachable!("`match` declarations have already been lowered")
        }
    }

    converted_exprs
}

pub(super) fn replace_const_refs(contract: &mut Contract) {
    let consts = contract
        .consts
        .iter()
        .map(|(path, Const { expr, .. })| (path.clone(), *expr))
        .collect::<Vec<_>>();

    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        // Find all the paths which refer to a const and link them.  Iterate over all predicate
        // exprs and array range expressions.
        let const_refs = ExprsIter::new_by_expr_set(contract, Some(pred_key), false, true)
            .filter_map(|path_expr_key| {
                if let Expr::Path(path, _span) = path_expr_key.get(contract) {
                    consts.iter().find_map(|(const_path, const_expr_key)| {
                        (path == const_path).then_some((path_expr_key, *const_expr_key))
                    })
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        // Replace all paths to consts with the consts themselves.
        for (old_expr_key, new_expr_key) in const_refs {
            contract.replace_exprs(Some(pred_key), old_expr_key, new_expr_key);
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
            contract.replace_exprs(Some(pred_key), old_key, new_key);

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
                | Expr::UnionVariant { .. }
                | Expr::Path(..)
                | Expr::StorageAccess { .. }
                | Expr::ExternalStorageAccess { .. }
                | Expr::UnaryOp { .. }
                | Expr::BinaryOp { .. }
                | Expr::MacroCall { .. }
                | Expr::IntrinsicCall { .. }
                | Expr::Select { .. }
                | Expr::Match { .. }
                | Expr::Cast { .. }
                | Expr::In { .. }
                | Expr::Range { .. }
                | Expr::Generator { .. }
                | Expr::UnionTag { .. }
                | Expr::UnionValue { .. } => Coalescence::None,
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

pub(super) fn lower_matches(
    handler: &Handler,
    contract: &mut Contract,
) -> Result<(), ErrorEmitted> {
    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        // Remove each match decl one at a time and convert to equivalent if decls.
        while let Some(match_decl) = contract.preds[pred_key].match_decls.pop() {
            let if_decl = convert_match_to_if_decl(handler, contract, pred_key, match_decl)?;
            contract.preds[pred_key].if_decls.push(if_decl);
        }

        // Convert any match expressions in this predicate too.
        convert_match_exprs(handler, contract, pred_key);
    }

    Ok(())
}

// Convert `match` declarations into similar `if` declarations by casting the union to the variant
// required explicitly for each match branch.
//
// E.g.,
//
// union u = var1 | var2(bool) | var3(int);
// var x: u;
// match x {
//     u::var1 => {
//         constraint expr1;
//     },
//     u::var2(var2_expr) => {
//         constraint var2_expr;
//     },
//     else => {
//         constraint expr2;
//     },
// }
//
// becomes
//
// if union_tag_is(x, u::var1) {
//     constraint expr1;
// } else if union_tag_is(x, u::var2) {
//     constraint union_val(x, bool);
// } else {
//     constraint expr2;
// }
//
// `Contract::match_decls` is cleared when done.

fn convert_match_to_if_decl(
    handler: &Handler,
    contract: &mut Contract,
    pred_key: PredKey,
    match_decl: MatchDecl,
) -> Result<IfDecl, ErrorEmitted> {
    let MatchDecl {
        match_expr,
        match_branches,
        else_branch,
        span,
    } = match_decl;

    let union_ty = match_expr.get_ty(contract).clone();
    let bool_ty = types::r#bool();

    // For each branch replace any references to the binding with union value accessors.
    let mut if_branches: VecDeque<(ExprKey, Vec<BlockStatement>)> = match_branches
        .into_iter()
        .map(
            |MatchDeclBranch {
                 name,
                 name_span,
                 binding,
                 block,
             }| {
                // The binding type is the type we're matching for this variant.  It should be
                // valid as it passed type checking.  It _could_ be None as not all variants have a
                // binding.  In that case the `binding` above should also be None.
                union_ty
                    .get_union_variant_ty(&contract.unions, &name)
                    .map_err(|_| {
                        handler.emit_err(Error::Compile {
                            error: CompileError::Internal {
                                msg: "match can't be converted to if -- missing union type",
                                span: name_span.clone(),
                            },
                        });
                        handler.cancel()
                    })
                    .and_then(|binding_ty| match (binding, binding_ty) {
                        // Both are bound.
                        (Some(id), Some(ty)) => Ok(Some((id, ty.clone()))),

                        // Both are unbound.
                        (None, None) => Ok(None),

                        // There's a mismatch.
                        _ => {
                            handler.emit_err(Error::Compile {
                                error: CompileError::Internal {
                                    msg: "match can't be converted to if -- bindings mismatch",
                                    span: span.clone(),
                                },
                            });
                            Err(handler.cancel())
                        }
                    })
                    .and_then(|full_binding| {
                        // The condition expression is a 'union_tag == ...' expression.
                        let cond_expr = build_compare_tag_expr(
                            handler, contract, match_expr, &name, &name_span,
                        )?;

                        block
                            .into_iter()
                            .map(|block| {
                                convert_match_block_statement(
                                    handler,
                                    contract,
                                    pred_key,
                                    match_expr,
                                    &full_binding,
                                    block,
                                )
                            })
                            .collect::<Result<Vec<_>, _>>()
                            .map(|then_branch| (cond_expr, then_branch))
                    })
            },
        )
        .collect::<Result<_, _>>()?;

    let else_block = else_branch
        .map(|block| {
            block
                .into_iter()
                .map(|block| {
                    convert_match_block_statement(
                        handler, contract, pred_key, match_expr, &None, block,
                    )
                })
                .collect::<Result<_, _>>()
        })
        .transpose()?;

    fn build_if_decl(
        then_branch: (ExprKey, Vec<BlockStatement>),
        mut rest_branches: VecDeque<(ExprKey, Vec<BlockStatement>)>,
        else_block: Option<Vec<BlockStatement>>,
        span: &Span,
    ) -> IfDecl {
        IfDecl {
            condition: then_branch.0,
            then_block: then_branch.1,
            else_block: if let Some(next_then_branch) = rest_branches.pop_front() {
                // There are branches; recurse.
                let else_if_decl = build_if_decl(next_then_branch, rest_branches, else_block, span);

                Some(vec![BlockStatement::If(else_if_decl)])
            } else {
                // There are no more branches.  Use the else block
                else_block
            },
            span: span.clone(),
        }
    }

    if let Some(then_branch) = if_branches.pop_front() {
        Ok(build_if_decl(then_branch, if_branches, else_block, &span))
    } else {
        // There are no match branches.  Just make an `if false {} else <else_block>`.
        let false_imm_key = contract.exprs.insert(
            Expr::Immediate {
                value: Immediate::Bool(false),
                span: empty_span(),
            },
            bool_ty.clone(),
        );

        Ok(IfDecl {
            condition: false_imm_key,
            then_block: Vec::default(),
            else_block,
            span: span.clone(),
        })
    }
}

fn convert_match_block_statement(
    handler: &Handler,
    contract: &mut Contract,
    pred_key: PredKey,
    union_expr: ExprKey,
    binding: &Option<(Ident, Type)>,
    block_stmt: BlockStatement,
) -> Result<BlockStatement, ErrorEmitted> {
    match block_stmt {
        BlockStatement::Constraint(ConstraintDecl { expr, ref span }) => {
            if let Some((id, ty)) = binding {
                // Replace any bindings.
                let mut constraint_expr = expr;
                replace_binding(contract, pred_key, union_expr, id, ty, &mut constraint_expr);

                Ok(BlockStatement::Constraint(ConstraintDecl {
                    expr: constraint_expr,
                    span: span.clone(),
                }))
            } else {
                // No binding, return same statement.
                Ok(block_stmt)
            }
        }

        BlockStatement::If(IfDecl {
            mut condition,
            then_block,
            else_block,
            span,
        }) => {
            // Replace any bindings, recurse for the sub-blocks and just return the same statement.
            if let Some((id, ty)) = binding {
                replace_binding(contract, pred_key, union_expr, id, ty, &mut condition);
            }

            let then_block = then_block
                .into_iter()
                .map(|stmt| {
                    convert_match_block_statement(
                        handler, contract, pred_key, union_expr, binding, stmt,
                    )
                })
                .collect::<Result<_, _>>()?;

            let else_block = else_block
                .map(|else_block| {
                    else_block
                        .into_iter()
                        .map(|stmt| {
                            convert_match_block_statement(
                                handler, contract, pred_key, union_expr, binding, stmt,
                            )
                        })
                        .collect::<Result<_, _>>()
                })
                .transpose()?;

            Ok(BlockStatement::If(IfDecl {
                condition,
                then_block,
                else_block,
                span,
            }))
        }

        BlockStatement::Match(match_decl) => {
            // This is a nested match decl and has its own scope.
            convert_match_to_if_decl(handler, contract, pred_key, match_decl)
                .map(BlockStatement::If)
        }
    }
}

fn replace_binding(
    contract: &mut Contract,
    pred_key: PredKey,
    union_expr: ExprKey,
    binding: &Ident,
    bound_ty: &Type,
    expr_key: &mut ExprKey,
) {
    let mut path_exprs = Vec::default();

    let expr_is_bound = |path_expr: &Expr| {
        if let Expr::Path(path, _span) = path_expr {
            path.len() > 2 && binding.name == path[2..]
        } else {
            false
        }
    };

    let passed_expr_is_bound = expr_is_bound(expr_key.get(contract));

    contract.visitor_from_key(
        VisitorKind::DepthFirstParentsBeforeChildren,
        *expr_key,
        &mut |path_expr_key, path_expr| {
            if expr_is_bound(path_expr) {
                path_exprs.push(path_expr_key);
            }
        },
    );

    if passed_expr_is_bound || !path_exprs.is_empty() {
        let union_val_expr_key = contract.exprs.insert(
            Expr::UnionValue {
                union_expr,
                variant_ty: bound_ty.clone(),
                span: empty_span(),
            },
            bound_ty.clone(),
        );

        if passed_expr_is_bound {
            *expr_key = union_val_expr_key;
        }

        for path_expr in path_exprs {
            contract.replace_exprs(Some(pred_key), path_expr, union_val_expr_key);
        }
    }
}

// Convert `match` expressions into similar `select` expressions by casting the union to the
// variant required explicitly for each match branch.  Any constraints within the expression are
// extracted into an `if` declaration.
//
// E.g.,
//
// union u = var1 | var2(bool) | var3(int);
// var x: u;
// constraint match x {
//     u::var1 => {
//         constraint var1_expr;
//         0
//     }
//     u::var2(var2_expr) => {
//         constraint var2_expr;
//         1
//     }
//     u::var3(var3_expr) => var3_expr,
// }
//
// becomes
//
// if union_tag_is(x, u::var1) {
//     constraint var1_expr;
// } else if union_tag_is(x, u::var2) {
//     constraint union_val(x, bool);
// }
// constraint union_tag_is(x, u::var1) ? 0 : union_tag_is(x, u::var2) ? 1 : union_val(x, int);

fn convert_match_exprs(handler: &Handler, contract: &mut Contract, pred_key: PredKey) {
    let match_expr_keys = contract
        .exprs(pred_key)
        .filter(|expr_key| {
            expr_key
                .try_get(contract)
                .map(|expr| matches!(expr, Expr::Match { .. }))
                .unwrap_or(false)
        })
        .collect::<Vec<_>>();

    for match_expr_key in match_expr_keys {
        let _ = convert_match_expr(handler, contract, pred_key, match_expr_key);
    }
}

fn convert_match_expr(
    handler: &Handler,
    contract: &mut Contract,
    pred_key: PredKey,
    match_expr_key: ExprKey,
) -> Result<(), ErrorEmitted> {
    struct BranchInfo {
        union_expr: ExprKey,
        name: Path,
        name_span: Span,
        binding: Option<(Ident, Type)>,
        constraints: Vec<ExprKey>,
        expr: ExprKey,
    }

    let mut if_then_branches = VecDeque::default();
    let mut if_else_branch = None;
    let mut if_decl_span = None;

    if let Expr::Match {
        match_expr,
        match_branches,
        else_branch,
        span,
    } = match_expr_key.get(contract)
    {
        if_decl_span = Some(span.clone());

        let union_ty = match_expr.get_ty(contract);

        for MatchBranch {
            name,
            name_span,
            binding,
            constraints,
            expr,
        } in match_branches
        {
            // Replace the bound values within the constraint exprs, if there is a binding.
            let bound_ty = union_ty
                .get_union_variant_ty(&contract.unions, name)
                .map_err(|_| {
                    handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "match can't be converted to if -- missing union type",
                            span: name_span.clone(),
                        },
                    });
                    handler.cancel()
                })?;

            let full_binding = binding
                .as_ref()
                .and_then(|binding| bound_ty.map(|bound_ty| (binding.clone(), bound_ty.clone())));

            if_then_branches.push_back(BranchInfo {
                union_expr: *match_expr,
                name: name.clone(),
                name_span: name_span.clone(),
                binding: full_binding,
                constraints: constraints.clone(),
                expr: *expr,
            });
        }

        if_else_branch = else_branch
            .as_ref()
            .map(|else_branch| (else_branch.constraints.clone(), else_branch.expr));
    }

    if if_then_branches.is_empty() && if_else_branch.is_none() {
        // There were no constraints in any of the match branches.  No need for an `if` decl.
        return Ok(());
    }

    let else_block = if_else_branch.map(|(else_constraint_exprs, else_expr)| {
        let constraint_decls = else_constraint_exprs
            .into_iter()
            .map(|constraint_expr| {
                BlockStatement::Constraint(ConstraintDecl {
                    expr: constraint_expr,
                    span: contract.expr_key_to_span(constraint_expr),
                })
            })
            .collect();

        (constraint_decls, else_expr)
    });

    fn build_if_decl_and_select(
        handler: &Handler,
        contract: &mut Contract,
        pred_key: PredKey,
        mut then_branch: BranchInfo,
        mut rest_branches: VecDeque<BranchInfo>,
        else_branch: Option<(Vec<BlockStatement>, ExprKey)>,
        span: &Span,
    ) -> Result<(IfDecl, ExprKey), ErrorEmitted> {
        // The condition expression is used by both the `if` and `select`.
        let condition = build_compare_tag_expr(
            handler,
            contract,
            then_branch.union_expr,
            &then_branch.name,
            &then_branch.name_span,
        )?;

        if let Some((binding, bound_ty)) = then_branch.binding {
            for constraint_expr in &mut then_branch.constraints {
                replace_binding(
                    contract,
                    pred_key,
                    then_branch.union_expr,
                    &binding,
                    &bound_ty,
                    constraint_expr,
                );
            }

            replace_binding(
                contract,
                pred_key,
                then_branch.union_expr,
                &binding,
                &bound_ty,
                &mut then_branch.expr,
            );

            if let Some((_, mut else_expr)) = else_branch {
                replace_binding(
                    contract,
                    pred_key,
                    then_branch.union_expr,
                    &binding,
                    &bound_ty,
                    &mut else_expr,
                );
            }
        }

        let then_block = then_branch
            .constraints
            .into_iter()
            .map(|constraint_expr| {
                BlockStatement::Constraint(ConstraintDecl {
                    expr: constraint_expr,
                    span: contract.expr_key_to_span(constraint_expr),
                })
            })
            .collect();

        let (else_block, select_expr) = if let Some(next_then_branch) = rest_branches.pop_front() {
            // There are branches; recurse.
            let (else_if_decl, else_expr) = build_if_decl_and_select(
                handler,
                contract,
                pred_key,
                next_then_branch,
                rest_branches,
                else_branch,
                span,
            )?;

            let else_block = Some(vec![BlockStatement::If(else_if_decl)]);

            let select_expr_ty = else_expr.get_ty(contract).clone();
            let select_expr = contract.exprs.insert(
                Expr::Select {
                    condition,
                    then_expr: then_branch.expr,
                    else_expr,
                    span: span.clone(),
                },
                select_expr_ty,
            );

            (else_block, select_expr)
        } else {
            // There are no more branches.  Use the passed optional else block.
            let (else_block, else_expr) = match else_branch {
                Some((else_block, else_expr)) => (Some(else_block), Some(else_expr)),
                None => (None, None),
            };

            // If we have no else branch then we don't need to build a full 'select' expr, we can
            // just use the 'then' expr as a default/implied 'else'.
            let select_expr = else_expr
                .map(|else_expr| {
                    let select_expr_ty = else_expr.get_ty(contract).clone();
                    contract.exprs.insert(
                        Expr::Select {
                            condition,
                            then_expr: then_branch.expr,
                            else_expr,
                            span: span.clone(),
                        },
                        select_expr_ty,
                    )
                })
                .unwrap_or(then_branch.expr);

            (else_block, select_expr)
        };

        let if_decl = IfDecl {
            condition,
            then_block,
            else_block,
            span: span.clone(),
        };

        Ok((if_decl, select_expr))
    }

    let (if_decl, select_expr_key) = if let Some(then_branch) = if_then_branches.pop_front() {
        build_if_decl_and_select(
            handler,
            contract,
            pred_key,
            then_branch,
            if_then_branches,
            else_block,
            &if_decl_span.unwrap(),
        )?
    } else {
        // There are no match branches.  Just make an `if false {} else <else_block>` and use the
        // else expression as the 'select'.
        let else_expr = else_block.as_ref().map(|eb| Ok(eb.1)).unwrap_or_else(|| {
            // There are no then branches and no else branch which makes it impossible to create an
            // expression. (NOTE: Currently the parser makes this impossible.)
            Err(handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "Unable to convert a completely empty match expression.",
                    span: contract.expr_key_to_span(match_expr_key),
                },
            }))
        })?;

        let false_imm_key = contract.exprs.insert(
            Expr::Immediate {
                value: Immediate::Bool(false),
                span: empty_span(),
            },
            types::r#bool(),
        );

        let if_decl = IfDecl {
            condition: false_imm_key,
            then_block: Vec::default(),
            else_block: else_block.map(|eb| eb.0),
            span: if_decl_span.unwrap(),
        };

        (if_decl, else_expr)
    };

    contract.preds[pred_key].if_decls.push(if_decl);
    contract.replace_exprs(Some(pred_key), match_expr_key, select_expr_key);

    Ok(())
}

fn build_compare_tag_expr(
    handler: &Handler,
    contract: &mut Contract,
    union_expr: ExprKey,
    tag: &Path,
    span: &Span,
) -> Result<ExprKey, ErrorEmitted> {
    let tag_num = union_expr
        .get_ty(contract)
        .get_union_variant_as_num(&contract.unions, tag)
        .ok_or_else(|| {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "Union tag not found in union decl",
                    span: contract.expr_key_to_span(union_expr),
                },
            })
        })?;

    let get_tag_expr = contract.exprs.insert(
        Expr::UnionTag {
            union_expr,
            span: span.clone(),
        },
        types::r#int(),
    );

    let tag_num_expr = contract.exprs.insert(
        Expr::Immediate {
            value: Immediate::Int(tag_num as i64),
            span: span.clone(),
        },
        types::r#int(),
    );

    Ok(contract.exprs.insert(
        Expr::BinaryOp {
            op: BinaryOp::Equal,
            lhs: get_tag_expr,
            rhs: tag_num_expr,
            span: span.clone(),
        },
        types::r#bool(),
    ))
}

pub(super) fn lower_union_variant_paths(contract: &mut Contract) {
    let mut replacements: Vec<(PredKey, ExprKey, Type, Path, Span)> = Vec::default();

    for pred_key in contract.preds.keys() {
        for expr_key in contract.exprs(pred_key) {
            if let Expr::Path(path, span) = expr_key.get(contract) {
                let expr_ty = expr_key.get_ty(contract);
                if expr_ty.is_union(&contract.unions) {
                    // We have *some* path expression whose type is a union.  Could be a variable
                    // or constant, could be a path to the union (maybe?) or to a non-value
                    // variant.
                    if let Some(union_name) = expr_ty.get_union_name(&contract.unions) {
                        if path.starts_with(union_name) && path != union_name {
                            // The name of the union is at the start of the path, but it isn't the
                            // whole path.  So it must be a union variant path.
                            replacements.push((
                                pred_key,
                                expr_key,
                                expr_ty.clone(),
                                path.clone(),
                                span.clone(),
                            ));
                        }
                    }
                }
            }
        }
    }

    // For every found union variant path, replace it with an equivalent union variant expression.
    for (pred_key, old_expr_key, union_ty, path, span) in replacements {
        let new_expr_key = contract.exprs.insert(
            Expr::UnionVariant {
                path,
                path_span: span.clone(),
                value: None,
                span,
            },
            union_ty,
        );

        contract.replace_exprs(Some(pred_key), old_expr_key, new_expr_key);
    }
}
