use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::{
        evaluate::Evaluator, BinaryOp, Expr, ExternalIntrinsic, Immediate, IntrinsicKind,
        MatchBranch, TupleAccess, UnaryOp,
    },
    predicate::{
        BlockStatement, Const, ConstraintDecl, Contract, ExprKey, ExprsIter, Ident, IfDecl,
        Interface, MatchDecl, MatchDeclBranch, Param, PredKey, PredicateInterface, StorageVar,
        VisitorKind,
    },
    span::{empty_span, Span, Spanned},
    types::{self, NewTypeDecl, PrimitiveKind, Type},
};

use fxhash::FxHashMap;

use std::{collections::VecDeque, sync::Arc};

mod lower_storage_accesses;
pub(crate) use lower_storage_accesses::lower_storage_accesses;

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
                    handler.emit_internal_err(
                        "expression type is `Unknown` while lowering casts".to_string(),
                        span.clone(),
                    );
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

            Type::Custom { name, .. } => {
                if let Some(ty) = new_types_map.get(name) {
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

    contract.update_types(|ty| replace_alias(&new_types_map, ty), true);
}

pub(crate) fn lower_array_ranges(
    handler: &Handler,
    contract: &mut Contract,
) -> Result<(), ErrorEmitted> {
    // The type checker will have confirmed that every array range expression has an integer (or
    // enum) _type_. Find every single array range expression which isn't already an int primitive.
    // (One day we'll have `var` ranges, but we'll ignore them.)

    // Check if an expr key is to an immediate.
    let expr_is_not_int_nor_enum = |contract: &Contract, expr_key: ExprKey| {
        !matches!(
            expr_key.get(contract),
            Expr::Immediate {
                value: Immediate::Int(_),
                ..
            }
        )
    };

    // Get an array range expression from a type iff it's not already an immediate.
    let ty_non_int_range_expr = |contract: &Contract, pred_key: Option<PredKey>, ty: &Type| {
        ty.get_array_range_expr().and_then(|range_expr_key| {
            expr_is_not_int_nor_enum(contract, range_expr_key).then_some((pred_key, range_expr_key))
        })
    };

    // Get all the non-immediate array range exprs from the contract root exprs.
    let mut array_range_expr_keys: Vec<(Option<PredKey>, ExprKey)> = contract
        .root_array_range_exprs()
        .filter_map(|range_expr_key| {
            expr_is_not_int_nor_enum(contract, range_expr_key).then_some((None, range_expr_key))
        })
        .collect();

    for Interface {
        storage,
        predicate_interfaces,
        ..
    } in &contract.interfaces
    {
        if let Some((storage_vars, _)) = storage {
            array_range_expr_keys.extend(
                storage_vars
                    .iter()
                    .filter_map(|StorageVar { ty, .. }| ty_non_int_range_expr(contract, None, ty)),
            );
        }

        array_range_expr_keys.extend(predicate_interfaces.iter().flat_map(
            |PredicateInterface { params, .. }| {
                params
                    .iter()
                    .filter_map(|Param { ty, .. }| ty_non_int_range_expr(contract, None, ty))
            },
        ));
    }

    for pred_key in contract.preds.keys() {
        array_range_expr_keys.extend(contract.exprs(pred_key).filter_map(|expr_key| {
            ty_non_int_range_expr(contract, Some(pred_key), expr_key.get_ty(contract))
        }));

        let pred = &contract.preds[pred_key];

        array_range_expr_keys.extend(
            pred.params
                .iter()
                .filter_map(|param| ty_non_int_range_expr(contract, Some(pred_key), &param.ty)),
        );

        array_range_expr_keys.extend(pred.variables().filter_map(|(variable_key, _)| {
            ty_non_int_range_expr(contract, Some(pred_key), variable_key.get_ty(pred))
        }));
    }

    // Now evaluate them all.  This pass should be after const decls have been resolved/replaced
    // and enums have been lowered, so the evaluator should be fairly simple.
    // TODO: It seems lower_enums() isn't lowering within array ranges, so we need to included them
    // here.
    let evaluator = Evaluator::new(contract);
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
                if let Expr::Path(name, _) = old_range_expr_key.get(contract) {
                    if contract
                        .unions
                        .values()
                        .any(|union| union.name.name == *name)
                    {
                        let new_expr_key = old_range_expr_key;
                        eval_memos.insert(old_range_expr_key, new_expr_key);
                        new_expr_key
                    } else {
                        let value =
                            evaluator.evaluate_key(&old_range_expr_key, handler, contract)?;
                        if !matches!(value, Immediate::Int(_) | Immediate::UnionVariant { .. }) {
                            return Err(handler.emit_internal_err(
                                "array range expression evaluates to non int immediate".to_string(),
                                contract.expr_key_to_span(old_range_expr_key),
                            ));
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
                } else {
                    let value = evaluator.evaluate_key(&old_range_expr_key, handler, contract)?;
                    if !matches!(value, Immediate::Int(_) | Immediate::UnionVariant { .. }) {
                        return Err(handler.emit_internal_err(
                            "array range expression evaluates to non int immediate".to_string(),
                            contract.expr_key_to_span(old_range_expr_key),
                        ));
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

        let evaluator = Evaluator::new(contract);
        let mut replacements = Vec::new();
        for (old_expr_key, array_idx, field_idx) in candidates {
            assert!(
                (array_idx.is_some() && field_idx.is_none())
                    || (array_idx.is_none() && field_idx.is_some())
            );

            if let Some((array_key, array_idx_key)) = array_idx {
                // We have an array access into an immediate.  Evaluate the index.
                let Some(idx_expr) = array_idx_key.try_get(contract) else {
                    return Err(handler.emit_internal_err(
                        "missing array index expression in lower_imm_accesses()".to_string(),
                        empty_span(),
                    ));
                };

                match evaluator.evaluate(idx_expr, handler, contract) {
                    Ok(
                        Immediate::Int(idx_val)
                        | Immediate::UnionVariant {
                            tag_num: idx_val, ..
                        },
                    ) if idx_val >= 0 => {
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
                return Err(handler.emit_internal_err(
                    "infinite loop in lower_imm_accesses()".to_string(),
                    empty_span(),
                ));
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
                            handler.emit_internal_err(
                                "invalid collection (not range or array) in `in` expr".to_string(),
                                span.clone(),
                            );
                        }
                    }
                } else {
                    handler.emit_internal_err(
                        "missing collection in `in` expr".to_string(),
                        span.clone(),
                    );
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

/// Convert all comparisons to `nil` to comparisons between the intrinsic `__size_of` and 0.
/// For example:
///
/// let x = storage::x;
/// let y = storage::x;
/// constraint x == nil;
/// constraint y != nil;
///
/// becomes:
///
/// let x = storage::x;
/// let y = storage::x;
/// constraint __size_of(x) == 0;
/// constraint __size_of(y) != 0;
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

        let convert_to_size_of_compare =
            |contract: &mut Contract, op: &BinaryOp, expr: &ExprKey, span: &crate::span::Span| {
                let size_of = contract.exprs.insert(
                    Expr::IntrinsicCall {
                        kind: (
                            IntrinsicKind::External(ExternalIntrinsic::SizeOf),
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

                // New binary op: `__size_of(expr) == 0`
                contract.exprs.insert(
                    Expr::BinaryOp {
                        op: *op,
                        lhs: size_of,
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
                (false, true) => convert_to_size_of_compare(contract, op, lhs, span),
                (true, false) => convert_to_size_of_compare(contract, op, rhs, span),
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
                | Expr::LocalStorageAccess { .. }
                | Expr::ExternalStorageAccess { .. }
                | Expr::UnaryOp { .. }
                | Expr::BinaryOp { .. }
                | Expr::MacroCall { .. }
                | Expr::IntrinsicCall { .. }
                | Expr::LocalPredicateCall { .. }
                | Expr::ExternalPredicateCall { .. }
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
            let if_decl = convert_match_to_if_decl(
                handler,
                contract,
                pred_key,
                match_decl,
                Arc::new(BindingStack::default()),
            )?;
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
    binding_stack: Arc<BindingStack>,
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
                    .get_union_variant_ty(contract, &name)
                    .map_err(|_| {
                        handler.emit_internal_err(
                            "match can't be converted to if -- missing union type".to_string(),
                            name_span.clone(),
                        );
                        handler.cancel()
                    })
                    .and_then(|binding_ty| match (binding, binding_ty) {
                        // Both are bound.
                        (Some(id), Some(ty)) => Ok(Some(Arc::new((match_expr, id, ty.clone())))),

                        // Both are unbound.
                        (None, None) => Ok(None),

                        // There's a mismatch.
                        _ => {
                            handler.emit_internal_err(
                                "match can't be converted to if -- bindings mismatch".to_string(),
                                span.clone(),
                            );
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
                                    BindingStack::wrap(
                                        full_binding.as_ref().map(Arc::clone),
                                        &binding_stack,
                                    ),
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
                        handler,
                        contract,
                        pred_key,
                        Arc::clone(&binding_stack),
                        block,
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
    binding_stack: Arc<BindingStack>,
    block_stmt: BlockStatement,
) -> Result<BlockStatement, ErrorEmitted> {
    match block_stmt {
        BlockStatement::Constraint(ConstraintDecl { expr, ref span }) => {
            if !binding_stack.is_empty() {
                // Replace any bindings.
                let mut constraint_expr = expr;
                binding_stack.replace_binding(contract, pred_key, &mut constraint_expr);

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
            binding_stack.replace_binding(contract, pred_key, &mut condition);

            let then_block = then_block
                .into_iter()
                .map(|stmt| {
                    convert_match_block_statement(
                        handler,
                        contract,
                        pred_key,
                        Arc::clone(&binding_stack),
                        stmt,
                    )
                })
                .collect::<Result<_, _>>()?;

            let else_block = else_block
                .map(|else_block| {
                    else_block
                        .into_iter()
                        .map(|stmt| {
                            convert_match_block_statement(
                                handler,
                                contract,
                                pred_key,
                                Arc::clone(&binding_stack),
                                stmt,
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

        BlockStatement::Match(mut match_decl) => {
            // Replace the match expression first if need be.
            binding_stack.replace_binding(contract, pred_key, &mut match_decl.match_expr);

            // Recurse back to converting this nested match into if decls.
            convert_match_to_if_decl(handler, contract, pred_key, match_decl, binding_stack)
                .map(BlockStatement::If)
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
        name: String,
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
            let bound_ty = union_ty.get_union_variant_ty(contract, name).map_err(|_| {
                handler.emit_internal_err(
                    "match can't be converted to if -- missing union type".to_string(),
                    name_span.clone(),
                );
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
            let binding_stack = BindingStack::wrap(
                Some(Arc::new((then_branch.union_expr, binding, bound_ty))),
                &Arc::new(BindingStack::default()),
            );

            for constraint_expr in &mut then_branch.constraints {
                binding_stack.replace_binding(contract, pred_key, constraint_expr);
            }

            binding_stack.replace_binding(contract, pred_key, &mut then_branch.expr);

            if let Some((_, mut else_expr)) = else_branch {
                binding_stack.replace_binding(contract, pred_key, &mut else_expr);
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
            Err(handler.emit_internal_err(
                "Unable to convert a completely empty match expression.".to_string(),
                contract.expr_key_to_span(match_expr_key),
            ))
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
    tag: &str,
    span: &Span,
) -> Result<ExprKey, ErrorEmitted> {
    let tag_num = union_expr
        .get_ty(contract)
        .get_union_variant_as_num(contract, tag)
        .ok_or_else(|| {
            handler.emit_internal_err(
                "Union tag not found in union decl".to_string(),
                contract.expr_key_to_span(union_expr),
            )
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
    // Converts a path to a union path, if possible. The path may stay the same or may be resolved
    // if an alias to the union is used in the original path.
    fn resolve_path(contract: &Contract, path: &str, expr_key: ExprKey) -> Option<String> {
        let path_parts = path.rfind("::").map(|sep_idx| {
            let prefix = &path[0..sep_idx];
            let suffix = &path[(sep_idx + 2)..];

            if let Some(alias_ty) = contract
                .new_types
                .iter()
                .find_map(|NewTypeDecl { name, ty, .. }| (prefix == name.name).then_some(ty))
            {
                if let Type::Union { decl, .. } = alias_ty {
                    // It's a union; use the union name as the prefix rather than the alias.
                    (contract.unions[*decl].name.name.as_str(), suffix)
                } else {
                    // Not a unions, just return the original split path.
                    (prefix, suffix)
                }
            } else {
                // Not an alias, ditto.
                (prefix, suffix)
            }
        });

        if let Some((path_prefix, path_suffix)) = path_parts {
            // We have *some* path expression whose type is a union.  Could be a variable
            // or constant, could be a path to the union (maybe?) or to a non-value
            // variant.
            if let Some(union_name) = expr_key.get_ty(contract).get_union_name(contract) {
                if path_prefix == union_name {
                    // The name of the union matches the path prefix.  We'll assume the
                    // suffix matches a variant..?  It really shouldn't type-check if not.
                    return Some(path_prefix.to_string() + "::" + path_suffix);
                }
            }
        }
        None
    }

    let mut replacements: Vec<(PredKey, ExprKey, String, Option<ExprKey>, Span)> = Vec::default();
    for pred_key in contract.preds.keys() {
        for expr_key in contract.exprs(pred_key) {
            match expr_key.get(contract) {
                Expr::Path(path, span) if expr_key.get_ty(contract).is_union() => {
                    if let Some(resolved) = resolve_path(contract, path, expr_key) {
                        replacements.push((pred_key, expr_key, resolved, None, span.clone()));
                    }
                }

                Expr::UnionVariant {
                    path, value, span, ..
                } => {
                    if let Some(resolved) = resolve_path(contract, path, expr_key) {
                        replacements.push((pred_key, expr_key, resolved, *value, span.clone()));
                    }
                }

                _ => {}
            }
        }
    }

    // For every found union variant path, replace it with an equivalent union variant expression.
    for (pred_key, old_expr_key, path, value, span) in replacements {
        let new_expr_key = contract.exprs.insert(
            Expr::UnionVariant {
                path,
                path_span: span.clone(),
                value,
                span,
            },
            old_expr_key.get_ty(contract).clone(),
        );

        contract.replace_exprs(Some(pred_key), old_expr_key, new_expr_key);
    }
}

#[derive(Default)]
struct BindingStack {
    binding: Option<Arc<(ExprKey, Ident, Type)>>,
    next: Option<Arc<Self>>,
}

impl BindingStack {
    fn wrap(new_binding: Option<Arc<(ExprKey, Ident, Type)>>, other: &Arc<Self>) -> Arc<Self> {
        if let Some(binding) = new_binding {
            Arc::new(BindingStack {
                binding: Some(Arc::clone(&binding)),
                next: Some(Arc::clone(other)),
            })
        } else {
            // No new binding to wrap; just return other.
            Arc::clone(other)
        }
    }

    fn is_empty(&self) -> bool {
        // Assuming .next is None if .binding is None.
        self.binding.is_none()
    }

    fn replace_binding(&self, contract: &mut Contract, pred_key: PredKey, expr_key: &mut ExprKey) {
        if let Some((_, union_expr, variant_ty, span)) =
            self.expr_bound_ty(*expr_key, expr_key.get(contract))
        {
            // The passed expression is a path which is bound.  Replace it in place.
            let union_val_expr_key = contract.exprs.insert(
                Expr::UnionValue {
                    union_expr,
                    variant_ty: variant_ty.clone(),
                    span,
                },
                variant_ty,
            );

            *expr_key = union_val_expr_key;
        } else {
            // Collect any path sub-expressions which may be found.
            let mut path_exprs = Vec::default();

            contract.visitor_from_key(
                VisitorKind::DepthFirstParentsBeforeChildren,
                *expr_key,
                &mut |path_expr_key, path_expr| {
                    if let Some(bound_ty) = self.expr_bound_ty(path_expr_key, path_expr) {
                        path_exprs.push(bound_ty);
                    }
                },
            );

            for (path_expr, union_expr, variant_ty, span) in path_exprs {
                let union_val_expr_key = contract.exprs.insert(
                    Expr::UnionValue {
                        union_expr,
                        variant_ty: variant_ty.clone(),
                        span,
                    },
                    variant_ty,
                );

                contract.replace_exprs(Some(pred_key), path_expr, union_val_expr_key);
            }
        }
    }

    fn expr_bound_ty(
        &self,
        path_expr_key: ExprKey,
        path_expr: &Expr,
    ) -> Option<(ExprKey, ExprKey, Type, Span)> {
        if let Some(binding) = &self.binding {
            let (union_expr, binding, bound_ty) = binding.as_ref();

            if let Expr::Path(path, span) = path_expr {
                // It IS a path...
                if path.len() > 2 && binding.name == path[2..] {
                    // ...and it is bound by this binding.
                    Some((path_expr_key, *union_expr, bound_ty.clone(), span.clone()))
                } else {
                    // Recurse to next binding in stack.
                    if let Some(binding_stack) = &self.next {
                        binding_stack.expr_bound_ty(path_expr_key, path_expr)
                    } else {
                        None
                    }
                }
            } else {
                // Not a path expr.
                None
            }
        } else {
            // No more bindings in stack.  (Assuming that if .binding is None then .next is too.)
            None
        }
    }
}
