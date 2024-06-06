use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::{evaluate::Evaluator, BinaryOp, Expr, Immediate, TupleAccess, UnaryOp},
    intermediate::{BlockStatement, ConstraintDecl, ExprKey, IfDecl, IntermediateIntent},
    span::{empty_span, Spanned},
    types::{EnumDecl, NewTypeDecl, PrimitiveKind, Type},
};

use fxhash::FxHashMap;

pub(crate) fn lower_enums(
    handler: &Handler,
    ii: &mut IntermediateIntent,
) -> Result<(), ErrorEmitted> {
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
    for e in &ii.enums {
        // Add all variants for the enum.
        add_variants(e, &e.name.name);

        // We need to do exactly the same for any newtypes which are aliasing this enum.
        if let Some(alias_name) =
            ii.new_types
                .iter()
                .find_map(|NewTypeDecl { name, ty, .. }| {
                    (ty.get_enum_name() == Some(&e.name.name)).then_some(&name.name)
                })
        {
            add_variants(e, alias_name);
        }
    }

    // Find all the expressions referring to the variants and save them in a list.
    let mut replacements = Vec::new();
    for old_expr_key in ii.exprs() {
        if let Some(Expr::PathByName(path, _span)) = old_expr_key.try_get(ii) {
            if let Some(idx) = variant_map.get(path) {
                replacements.push((old_expr_key, idx));
            }
        }
    }

    let int_ty = Type::Primitive {
        kind: PrimitiveKind::Int,
        span: empty_span(),
    };

    let bool_ty = Type::Primitive {
        kind: PrimitiveKind::Bool,
        span: empty_span(),
    };

    // Replace the variant expressions with literal int equivalents.
    for (old_expr_key, idx) in replacements {
        let new_expr_key = ii.exprs.insert(
            Expr::Immediate {
                value: Immediate::Int(*idx as i64),
                span: empty_span(),
            },
            int_ty.clone(),
        );
        ii.replace_exprs(old_expr_key, new_expr_key);
    }

    // Replace any var or state enum type with int.  Also add constraints to disallow vars or state
    // to have values outside of the enum.
    for var_key in ii
        .vars()
        .map(|(var_key, _)| var_key)
        .filter(|var_key| var_key.get_ty(ii).is_enum())
        .collect::<Vec<_>>()
    {
        // Add the constraint.  Get the variant max for this enum first.
        let ty = var_key.get_ty(ii).clone();
        let variant_max = match variant_count_map.get(ty.get_enum_name().unwrap()) {
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

        let var_expr_key = ii
            .exprs
            .insert(Expr::PathByKey(var_key, empty_span()), int_ty.clone());

        let lower_bound_key = ii.exprs.insert(
            Expr::Immediate {
                value: Immediate::Int(0),
                span: empty_span(),
            },
            int_ty.clone(),
        );

        let upper_bound_key = ii.exprs.insert(
            Expr::Immediate {
                value: Immediate::Int(variant_max),
                span: empty_span(),
            },
            int_ty.clone(),
        );

        let lower_bound_cmp_key = ii.exprs.insert(
            Expr::BinaryOp {
                op: BinaryOp::GreaterThanOrEqual,
                lhs: var_expr_key,
                rhs: lower_bound_key,
                span: empty_span(),
            },
            bool_ty.clone(),
        );

        let upper_bound_cmp_key = ii.exprs.insert(
            Expr::BinaryOp {
                op: BinaryOp::LessThanOrEqual,
                lhs: var_expr_key,
                rhs: upper_bound_key,
                span: empty_span(),
            },
            bool_ty.clone(),
        );

        ii.constraints.push(ConstraintDecl {
            expr: lower_bound_cmp_key,
            span: empty_span(),
        });
        ii.constraints.push(ConstraintDecl {
            expr: upper_bound_cmp_key,
            span: empty_span(),
        });

        // Replace the type.
        ii.exprs.update_types(|_, expr_type| {
            if ty == *expr_type {
                *expr_type = int_ty.clone();
            }
        });
    }

    // Now do the actual type update from enum to int
    ii.vars.update_types(|_, ty| {
        if ty.is_enum() {
            *ty = int_ty.clone()
        }
    });

    // Not sure at this stage if we'll ever allow state to be an enum.
    for (state_key, _) in ii.states() {
        if state_key.get_ty(ii).is_enum() {
            return Err(handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "found state with an enum type",
                    span: empty_span(),
                },
            }));
        }
    }

    Ok(())
}

pub(crate) fn lower_bools(ii: &mut IntermediateIntent) {
    // Just do a blanket replacement of all bool types with int types.
    let int_ty = Type::Primitive {
        kind: PrimitiveKind::Int,
        span: empty_span(),
    };

    ii.vars.update_types(|_, ty| {
        if ty.is_bool() {
            *ty = int_ty.clone();
        }
    });

    ii.exprs.update_types(|_, expr_type| {
        if expr_type.is_bool() {
            *expr_type = int_ty.clone();
        }
    });

    ii.states.update_types(|_, ty| {
        if ty.is_bool() {
            *ty = int_ty.clone();
        }
    });

    // Replace any literal true or false falures with int equivalents.
    ii.exprs.update_exprs(|_, expr| {
        if let Expr::Immediate {
            value: Immediate::Bool(bool_val),
            span,
        } = expr
        {
            *expr = Expr::Immediate {
                value: Immediate::Int(*bool_val as i64),
                span: span.clone(),
            };
        }
    });
}

pub(crate) fn lower_casts(
    handler: &Handler,
    ii: &mut IntermediateIntent,
) -> Result<(), ErrorEmitted> {
    let mut replacements = Vec::new();

    for old_expr_key in ii.exprs() {
        if let Some(Expr::Cast {
            value,
            ty: to_ty,
            span,
        }) = old_expr_key.try_get(ii)
        {
            let from_ty = value.get_ty(ii);
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
        ii.replace_exprs(old_expr_key, new_expr_key);
    }

    Ok(())
}

pub(crate) fn lower_aliases(ii: &mut IntermediateIntent) {
    use std::borrow::BorrowMut;

    fn replace_alias(old_ty: &mut Type) {
        match old_ty {
            Type::Alias { ty, .. } => {
                *old_ty = *ty.clone();
            }

            Type::Array { ty, .. } => replace_alias(ty),
            Type::Tuple { fields, .. } => fields.iter_mut().for_each(|(_, ty)| replace_alias(ty)),
            Type::Map { ty_from, ty_to, .. } => {
                replace_alias(ty_from);
                replace_alias(ty_to);
            }

            Type::Error(_) | Type::Unknown(_) | Type::Primitive { .. } | Type::Custom { .. } => {}
        }
    }

    // Replace aliases with the actual type.
    ii.vars.update_types(|_, var| replace_alias(var));
    ii.states.update_types(|_, state| replace_alias(state));
    ii.exprs.update_types(|_, expr| replace_alias(expr));

    ii.exprs.update_exprs(|_, expr| {
        if let Expr::Cast { ty, .. } = expr {
            replace_alias(ty.borrow_mut());
        }
    });
}

pub(crate) fn lower_imm_accesses(
    handler: &Handler,
    ii: &mut IntermediateIntent,
) -> Result<(), ErrorEmitted> {
    let mut replace_direct_accesses = || {
        let candidates = ii
            .exprs()
            .filter_map(|expr_key| match expr_key.get(ii) {
                Expr::Index { expr, index, .. } => expr.try_get(ii).and_then(|array_expr| {
                    matches!(
                        array_expr,
                        Expr::Immediate {
                            value: Immediate::Array { .. },
                            ..
                        }
                    )
                    .then(|| (expr_key, Some((*expr, *index)), None))
                }),

                Expr::TupleFieldAccess { tuple, field, .. } => {
                    tuple.try_get(ii).and_then(|tuple_expr| {
                        matches!(
                            tuple_expr,
                            Expr::Immediate {
                                value: Immediate::Tuple(_),
                                ..
                            }
                        )
                        .then(|| (expr_key, None, Some((*tuple, field.clone()))))
                    })
                }

                _ => None,
            })
            .collect::<Vec<_>>();

        let evaluator = Evaluator::new(ii);
        let mut replacements = Vec::new();
        for (old_expr_key, array_idx, field_idx) in candidates {
            assert!(
                (array_idx.is_some() && field_idx.is_none())
                    || (array_idx.is_none() && field_idx.is_some())
            );

            if let Some((array_key, array_idx_key)) = array_idx {
                // We have an array access into an immediate.  Evaluate the index.
                let Some(idx_expr) = array_idx_key.try_get(ii) else {
                    return Err(handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "missing array index expression in lower_imm_accesses()",
                            span: empty_span(),
                        },
                    }));
                };

                match evaluator.evaluate(idx_expr, handler, ii) {
                    Ok(Immediate::Int(idx_val)) if idx_val >= 0 => {
                        let Some(Expr::Immediate {
                            value: Immediate::Array { elements, .. },
                            ..
                        }) = array_key.try_get(ii)
                        else {
                            return Err(handler.emit_err(Error::Compile {
                                error: CompileError::Internal {
                                    msg: "missing array expression in lower_imm_accesses()",
                                    span: empty_span(),
                                },
                            }));
                        };

                        // Save the original access expression key and the element it referred to.
                        replacements.push((old_expr_key, elements[idx_val as usize]));
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
                let Some(Expr::Immediate {
                    value: Immediate::Tuple(fields),
                    ..
                }) = tuple_key.try_get(ii)
                else {
                    return Err(handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "missing tuple expression in lower_imm_accesses()",
                            span: empty_span(),
                        },
                    }));
                };

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
        }

        let modified = !replacements.is_empty();

        // Iterate for each replacement without borrowing.
        while let Some((old_expr_key, new_expr_key)) = replacements.pop() {
            // Replace the old with the new throughout the II.
            ii.replace_exprs(old_expr_key, new_expr_key);
            ii.exprs.remove(old_expr_key);

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
    for loop_check in 0.. {
        if !replace_direct_accesses()? {
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

    Ok(())
}

pub(crate) fn lower_ins(
    handler: &Handler,
    ii: &mut IntermediateIntent,
) -> Result<(), ErrorEmitted> {
    let mut in_range_collections = Vec::new();
    let mut array_collections = Vec::new();

    // Collect all the `in` expressions which need to be replaced.  (Copy them out of the II.)
    for expr_key in ii.exprs() {
        if let Some(Expr::In {
            value,
            collection,
            span,
        }) = expr_key.try_get(ii)
        {
            if let Some(collection_expr) = collection.try_get(ii) {
                match collection_expr {
                    Expr::Range { lb, ub, span } => {
                        in_range_collections.push((expr_key, *value, *lb, *ub, span.clone()));
                    }

                    Expr::Immediate {
                        value: Immediate::Array { elements, .. },
                        span,
                    } => {
                        array_collections.push((expr_key, *value, elements.clone(), span.clone()));
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
    for (in_expr_key, value_key, lower_bounds_key, upper_bounds_key, span) in in_range_collections {
        let lb_cmp_key = ii.exprs.insert(
            Expr::BinaryOp {
                op: BinaryOp::GreaterThanOrEqual,
                lhs: value_key,
                rhs: lower_bounds_key,
                span: span.clone(),
            },
            bool_ty.clone(),
        );

        let ub_cmp_key = ii.exprs.insert(
            Expr::BinaryOp {
                op: BinaryOp::LessThanOrEqual,
                lhs: value_key,
                rhs: upper_bounds_key,
                span: span.clone(),
            },
            bool_ty.clone(),
        );

        let and_key = ii.exprs.insert(
            Expr::BinaryOp {
                op: BinaryOp::LogicalAnd,
                lhs: lb_cmp_key,
                rhs: ub_cmp_key,
                span: span.clone(),
            },
            bool_ty.clone(),
        );

        ii.replace_exprs(in_expr_key, and_key);
    }

    // Replace the array expressions.  `x in [a, b, c]` becomes `(x == a) || (x == b) || (x == c)`.
    for (in_expr_key, value_key, elements, span) in array_collections {
        let or_key = elements
            .into_iter()
            .map(|el_expr_key| {
                ii.exprs.insert(
                    Expr::BinaryOp {
                        op: BinaryOp::Equal,
                        lhs: value_key,
                        rhs: el_expr_key,
                        span: span.clone(),
                    },
                    bool_ty.clone(),
                )
            })
            .collect::<Vec<_>>() // Collect into Vec to avoid borrowing ii.exprs conflict.
            .into_iter()
            .reduce(|lhs, rhs| {
                ii.exprs.insert(
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

        ii.replace_exprs(in_expr_key, or_key);
    }

    if handler.has_errors() {
        Err(handler.cancel())
    } else {
        Ok(())
    }
}

/// Convert all `if` declarations into individual constraints that are pushed to `ii.constraints`.
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
pub(crate) fn lower_ifs(ii: &mut IntermediateIntent) {
    for if_decl in &ii.if_decls.clone() {
        let all_exprs = convert_if(ii, if_decl);
        for expr in all_exprs {
            ii.constraints.push(ConstraintDecl {
                expr,
                span: empty_span(),
            });
        }
    }

    // Remove all `if_decls`. We don't need them anymore.
    ii.if_decls.clear();
}

// Given an `IfDecl`, convert all of its statements to Boolean expressions and return all of
// them in a `Vec<ExprKey>`. This follows the principle that `a ==> b` is equivalent to `!a || b`.
fn convert_if(
    ii: &mut IntermediateIntent,
    IfDecl {
        condition,
        then_block,
        else_block,
        ..
    }: &IfDecl,
) -> Vec<ExprKey> {
    let condition_inverse = ii.exprs.insert(
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
        all_exprs.extend(convert_if_block_statement(ii, statement, condition_inverse));
    }

    if let Some(else_block) = else_block {
        // `!condition => statement` i.e. `!!condition || statement`
        for statement in else_block {
            all_exprs.extend(convert_if_block_statement(
                ii, statement,
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
    ii: &mut IntermediateIntent,
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
            converted_exprs.push(ii.exprs.insert(
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
            for inner_expr in convert_if(ii, if_decl) {
                converted_exprs.push(ii.exprs.insert(
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
