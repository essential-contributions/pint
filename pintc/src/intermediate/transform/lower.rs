use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::{BinaryOp, Expr, Immediate, TupleAccess},
    intermediate::IntermediateIntent,
    span::{empty_span, Spanned},
    types::{EnumDecl, NewTypeDecl, PrimitiveKind, Type},
};

use std::collections::HashMap;

pub(crate) fn lower_enums(
    handler: &Handler,
    ii: &mut IntermediateIntent,
) -> Result<(), ErrorEmitted> {
    // Each enum has its variants indexed from 0.  Gather all the enum declarations and create a
    // map from path to integer index.
    let mut variant_map = HashMap::new();
    let mut variant_count_map = HashMap::new();
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
        if let Some(Expr::PathByName(path, _span)) = ii.exprs.get(old_expr_key) {
            if let Some(idx) = variant_map.get(path) {
                replacements.push((old_expr_key, idx));
            }
        }
    }

    let int_ty = Type::Primitive {
        kind: PrimitiveKind::Int,
        span: empty_span(),
    };

    // Replace the variant expressions with literal int equivalents.
    for (old_expr_key, idx) in replacements {
        let new_expr_key = ii.exprs.insert(Expr::Immediate {
            value: Immediate::Int(*idx as i64),
            span: empty_span(),
        });
        ii.expr_types.insert(new_expr_key, int_ty.clone());
        ii.replace_exprs(old_expr_key, new_expr_key);
    }

    // Replace any var or state enum type with int.  Also add constraints to disallow vars or state
    // to have values outside of the enum.

    for (var_key, ty) in ii.var_types.iter_mut() {
        if ty.is_enum() {
            // Add the constraint.  Get the variant max for this enum first.
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

            let var_expr_key = ii.exprs.insert(Expr::PathByKey(var_key, empty_span()));
            ii.expr_types.insert(var_expr_key, int_ty.clone());

            let lower_bound_key = ii.exprs.insert(Expr::Immediate {
                value: Immediate::Int(0),
                span: empty_span(),
            });
            ii.expr_types.insert(lower_bound_key, int_ty.clone());

            let upper_bound_key = ii.exprs.insert(Expr::Immediate {
                value: Immediate::Int(variant_max),
                span: empty_span(),
            });
            ii.expr_types.insert(upper_bound_key, int_ty.clone());

            let bool_ty = Type::Primitive {
                kind: PrimitiveKind::Bool,
                span: empty_span(),
            };

            let lower_bound_cmp_key = ii.exprs.insert(Expr::BinaryOp {
                op: BinaryOp::GreaterThanOrEqual,
                lhs: var_expr_key,
                rhs: lower_bound_key,
                span: empty_span(),
            });
            ii.expr_types.insert(lower_bound_cmp_key, bool_ty.clone());

            let upper_bound_cmp_key = ii.exprs.insert(Expr::BinaryOp {
                op: BinaryOp::LessThanOrEqual,
                lhs: var_expr_key,
                rhs: upper_bound_key,
                span: empty_span(),
            });
            ii.expr_types.insert(upper_bound_cmp_key, bool_ty.clone());

            ii.constraints.push((lower_bound_cmp_key, empty_span()));
            ii.constraints.push((upper_bound_cmp_key, empty_span()));

            // Replace the type.
            *ty = int_ty.clone();

            // TODO: fix expects
            let var_name = &ii.vars.get(var_key).expect("msg").name;

            for (expr_key, expr) in ii.exprs.iter() {
                if let Expr::PathByName(name, _) = expr {
                    if name == var_name {
                        ii.expr_types.insert(expr_key, int_ty.clone());
                    }
                } else if let Expr::PathByKey(key, _) = expr {
                    let name = &ii.vars.get(*key).expect("msg").name;
                    if name == var_name {
                        ii.expr_types.insert(expr_key, int_ty.clone());
                    }
                }
            }
        }
    }

    // Not sure at this stage if we'll ever allow state to be an enum.
    for (_, ty) in ii.state_types.iter_mut() {
        if ty.is_enum() {
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

    for (_var_key, ty) in ii.var_types.iter_mut() {
        if ty.is_bool() {
            *ty = int_ty.clone();
        }
    }

    for (_expr_key, ty) in ii.expr_types.iter_mut() {
        if ty.is_bool() {
            *ty = int_ty.clone();
        }
    }

    for (_state_key, ty) in ii.state_types.iter_mut() {
        if ty.is_bool() {
            *ty = int_ty.clone();
        }
    }

    // Replace any literal true or false falures with int equivalents.
    for (_expr_key, expr) in ii.exprs.iter_mut() {
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
    }
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
        }) = ii.exprs.get(old_expr_key)
        {
            let from_ty = ii.expr_types.get(*value).ok_or_else(|| {
                handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "unable to get expression type while lowering casts",
                        span: span.clone(),
                    },
                })
            })?;

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

            Type::Error(_) | Type::Primitive { .. } | Type::Custom { .. } => {}
        }
    }

    // Replace aliases with the actual type.
    for (_, old_ty) in ii.var_types.iter_mut() {
        replace_alias(old_ty);
    }

    for (_, old_ty) in ii.state_types.iter_mut() {
        replace_alias(old_ty);
    }

    for (_, old_ty) in ii.expr_types.iter_mut() {
        replace_alias(old_ty);
    }

    for (_, expr) in ii.exprs.iter_mut() {
        if let Expr::Cast { ty, .. } = expr {
            replace_alias(ty.borrow_mut());
        }
    }
}

pub(crate) fn lower_imm_accesses(
    handler: &Handler,
    ii: &mut IntermediateIntent,
) -> Result<(), ErrorEmitted> {
    let mut replace_direct_accesses = || {
        let candidates = ii
            .exprs()
            .filter_map(
                |expr_key| match ii.exprs.get(expr_key).expect("invalid key in iter") {
                    Expr::ArrayElementAccess { array, index, .. } => {
                        ii.exprs.get(*array).and_then(|array_expr| {
                            matches!(array_expr, Expr::Array { .. })
                                .then(|| (expr_key, Some((*array, *index)), None))
                        })
                    }

                    Expr::TupleFieldAccess { tuple, field, .. } => {
                        ii.exprs.get(*tuple).and_then(|tuple_expr| {
                            matches!(tuple_expr, Expr::Tuple { .. })
                                .then(|| (expr_key, None, Some((*tuple, field.clone()))))
                        })
                    }

                    _ => None,
                },
            )
            .collect::<Vec<_>>();

        let mut replacements = Vec::new();
        for (old_expr_key, array_idx, field_idx) in candidates {
            assert!(
                (array_idx.is_some() && field_idx.is_none())
                    || (array_idx.is_none() && field_idx.is_some())
            );

            if let Some((array_key, array_idx_key)) = array_idx {
                // We have an array access into an immediate.  Evaluate the index.
                let Some(idx_expr) = ii.exprs.get(array_idx_key) else {
                    return Err(handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "missing array index expression in lower_imm_accesses()",
                            span: empty_span(),
                        },
                    }));
                };

                match idx_expr.evaluate(handler, ii, &HashMap::new()) {
                    Ok(Immediate::Int(idx_val)) if idx_val >= 0 => {
                        let Some(Expr::Array { elements, .. }) = ii.exprs.get(array_key) else {
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
                let Some(Expr::Tuple { fields, .. }) = ii.exprs.get(tuple_key) else {
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
            ii.expr_types.remove(old_expr_key);

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
        }) = ii.exprs.get(expr_key)
        {
            if let Some(collection_expr) = ii.exprs.get(*collection) {
                match collection_expr {
                    Expr::Range { lb, ub, span } => {
                        in_range_collections.push((expr_key, *value, *lb, *ub, span.clone()));
                    }

                    Expr::Array { elements, span, .. } => {
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
        let lb_cmp_key = ii.exprs.insert(Expr::BinaryOp {
            op: BinaryOp::GreaterThanOrEqual,
            lhs: value_key,
            rhs: lower_bounds_key,
            span: span.clone(),
        });
        ii.expr_types.insert(lb_cmp_key, bool_ty.clone());

        let ub_cmp_key = ii.exprs.insert(Expr::BinaryOp {
            op: BinaryOp::LessThanOrEqual,
            lhs: value_key,
            rhs: upper_bounds_key,
            span: span.clone(),
        });
        ii.expr_types.insert(ub_cmp_key, bool_ty.clone());

        let and_key = ii.exprs.insert(Expr::BinaryOp {
            op: BinaryOp::LogicalAnd,
            lhs: lb_cmp_key,
            rhs: ub_cmp_key,
            span: span.clone(),
        });
        ii.expr_types.insert(and_key, bool_ty.clone());

        ii.replace_exprs(in_expr_key, and_key);
    }

    // Replace the array expressions.  `x in [a, b, c]` becomes `(x == a) || (x == b) || (x == c)`.
    for (in_expr_key, value_key, elements, span) in array_collections {
        let or_key = elements
            .into_iter()
            .map(|el_expr_key| {
                let cmp_eq_key = ii.exprs.insert(Expr::BinaryOp {
                    op: BinaryOp::Equal,
                    lhs: value_key,
                    rhs: el_expr_key,
                    span: span.clone(),
                });
                ii.expr_types.insert(cmp_eq_key, bool_ty.clone());
                cmp_eq_key
            })
            .collect::<Vec<_>>() // Collect into Vec to avoid borrowing ii.exprs conflict.
            .into_iter()
            .reduce(|lhs, rhs| {
                let cmp_or_key = ii.exprs.insert(Expr::BinaryOp {
                    op: BinaryOp::LogicalOr,
                    lhs,
                    rhs,
                    span: span.clone(),
                });
                ii.expr_types.insert(cmp_or_key, bool_ty.clone());
                cmp_or_key
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
