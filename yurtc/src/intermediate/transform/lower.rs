use crate::{
    error::CompileError,
    expr::{BinaryOp, Expr, Immediate, TupleAccess},
    intermediate::IntermediateIntent,
    span::{empty_span, Span, Spanned},
    types::{EnumDecl, NewTypeDecl, PrimitiveKind, Type},
};

use std::collections::HashMap;

pub(crate) fn lower_enums(ii: &mut IntermediateIntent) -> Result<(), CompileError> {
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
    for (old_expr_key, expr) in &ii.exprs {
        if let Expr::PathByName(path, _span) = expr {
            if let Some(idx) = variant_map.get(path) {
                replacements.push((old_expr_key, idx));
            }
        }
    }

    // Replace the variant expressions with literal int equivalents.
    for (old_expr_key, idx) in replacements {
        let new_expr_key = ii.exprs.insert(Expr::Immediate {
            value: Immediate::Int(*idx as i64),
            span: empty_span(),
        });
        ii.replace_exprs(old_expr_key, new_expr_key);
    }

    // Replace any var or state enum type with int.  Also add constraints to disallow vars or state
    // to have values outside of the enum.

    let int_ty = Type::Primitive {
        kind: PrimitiveKind::Int,
        span: empty_span(),
    };

    for (var_key, ty) in ii.var_types.iter_mut() {
        if ty.is_enum() {
            // Add the constraint.  Get the variant max for this enum first.
            let variant_max = match variant_count_map.get(ty.get_enum_name().unwrap()) {
                Some(c) => *c as i64 - 1,
                None => {
                    return Err(CompileError::Internal {
                        msg: "unable to get enum variant count",
                        span: empty_span(),
                    })
                }
            };

            let var_expr_key = ii.exprs.insert(Expr::PathByKey(var_key, empty_span()));

            let lower_bound_key = ii.exprs.insert(Expr::Immediate {
                value: Immediate::Int(0),
                span: empty_span(),
            });

            let upper_bound_key = ii.exprs.insert(Expr::Immediate {
                value: Immediate::Int(variant_max),
                span: empty_span(),
            });

            let lower_bound_cmp_key = ii.exprs.insert(Expr::BinaryOp {
                op: BinaryOp::GreaterThanOrEqual,
                lhs: var_expr_key,
                rhs: lower_bound_key,
                span: empty_span(),
            });

            let upper_bound_cmp_key = ii.exprs.insert(Expr::BinaryOp {
                op: BinaryOp::LessThanOrEqual,
                lhs: var_expr_key,
                rhs: upper_bound_key,
                span: empty_span(),
            });

            ii.constraints.push((lower_bound_cmp_key, empty_span()));
            ii.constraints.push((upper_bound_cmp_key, empty_span()));

            // Replace the type.
            *ty = int_ty.clone();
        }
    }

    // Not sure at this stage if we'll ever allow state to be an enum.
    for (_, ty) in ii.state_types.iter_mut() {
        if ty.is_enum() {
            return Err(CompileError::Internal {
                msg: "found state with an enum type",
                span: empty_span(),
            });
        }
    }

    Ok(())
}

pub(crate) fn lower_bools(ii: &mut IntermediateIntent) -> Result<(), CompileError> {
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

    Ok(())
}

pub(crate) fn lower_casts(ii: &mut IntermediateIntent) -> Result<(), CompileError> {
    // FROM  TO    ACTION
    // int   int   No-op
    // int   real  Produce the closest possible real
    // real  real  No-op
    // enum  int   Enum cast (performed in lower_enums())
    // bool  int   Boolean to integer cast (performed in lower_bools())

    fn check_cast_types(
        ii: &IntermediateIntent,
        from_ty: &Type,
        to_ty: &Type,
        span: &Span,
    ) -> Result<(), CompileError> {
        if !to_ty.is_int() && !to_ty.is_real() {
            // We can only cast to ints or reals.
            Err(CompileError::BadCastTo {
                ty: ii.with_ii(to_ty).to_string(),
                span: span.clone(),
            })
        } else if (to_ty.is_int() && !from_ty.is_int() && !from_ty.is_enum() && !from_ty.is_bool())
            || (to_ty.is_real() && !from_ty.is_int() && !from_ty.is_real())
        {
            // We can only cast to ints from ints, enums or bools and to reals from ints or reals.
            Err(CompileError::BadCastFrom {
                ty: ii.with_ii(from_ty).to_string(),
                span: span.clone(),
            })
        } else {
            Ok(())
        }
    }

    let mut replacements = Vec::new();
    for (old_expr_key, expr) in &ii.exprs {
        if let Expr::Cast {
            value,
            ty: to_ty,
            span,
        } = expr
        {
            let from_ty = ii
                .expr_types
                .get(*value)
                .ok_or_else(|| CompileError::Internal {
                    msg: "unable to get expression type while lowering casts",
                    span: span.clone(),
                })?;

            check_cast_types(ii, from_ty, to_ty, span)?;
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

pub(crate) fn lower_aliases(ii: &mut IntermediateIntent) -> Result<(), CompileError> {
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

    Ok(())
}

pub(crate) fn lower_imm_accesses(ii: &mut IntermediateIntent) -> Result<(), CompileError> {
    let mut replace_direct_accesses = || {
        let candidates = ii
            .exprs
            .iter()
            .filter_map(|(expr_key, expr)| match expr {
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
            })
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
                    return Err(CompileError::Internal {
                        msg: "missing array index expression in lower_imm_accesses()",
                        span: empty_span(),
                    });
                };

                match idx_expr.evaluate(ii, &HashMap::new()) {
                    Ok(Immediate::Int(idx_val)) if idx_val >= 0 => {
                        let Some(Expr::Array { elements, .. }) = ii.exprs.get(array_key) else {
                            return Err(CompileError::Internal {
                                msg: "missing array expression in lower_imm_accesses()",
                                span: empty_span(),
                            });
                        };

                        // Save the original access expression key and the element it referred to.
                        replacements.push((old_expr_key, elements[idx_val as usize]));
                    }

                    Ok(_) => {
                        return Err(CompileError::InvalidConstArrayLength {
                            span: idx_expr.span().clone(),
                        })
                    }

                    _ => {
                        return Err(CompileError::NonConstArrayLength {
                            span: idx_expr.span().clone(),
                        })
                    }
                }
            }

            if let Some((tuple_key, tuple_field_key)) = field_idx {
                // We have a tuple access into an immediate.
                let Some(Expr::Tuple { fields, .. }) = ii.exprs.get(tuple_key) else {
                    return Err(CompileError::Internal {
                        msg: "missing tuple expression in lower_imm_accesses()",
                        span: empty_span(),
                    });
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
            return Err(CompileError::Internal {
                msg: "infinite loop in lower_imm_accesses()",
                span: empty_span(),
            });
        }
    }

    Ok(())
}
