use crate::{
    error::CompileError,
    expr::{BinaryOp, Expr, Immediate},
    intermediate::IntermediateIntent,
    span::{empty_span, Span},
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
