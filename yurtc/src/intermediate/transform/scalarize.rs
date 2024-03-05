use crate::{
    error::CompileError,
    expr::{BinaryOp, Immediate, TupleAccess},
    intermediate::{Expr, ExprKey, IntermediateIntent, Var, VarKey},
    span::{empty_span, Span, Spanned},
    types::{Path, PrimitiveKind, Type},
};
use std::collections::{BTreeMap, HashMap};

pub(crate) fn scalarize(ii: &mut IntermediateIntent) -> Result<(), CompileError> {
    scalarize_arrays(ii)?;
    scalarize_tuples(ii)?;

    Ok(())
}

/// Scalarize an array by converting it into `n` decision variables where `n` is the total size of
/// the array (taking into account multi-dimensional arrays0. These new variables represent the
/// individual elements of the array. The names of the individual elements are chosen to be
/// `<array-name>[<index>]..[<index>]`.
///
/// For example, this 2D array:
///
/// ```yurt
/// let a: int[3][2];
/// ```
///
/// becomes
///
/// ```yurt
/// let a[0][0]: int;
/// let a[1][0]: int;
/// let a[2][0]: int;
/// let a[0][1]: int;
/// let a[1][1]: int;
/// let a[2][1]: int;
/// ```
///
/// The above is not valid Yurt, of course, because the square brackets are not allowed in
/// identifiers, but internally, this is fine and helps make the lookup quite easy.
fn scalarize_array(
    ii: &mut IntermediateIntent,
    key: VarKey,
    name: &String,
    ty: &Type,
    range: ExprKey,
    span: &Span,
    array_sizes: &mut HashMap<String, i64>,
) -> Result<(), CompileError> {
    match ty {
        Type::Array { .. }
        | Type::Primitive {
            kind: PrimitiveKind::Int | PrimitiveKind::Real | PrimitiveKind::Bool,
            ..
        } => {
            let range = ii.exprs.get(range).expect("expr key guaranteed to exist");
            match range.evaluate(ii, &HashMap::new()) {
                Ok(Immediate::Int(val)) if val > 0 => {
                    array_sizes.insert(name.clone(), val);
                    for i in 0..val {
                        let new_var = Var {
                            name: format!("{name}[{i}]"),
                            span: span.clone(),
                        };
                        let new_var_key = ii.vars.insert(new_var.clone());
                        ii.var_types.insert(new_var_key, ty.clone());

                        // Recurse for arrays of arrays
                        if let Type::Array {
                            ty: inner_ty,
                            range: inner_range,
                            ..
                        } = ty
                        {
                            scalarize_array(
                                ii,
                                new_var_key,
                                &new_var.name,
                                inner_ty,
                                *inner_range,
                                span,
                                array_sizes,
                            )?;
                        }
                    }
                    ii.vars.remove(key);
                    Ok(())
                }
                Ok(_) => Err(CompileError::InvalidConstArrayLength {
                    span: range.span().clone(),
                }),
                _ => Err(CompileError::NonConstArrayLength {
                    span: range.span().clone(),
                }),
            }
        }
        _ => {
            // Eventually, this will go away. Hence why it's an internal error for the time being
            Err(CompileError::Internal {
                msg: "only arrays of ints, reals, and bools are currently supported",
                span: empty_span(),
            })
        }
    }
}

/// Scalarize an array access by converting it to a simple path expression that looks like
/// `<array-name>[<index>]..[<index>]`.
///
/// For example, this array element access:
///
/// ```yurt
/// constraint a[2][3] == 3; // here, `a[2][3]` is an `Expr::ArrayElementAccess { .. }`
/// ```
///
/// becomes
///
/// ```yurt
/// constraint a[2][3] == 3; // here, `a[2][3]` is an `Expr::PathByName( .. )`
/// ```
///
/// This matches the name of variable `a[2][3]` introduced when array `a` is scalaried in `fn
/// scalarize_array(..)`
fn scalarize_array_access(
    ii: &mut IntermediateIntent,
    key: ExprKey,
    array: ExprKey,
    index: ExprKey,
    span: &Span,
    array_sizes: &HashMap<String, i64>,
) -> Result<Path, CompileError> {
    let index = ii.exprs.get(index).expect("expr key guaranteed to exist");
    let index_value = index.evaluate(ii, &HashMap::new());
    let index_span = index.span().clone();
    let array = ii.exprs.get(array).expect("expr key guaranteed to exist");
    macro_rules! handle_array_access {
        ($path: expr, $size: expr) => {{
            // Try to evaluate the index using compile-time evaluation
            // Index must be a non-negative integer
            match &index_value {
                Ok(Immediate::Int(val)) if *val >= 0 => {
                    let path = format!("{}[{val}]", $path);

                    if val >= $size {
                        return Err(CompileError::ArrayIndexOutOfBounds { span: index_span });
                    }

                    *ii.exprs
                        .get_mut(key)
                        .expect("key guaranteed to exist in the map!") =
                        Expr::PathByName(path.clone(), span.clone());
                    Ok(path)
                }
                Ok(_) => Err(CompileError::InvalidConstArrayIndex { span: index_span }),
                _ => Err(CompileError::NonConstArrayIndex { span: index_span }),
            }
        }};
    }

    match &array {
        Expr::PathByName(path, _) => match array_sizes.get(path) {
            Some(size) => handle_array_access!(path, size),
            None => Err(CompileError::Internal {
                msg: "Array size not found for given path",
                span: index_span,
            }),
        },
        Expr::PathByKey(path_key, _) => {
            let path = &ii.vars[*path_key].name;
            match array_sizes.get(path) {
                Some(size) => handle_array_access!(path, size),
                None => Err(CompileError::Internal {
                    msg: "Array size not found for given path",
                    span: index_span,
                }),
            }
        }
        Expr::ArrayElementAccess {
            array: array_inner,
            index: index_inner,
            ..
        } => {
            let path =
                scalarize_array_access(ii, key, *array_inner, *index_inner, span, array_sizes)?;
            match &array_sizes.get(&path) {
                Some(size) => handle_array_access!(path, size),
                None => Err(CompileError::Internal {
                    msg: "Array size not found for given path",
                    span: index_span,
                }),
            }
        }
        // Now this does not catch paths that do not represent arrays just yet. That is,
        // you could still index into a variable of type `int` or even a type name. Once we
        // have a type checker and expressions hold types, then we can improve this check.
        _ => Err(CompileError::CannotIndexIntoValue {
            span: array.span().clone(),
            index_span: index.span().clone(),
        }),
    }
}

/// Scalarize arrays by converting each array of size `n` into `n` new decision variable that
/// represent the individual elements of the array. The names of the individual elements are chosen
/// to be `<array-name>[<index>]..[<index>]`.
///
/// For example, consider the following:
///
/// ```yurt
/// let a: int[3];
///
/// constraint a[2] == 3;
/// ```
///
/// this becomes
///
/// ```yurt
/// let a[0]: int;
/// let a[1]: int;
/// let a[2]: int;
///
/// constraint a[2] == 3; // here, `a[2]` is an `Expr::PathByName( .. )`
/// ```
///
/// The above is not valid Yurt, of course, because the square brackets are not allowed in
/// identifiers, but internally, this is fine and helps make the lookup quite easy.
fn scalarize_arrays(ii: &mut IntermediateIntent) -> Result<(), CompileError> {
    fn get_array_params(ary_ty: &Type) -> Option<(Type, ExprKey, Span)> {
        match ary_ty {
            Type::Alias { ty, .. } => get_array_params(ty),
            Type::Array { ty, range, span } => Some((*ty.clone(), *range, span.clone())),
            _ => None,
        }
    }

    let mut array_sizes: HashMap<String, i64> = HashMap::new();

    // First, convert decision variables that are arrays into `n` new decision variables that
    // represent the individual elements of the array, where `n` is the length of the array
    ii.vars
        .iter()
        .filter_map(|(key, var)| {
            // Only collect arrays
            ii.var_types.get(key).and_then(|var_ty| {
                get_array_params(var_ty).map(|ary_params| (key, var.name.clone(), ary_params))
            })
        })
        .collect::<Vec<_>>()
        .iter()
        .try_for_each(|(key, name, (ty, range, span))| {
            scalarize_array(ii, *key, name, ty, *range, span, &mut array_sizes)
        })?;

    // Next, change each array element access into its scalarized variable
    ii.exprs
        .iter()
        .filter_map(|(key, expr)| {
            // Only collect array element accesses
            if let Expr::ArrayElementAccess { array, index, span } = expr {
                Some((key, *array, *index, span.clone()))
            } else {
                None
            }
        })
        .collect::<Vec<_>>()
        .iter()
        .try_for_each(|(key, array, index, span)| {
            scalarize_array_access(ii, *key, *array, *index, span, &array_sizes).map(|_| ())
        })
}

/// Scalarize tuples by extracting the tuple fields out into their own decision variables.  The
/// new names are `<tuple-name>.<field-name>` if the fields are named, or `<tuple-name>.<index>` if
/// not.
///
/// E.g.,
///
/// ```yurt
/// let a: { x: int, real };
///
/// constraint a.x < 11;    // here `a.x` is a tuple-access expr.
/// ```
///
/// becomes
///
/// ```yurt
/// let a.x: int;
/// let a.1: real;
///
/// constraint a.x < 11;    // `a.x` is now a path expr, an illegal identifier usually.
///
/// ```
fn scalarize_tuples(ii: &mut IntermediateIntent) -> Result<(), CompileError> {
    macro_rules! iterate {
        ($continue_expr: expr, $msg: literal) => {
            for loop_check in 0.. {
                if !$continue_expr {
                    break;
                }

                if loop_check > 10_000 {
                    return Err(CompileError::Internal {
                        msg: concat!("infinite loop in ", $msg),
                        span: empty_span(),
                    });
                }
            }
        };
    }

    // First we need to lower any aggregate comparisons.  It is valid to use `==` or `!=` to
    // compare whole tuples, but we must split these comparisons into field-by-field ops before we
    // scalarise.
    //
    // Do it in a loop so we can lower nested tuples.  I.e., we might create a new field-by-field
    // comparison between inner tuples and so they need to be lowered too.
    iterate!(lower_tuple_compares(ii)?, "lower_tuple_compares()");

    // Split all tuple vars into their fields.  We accumulate a set of var_keys which need to be
    // removed once we're done.
    let mut old_tuple_vars = Vec::new();
    iterate!(
        split_tuple_vars(ii, &mut old_tuple_vars)?,
        "split_tuple_vars()"
    );
    for var_key in old_tuple_vars {
        ii.vars.remove(var_key);
    }

    Ok(())
}

fn lower_tuple_compares(ii: &mut IntermediateIntent) -> Result<bool, CompileError> {
    // Gather all the valid binary op exprs.
    let mut tuple_compare_ops = Vec::new();
    for (expr_key, expr) in &ii.exprs {
        if let Expr::BinaryOp { op, lhs, rhs, span } = expr {
            if (*op == BinaryOp::Equal || *op == BinaryOp::NotEqual)
                && ii
                    .expr_types
                    .get(*lhs)
                    .map(|lhs_ty| lhs_ty.is_tuple())
                    .unwrap_or(false)
            {
                // Type checking should ensure RHS is also a tuple.
                assert!(ii.expr_types.get(*rhs).unwrap().is_tuple());

                tuple_compare_ops.push((expr_key, *op, *lhs, *rhs, span.clone()));
            }
        }
    }

    let modified = !tuple_compare_ops.is_empty();

    for (expr_key, op, lhs_tuple_key, rhs_tuple_key, span) in tuple_compare_ops {
        // Get the tuple field names and types for both sides, as they might be the same type but
        // have different accessors.  We *could* just drop the names and always use indices but
        // this means our lowered identifiers will be less descriptive.

        let Some(lhs_fields) = ii
            .expr_types
            .get(lhs_tuple_key)
            .expect("failed to get tuple type in lower_tuple_compares()")
            .get_tuple_fields()
            .map(|fs| fs.to_vec())
        else {
            unreachable!("failed to get lhs tuple field types in lower_tuple_compares()");
        };
        let Some(rhs_fields) = ii
            .expr_types
            .get(rhs_tuple_key)
            .expect("failed to get tuple type in lower_tuple_compares()")
            .get_tuple_fields()
            .map(|fs| fs.to_vec())
        else {
            unreachable!("failed to get rhs tuple field types in lower_tuple_compares()");
        };

        // Replace the op with a chain of field-by-field comparisons.
        let mut new_field_compare_ops = Vec::new();

        // This can be a bit tricky since tuples with named fields can declare literals with
        // the fields in any order.  The rules say that two tuples-with-named-fields types are
        // only equivalent if all the fields are named.  This is managed by Type::eq() and type
        // checking has already checked this for us.
        //
        // So if ALL the fields for both tuples are named then we can use the names as
        // accessors, otherwise we will use indices.
        let use_named_accessors = lhs_fields.iter().all(|(name, _)| name.is_some())
            && rhs_fields.iter().all(|(name, _)| name.is_some());

        for (field_idx, (opt_field_name, field_ty)) in lhs_fields.iter().enumerate() {
            // For the LHS get the access by index, but use the name if its there.
            let lhs_field_access = opt_field_name
                .as_ref()
                .map(|field_name| TupleAccess::Name(field_name.clone()))
                .unwrap_or_else(|| TupleAccess::Index(field_idx));

            let rhs_field_access = if use_named_accessors {
                // We use the same named accessor as LHS.
                lhs_field_access.clone()
            } else {
                // Use the field at field_idx, but still by name if it has one.
                rhs_fields[field_idx]
                    .0
                    .as_ref()
                    .map(|field_name| TupleAccess::Name(field_name.clone()))
                    .unwrap_or_else(|| TupleAccess::Index(field_idx))
            };

            let lhs_access = ii.exprs.insert(Expr::TupleFieldAccess {
                tuple: lhs_tuple_key,
                field: lhs_field_access,
                span: span.clone(),
            });

            let rhs_access = ii.exprs.insert(Expr::TupleFieldAccess {
                tuple: rhs_tuple_key,
                field: rhs_field_access,
                span: span.clone(),
            });

            ii.expr_types.insert(lhs_access, field_ty.clone());
            ii.expr_types.insert(rhs_access, field_ty.clone());

            let field_compare_op = ii.exprs.insert(Expr::BinaryOp {
                op,
                lhs: lhs_access,
                rhs: rhs_access,
                span: span.clone(),
            });
            ii.expr_types.insert(
                field_compare_op,
                Type::Primitive {
                    kind: PrimitiveKind::Bool,
                    span: span.clone(),
                },
            );

            new_field_compare_ops.push(field_compare_op);
        }

        let and_chain_expr_key = new_field_compare_ops
            .into_iter()
            .reduce(|acc, compare_op_key| {
                let and_op_key = ii.exprs.insert(Expr::BinaryOp {
                    op: BinaryOp::LogicalAnd,
                    lhs: acc,
                    rhs: compare_op_key,
                    span: span.clone(),
                });

                ii.expr_types.insert(
                    and_op_key,
                    Type::Primitive {
                        kind: PrimitiveKind::Bool,
                        span: span.clone(),
                    },
                );

                and_op_key
            })
            .expect("there must be 1 or more tuple fields");

        ii.replace_exprs(expr_key, and_chain_expr_key);
        ii.exprs.remove(expr_key);
    }

    Ok(modified)
}

fn split_tuple_vars(
    ii: &mut IntermediateIntent,
    old_tuple_vars: &mut Vec<VarKey>,
) -> Result<bool, CompileError> {
    let mut new_vars = Vec::new();

    // Iterate for all the tuple vars and gather their fields into `new_vars`.
    for (var_key, Var { name, span }) in ii.vars.iter() {
        if old_tuple_vars.contains(&var_key) {
            // Already split; skip it.
            continue;
        }

        let Some(var_ty) = ii.var_types.get(var_key) else {
            return Err(CompileError::Internal {
                msg: "missing var type in split_tuple_vars",
                span: span.clone(),
            });
        };

        if let Some(fields) = var_ty.get_tuple_fields() {
            // We now know we have a tuple var and its field types.
            for (field_idx, (opt_field_name, field_ty)) in fields.iter().enumerate() {
                // Always save the numeric index name, and optionally the symbolic name.
                let new_idx_name = format!("{name}.{field_idx}");
                let new_sym_name = opt_field_name
                    .as_ref()
                    .map(|field_name| format!("{name}.{field_name}"));
                new_vars.push(((new_idx_name, new_sym_name), span.clone(), field_ty.clone()));
            }

            old_tuple_vars.push(var_key);
        }
    }

    if new_vars.is_empty() {
        return Ok(false);
    }

    let mut new_tuple_vars = BTreeMap::new();

    // Add all the new vars to the intermediate intent and memo the new key.
    for ((idx_name, opt_sym_name), span, field_ty) in new_vars {
        // Prefer the symbolic name if it's there.
        let new_var_key = ii.vars.insert(Var {
            name: opt_sym_name.as_ref().unwrap_or(&idx_name).clone(),
            span,
        });
        ii.var_types.insert(new_var_key, field_ty);

        // Add both names to this map so we cover both potential uses below.
        new_tuple_vars.insert(idx_name, new_var_key);
        if let Some(sym_name) = opt_sym_name {
            new_tuple_vars.insert(sym_name, new_var_key);
        }
    }

    let mut new_accesses = Vec::new();

    // Iterate for each tuple access which is into a var and get the new var key.
    for (expr_key, expr) in ii.exprs.iter() {
        if let Expr::TupleFieldAccess { tuple, field, span } = expr {
            let mut push_new_access = |tuple_name: String| {
                // Work out the access name after the dot.
                let field_name = match field {
                    TupleAccess::Index(field_idx) => format!("{field_idx}"),
                    TupleAccess::Name(name) => name.name.clone(),
                    _ => unreachable!("tuple access error in split_tuple_vars()"),
                };

                let access_name = tuple_name + "." + &field_name;

                if let Some(new_tuple_var_key) = new_tuple_vars.get(&access_name) {
                    // We've matched this tuple field access with one of the new split vars we
                    // created above.  Mark this expr to be replaced.
                    new_accesses.push((expr_key, *new_tuple_var_key, span.clone()));
                }
            };

            match ii.exprs.get(*tuple) {
                Some(Expr::PathByKey(var_key, _)) => push_new_access(
                    ii.vars
                        .get(*var_key)
                        .expect("missing var in split_tuple_vars()")
                        .name
                        .clone(),
                ),
                Some(Expr::PathByName(path, _)) => push_new_access(path.clone()),
                _ => {}
            }
        }
    }

    // Replace all the old tuple accesses with new PathByKey exprs.
    for (old_expr_key, new_var_key, span) in new_accesses {
        let new_expr_key = ii.exprs.insert(Expr::PathByKey(new_var_key, span));
        ii.replace_exprs(old_expr_key, new_expr_key);
    }

    Ok(true)
}
