use crate::{
    error::CompileError,
    expr::Immediate,
    intent::{
        intermediate::{Expr, Var},
        IntermediateIntent,
    },
    span::{empty_span, Spanned},
    types::{PrimitiveKind, Type},
};
use std::collections::HashMap;

/// Scalarize arrays by converting each array of size `n` into `n` new decision variable that
/// represent the individual elements of the array. The names of the individual elements are chosen
/// to be `<array-name>[<index>]`.
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
/// constraint a[2]: int;
/// ```
///
/// The above is not valid Yurt, of course, because the square brackets are not allowed in
/// identifiers, but internally, this is fine and helps make the loopkup quite easy.
pub(crate) fn scalarize(ii: &mut IntermediateIntent) -> Result<(), CompileError> {
    // First, convert decision variables that are arrays into `n` new decision variables that
    // represent the individual elements of the array, where `n` is the length of the array
    ii.vars
        .iter()
        .filter_map(|(key, var)| {
            // Only collect arrays
            if let Some(Type::Array { ty, range, span }) = &var.ty {
                Some((key, var.name.clone(), ty.clone(), *range, span.clone()))
            } else {
                None
            }
        })
        .collect::<Vec<_>>()
        .iter()
        .try_for_each(|(key, name, ty, range, span)| {
            if let Type::Primitive {
                kind: PrimitiveKind::Int | PrimitiveKind::Real | PrimitiveKind::Bool,
                ..
            } = &**ty
            {
                // Try to evaluate the length using compile-time evaluation
                // Index must be a strictly positive integer
                let range = ii.exprs.get(*range).expect("expr key guaranteed to exist");
                match range.evaluate(ii, &HashMap::new()) {
                    Ok(Immediate::Int(val)) if val > 0 => {
                        for i in 0..val {
                            ii.vars.insert(Var {
                                name: format!("{name}[{i}]"),
                                ty: Some(*ty.clone()),
                                span: span.clone(),
                            });
                        }
                        ii.vars.remove(*key);
                        Ok(())
                    }
                    Ok(_) => Err(CompileError::InvalidConstArrayLength {
                        span: range.span().clone(),
                    }),
                    _ => Err(CompileError::NonConstArrayLength {
                        span: range.span().clone(),
                    }),
                }
            } else {
                // Eventually, this will go away. Hence why it's an internal error for the time being
                Err(CompileError::Internal {
                    msg: "only arrays of ints, reals, and bools are currently supported",
                    span: empty_span(),
                })
            }
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
            let index = ii.exprs.get(*index).expect("expr key guaranteed to exist");
            let array = ii.exprs.get(*array).expect("expr key guaranteed to exist");
            macro_rules! handle_array_access {
                ($path: expr) => {{
                    // Try to evaluate the index using compile-time evaluation
                    // Index must be a non-negative integer
                    match &index.evaluate(ii, &HashMap::new()) {
                        Ok(Immediate::Int(val)) if *val >= 0 => {
                            *ii.exprs
                                .get_mut(*key)
                                .expect("key guaranteed to exist in the map!") =
                                Expr::PathByName(format!("{}[{val}]", $path), span.clone());
                            Ok(())
                        }
                        Ok(_) => Err(CompileError::InvalidConstArrayIndex {
                            span: index.span().clone(),
                        }),
                        _ => Err(CompileError::NonConstArrayIndex {
                            span: index.span().clone(),
                        }),
                    }
                }};
            }

            match &array {
                Expr::PathByName(path, _) => {
                    handle_array_access!(path)
                }
                Expr::PathByKey(path_key, _) => {
                    handle_array_access!(&ii.vars[*path_key].name)
                }
                // Now this does not catch paths that do not represent arrays just yet. That is,
                // you could still index into a variable of type `int` or even a type name. Once we
                // have a type checker and expressions hold types, then we can improve this check.
                _ => Err(CompileError::CannotIndexIntoValue {
                    span: array.span().clone(),
                    index_span: index.span().clone(),
                }),
            }
        })
}
