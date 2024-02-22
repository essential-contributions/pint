use crate::{
    error::CompileError,
    expr::Immediate,
    intermediate::{Expr, ExprKey, IntermediateIntent, Var, VarKey},
    span::{empty_span, Span, Spanned},
    types::{Path, PrimitiveKind, Type},
};
use std::collections::{HashMap, HashSet};

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
/// identifiers, but internally, this is fine and helps make the loopkup quite easy.
fn scalarize_array(
    ii: &mut IntermediateIntent,
    key: VarKey,
    name: &String,
    ty: &Type,
    range: ExprKey,
    span: &Span,
) -> Result<(), CompileError> {
    match ty {
        Type::Array { .. }
        | Type::Primitive {
            kind: PrimitiveKind::Int | PrimitiveKind::Real | PrimitiveKind::Bool,
            ..
        } => {
            let range = ii.exprs.get(range).expect("expr key guaranteed to exist");
            println!("range value: {:?}", range);
            match range.evaluate(ii, &HashMap::new()) {
                Ok(Immediate::Int(val)) if val > 0 => {
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
    valid_paths: &HashSet<String>,
) -> Result<Path, CompileError> {
    let index = ii.exprs.get(index).expect("expr key guaranteed to exist");
    println!("index: {:?}", index);
    let index_value = index.evaluate(ii, &HashMap::new());
    let index_span = index.span().clone();
    let array = ii.exprs.get(array).expect("expr key guaranteed to exist");
    println!("array: {:?}", array);
    macro_rules! handle_array_access {
        ($path: expr) => {{
            // Try to evaluate the index using compile-time evaluation
            // Index must be a non-negative integer
            match &index_value {
                Ok(Immediate::Int(val)) if *val >= 0 => {
                    // TODO: Check here if there is a valid val for the array path
                    // Maybe store the upper and lower bound while we scalarize? Is that too much of a performance hit?
                    // Should I instead look through the entire intermediate intent for the proper path and see if the val exists?
                    // Or maybe if the val is above or below the lowest. Though that would take a full search regardless

                    // here I have the path
                    // ex. ::a[1]
                    // and the next index
                    // ex. immediate 1
                    // so if I store things by path name I can then check the bound on the next index?
                    // issue right now is I don't know what dimension I'm on so I don't know what to check
                    // could keep a counter that says what dimension to check against?
                    println!("val: {:?}", val);
                    let path = format!("{}[{val}]", $path);
                    println!("path: {:?}", path);

                    if !&valid_paths.contains(&path) {
                        println!("path is not valid");
                        return Err(CompileError::ArrayIndexOutOfBounds { span: index_span });
                    } else {
                        println!("path is valid");
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
        Expr::PathByName(path, _) => {
            println!("path by name found");
            handle_array_access!(path)
        }
        Expr::PathByKey(path_key, _) => {
            println!("path by key found");
            handle_array_access!(&ii.vars[*path_key].name)
        }
        Expr::ArrayElementAccess {
            array: array_inner,
            index: index_inner,
            ..
        } => {
            handle_array_access!(scalarize_array_access(
                ii,
                key,
                *array_inner,
                *index_inner,
                span,
                valid_paths,
            )?)
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
/// constraint a[2]: int; // here, `a[2]` is an `Expr::PathByName( .. )`
/// ```
///
/// The above is not valid Yurt, of course, because the square brackets are not allowed in
/// identifiers, but internally, this is fine and helps make the loopkup quite easy.
pub(crate) fn scalarize(ii: &mut IntermediateIntent) -> Result<(), CompileError> {
    fn get_array_params(ary_ty: &Type) -> Option<(Type, ExprKey, Span)> {
        match ary_ty {
            Type::Alias { ty, .. } => get_array_params(ty),
            Type::Array { ty, range, span } => Some((*ty.clone(), *range, span.clone())),
            _ => None,
        }
    }

    // println!("intermediate intent pre scalarize: {:?}", ii);

    let array_vars = ii
        .vars
        .iter()
        .filter_map(|(key, var)| {
            // Only collect arrays
            ii.var_types.get(key).and_then(|var_ty| {
                get_array_params(var_ty).map(|ary_params| (key, var.name.clone(), ary_params))
            })
        })
        .collect::<Vec<_>>();

    // Print the full collection
    println!("array vars: {:?}", array_vars);

    array_vars
        .iter()
        .try_for_each(|(key, name, (ty, range, span))| {
            println!("range: {:?}", range);
            scalarize_array(ii, *key, name, ty, *range, span)
        })?;

    // // First, convert decision variables that are arrays into `n` new decision variables that
    // // represent the individual elements of the array, where `n` is the length of the array
    // ii.vars
    //     .iter()
    //     .filter_map(|(key, var)| {
    //         // Only collect arrays
    //         ii.var_types.get(key).and_then(|var_ty| {
    //             get_array_params(var_ty).map(|ary_params| (key, var.name.clone(), ary_params))
    //         })
    //     })
    //     .collect::<Vec<_>>()
    //     .iter()
    //     .try_for_each(|(key, name, (ty, range, span))| {
    //         println!("range: {:?}", range);
    //         scalarize_array(ii, *key, name, ty, *range, span)
    //     })?;

    println!("intermediate intent post scalarize: {:#?}", ii);

    let mut valid_paths = HashSet::new();

    let yeet = ii.vars.iter();
    // println!("yeet: {:#?}", yeet);

    for slot in yeet {
        println!("slot: {:?}", slot);
    }

    for (_, value) in ii.vars.iter() {
        let path = value.name.clone();
        println!("value: {:?}", value);
        println!("name: {:?}", value.clone().name);
        valid_paths.insert(path.clone());

        // Insert all possible partial paths
        let mut partial_path = String::new();
        for part in path.split('[') {
            partial_path.push_str(part);
            valid_paths.insert(partial_path.clone());
            partial_path.push('[');
        }
    }

    println!("valid_paths: {:#?}", valid_paths);

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
            scalarize_array_access(ii, *key, *array, *index, span, &valid_paths)?;
            //dbg!(ii.exprs.get(*key).unwrap());
            Ok(())
        })
}

// array_vars is an array of everything we need
// make function that extracts the dimensions
// key can be the path?
// how to handle multidimensional?
// basically I need to be able to check ::a[1] and say is that too big?
// or ::a[2][1]
// so how should this be stored? I do think it should be stored vs. going to read from it each time
// scalarizing checks in order of dimensions -- ex.
/* path: "::a[1]"
path: "::a[1][2]"
path: "::a[1][2][1]"
path: "::a[2]"
path: "::a[2][2]"
path: "::a[2][2][1]"
path: "::b[2]"
path: "::b[2][1]"
 */
// so checks should be easy one by one
// don't need to have more than one dimension ready to check against at anytime
// if i store the paths by max dimension I could do a check that way?
// what if I extract to a hashmap?
// key : ::a
// doesn't work because its always appended to with dimension
// array is the pathbyname ex. ::a or ::a[1], each is it's own array
// I can stuff in every single path into a vec or a set and see if it exists.
// if it doesn't then it is out of bounds. Performance hit for sure, can chat
// with Mohammad about it tomorrow
