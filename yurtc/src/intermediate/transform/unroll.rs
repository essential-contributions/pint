use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::{BinaryOp, Ident, Immediate},
    intermediate::{Expr, ExprKey, IntermediateIntent},
    span::{empty_span, Span, Spanned},
    types::{PrimitiveKind, Type},
};
use itertools::Itertools;
use std::collections::{HashMap, HashSet};

/// Given an `IntermediateIntent`, a list of indices with their ranges `gen_ranges`, an optional
/// list of `conditions`, and a `forall` body, return a new expression that is the conjunction of
/// all the possible valid `forall` bodies.
///
/// For example,
/// ```yurt
/// forall i in 0..3, j in 0..3 where i <= j { a[i] != b[j] }
/// ```
///
/// becomes
///
/// ```yurt
///    a[0] != b[0]
/// && a[0] != b[1]
/// && a[0] != a[2]
/// && a[1] != b[1]
/// && a[1] != b[2]
/// && a[2] != b[2]
/// ```
fn unroll_forall(
    handler: &Handler,
    ii: &mut IntermediateIntent,
    gen_ranges: &Vec<(Ident, ExprKey)>,
    conditions: &Vec<ExprKey>,
    body: ExprKey,
    span: &Span,
) -> Result<Expr, ErrorEmitted> {
    // Sanity check
    let mut indices = HashSet::new();
    for range in gen_ranges {
        if !indices.insert(&range.0.name) {
            // The name is already in the HashSet, indicating a duplicate
            // Find the first occurance of the index
            let first_occurance = gen_ranges
                .iter()
                .find(|range_j| range.0.name == range_j.0.name)
                .expect("Index guaranteed to be in gen_ranges");

            // Error out
            return Err(handler.emit_err(Error::Compile {
                error: CompileError::DuplicateForAllIndex {
                    name: range.0.name.clone(),
                    span: range.0.span.clone(),
                    prev_span: first_occurance.0.span.clone(),
                },
            }));
        }
    }

    // Compute the Cartesian product of all the ranges
    let product_of_ranges = (0..gen_ranges.len())
        .map(|i| {
            match ii.exprs.get(gen_ranges[i].1).expect("guaranteed by parser") {
                Expr::Range { lb, ub, .. } => {
                    let lb = ii.exprs.get(*lb).expect("guaranteed by parser");
                    let ub = ii.exprs.get(*ub).expect("guaranteed by parser");
                    match (lb, ub) {
                        // Only support integer literals as bounds, for now
                        (
                            Expr::Immediate {
                                value: Immediate::Int(lb),
                                ..
                            },
                            Expr::Immediate {
                                value: Immediate::Int(ub),
                                ..
                            },
                        ) => Ok(*lb..=*ub),
                        _ if !matches!(
                            lb,
                            Expr::Immediate {
                                value: Immediate::Int(_),
                                ..
                            }
                        ) =>
                        {
                            // Bad lower bound
                            Err(handler.emit_err(Error::Compile {
                                error: CompileError::InvalidForAllIndexBound {
                                    name: gen_ranges[i].0.name.clone(),
                                    span: lb.span().clone(),
                                },
                            }))
                        }
                        _ => {
                            // Bad upper bound
                            Err(handler.emit_err(Error::Compile {
                                error: CompileError::InvalidForAllIndexBound {
                                    name: gen_ranges[i].0.name.clone(),
                                    span: ub.span().clone(),
                                },
                            }))
                        }
                    }
                }
                _ => panic!("guaranteed by the parser"),
            }
        })
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .multi_cartesian_product();

    // Collect the names of all the indices
    let indices = gen_ranges
        .iter()
        .map(|(index, _)| index.clone())
        .collect::<Vec<_>>();

    // Generate a new expression that is the conjunction of all the unrolled expressions. Consider
    // the following:
    // 1. All possible combinations of the indices given `gen_ranges`: this is tracked in
    //    `product_of_ranges`.
    // 2. All the conditions which restrict the possible index combinations.

    // Base value = `true`
    let mut unrolled = Expr::Immediate {
        value: Immediate::Bool(true),
        span: span.clone(),
    };

    for values in product_of_ranges {
        // Build a map from indicies to their concrete values
        let values_map = indices
            .iter()
            .zip(values.iter())
            .map(|(index, int_index)| ("::".to_owned() + &index.name, Immediate::Int(*int_index)))
            .collect::<HashMap<_, _>>();

        // Check each condition, if available, against the values map above
        let mut satisfied = true;
        for condition in conditions {
            match ii
                .exprs
                .get(*condition)
                .expect("guaranteed by parser")
                .evaluate(handler, ii, &values_map)?
            {
                Immediate::Bool(false) => {
                    satisfied = false;
                    break;
                }
                Immediate::Bool(true) => {}
                _ => {
                    return Err(handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "type error: boolean expression expected",
                            span: empty_span(),
                        },
                    }))
                }
            }
        }

        // If all conditions are satisifed (or if none are present), then update the resulting
        // expression by ANDing it with the newly unrolled `forall` body.
        if satisfied {
            let lhs = ii.exprs.insert(unrolled.clone());
            ii.expr_types.insert(
                lhs,
                Type::Primitive {
                    kind: PrimitiveKind::Bool,
                    span: span.clone(),
                },
            );
            unrolled = Expr::BinaryOp {
                op: BinaryOp::LogicalAnd,
                lhs,
                rhs: body.plug_in(ii, &values_map),
                span: span.clone(),
            };
        }
    }

    Ok(unrolled)
}

/// Given an `IntermediateIntent`, unroll all `forall` expressions and replace them in the
/// expressions map with their unrolled version
pub(crate) fn unroll_foralls(
    handler: &Handler,
    ii: &mut IntermediateIntent,
) -> Result<(), ErrorEmitted> {
    ii.exprs
        .iter()
        .filter_map(|(key, expr)| {
            // Only collect foralls
            if let Expr::ForAll {
                gen_ranges,
                conditions,
                body,
                span,
            } = expr
            {
                Some((
                    key,
                    gen_ranges.clone(),
                    conditions.clone(),
                    *body,
                    span.clone(),
                ))
            } else {
                None
            }
        })
        .collect::<Vec<_>>()
        .iter()
        .try_for_each(|(key, gen_ranges, conditions, body, span)| {
            // Update the key of the `forall` to map to the new unrolled expression (the big
            // conjunction)
            *ii.exprs
                .get_mut(*key)
                .expect("key guaranteed to exist in the map!") =
                unroll_forall(handler, ii, gen_ranges, conditions, *body, span)?;

            // Clean up all keys used by the `forall` now that it's gone
            gen_ranges
                .iter()
                .for_each(|(_, expr)| ii.remove_expr(*expr));
            conditions.iter().for_each(|expr| ii.remove_expr(*expr));
            ii.remove_expr(*body);

            Ok(())
        })
}
