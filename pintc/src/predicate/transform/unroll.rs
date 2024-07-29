use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::{evaluate::Evaluator, BinaryOp, GeneratorKind, Immediate},
    predicate::{Contract, Expr, ExprKey, VisitorKind},
    span::{empty_span, Spanned},
    types::{PrimitiveKind, Type},
};
use fxhash::FxHashMap;
use itertools::Itertools;
use std::collections::HashSet;

/// Given an `Predicate`, and a generator expression containing a list of indices with
/// their ranges `gen_ranges`, an optional list of `conditions`, and a body, return a new
/// expression that is the conjunction or disjunction of all the possible valid generator bodies.
///
/// For example,
/// ```pint
/// forall i in 0..3, j in 0..3 where i <= j { a[i] != b[j] }
/// ```
///
/// becomes
///
/// ```pint
/// true
/// && a[0] != b[0]
/// && a[0] != b[1]
/// && a[0] != a[2]
/// && a[1] != b[1]
/// && a[1] != b[2]
/// && a[2] != b[2]
/// ```
fn unroll_generator(
    handler: &Handler,
    contract: &mut Contract,
    generator: Expr,
) -> Result<ExprKey, ErrorEmitted> {
    let Expr::Generator {
        kind,
        gen_ranges,
        conditions,
        body,
        span,
    } = generator
    else {
        unreachable!("only Expr::Generator is passed here")
    };

    // Sanity check
    let mut indices = HashSet::new();
    for range in &gen_ranges {
        if !indices.insert(&range.0.name) {
            // The name is already in the HashSet, indicating a duplicate
            // Find the first occurance of the index
            let first_occurance = gen_ranges
                .iter()
                .find(|range_j| range.0.name == range_j.0.name)
                .expect("Index guaranteed to be in gen_ranges");

            // Error out
            return Err(handler.emit_err(Error::Compile {
                error: CompileError::DuplicateGeneratorIndex {
                    name: range.0.name.clone(),
                    gen_kind: kind.to_string(),
                    span: range.0.span.clone(),
                    prev_span: first_occurance.0.span.clone(),
                },
            }));
        }
    }

    // Compute the Cartesian product of all the ranges
    let product_of_ranges = gen_ranges
        .iter()
        .map(|range| {
            match range.1.get(contract) {
                Expr::Range { lb, ub, .. } => {
                    let lb = lb.get(contract);
                    let ub = ub.get(contract);
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
                                error: CompileError::InvalidGeneratorIndexBound {
                                    name: range.0.name.clone(),
                                    gen_kind: kind.to_string(),
                                    span: lb.span().clone(),
                                },
                            }))
                        }
                        _ => {
                            // Bad upper bound
                            Err(handler.emit_err(Error::Compile {
                                error: CompileError::InvalidGeneratorIndexBound {
                                    name: range.0.name.clone(),
                                    gen_kind: kind.to_string(),
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
        .into_iter()
        .map(|(index, _)| index)
        .collect::<Vec<_>>();

    // Generate a new expression that is the conjunction or disjunction of all the unrolled
    // expressions. Consider the following:
    // 1. All possible combinations of the indices given `gen_ranges`: this is tracked in
    //    `product_of_ranges`.
    // 2. All the conditions which restrict the possible index combinations.

    let bool_ty = Type::Primitive {
        kind: PrimitiveKind::Bool,
        span: span.clone(),
    };

    // Base value = `true` for foralls and `false` for exists
    let mut unrolled = contract.exprs.insert(
        Expr::Immediate {
            value: Immediate::Bool(match kind {
                GeneratorKind::ForAll => true,
                GeneratorKind::Exists => false,
            }),
            span: span.clone(),
        },
        bool_ty.clone(),
    );

    for values in product_of_ranges {
        // Build a map from indices to their concrete values
        let values_map = indices
            .iter()
            .zip(values.iter())
            .map(|(index, int_index)| ("::".to_owned() + &index.name, Immediate::Int(*int_index)))
            .collect::<FxHashMap<_, _>>();

        let mut satisfied = true;
        {
            let evaluator = Evaluator::from_values(&contract.enums, values_map.clone());

            // Check each condition, if available, against the values map above
            for condition in &conditions {
                match evaluator.evaluate_key(condition, handler, contract)? {
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
        }

        // If all conditions are satisifed (or if none are present), then update the resulting
        // expression by joining it with the newly unrolled generator body.
        if satisfied {
            let rhs = body.plug_in(contract, &values_map);
            unrolled = contract.exprs.insert(
                Expr::BinaryOp {
                    op: match kind {
                        GeneratorKind::ForAll => BinaryOp::LogicalAnd,
                        GeneratorKind::Exists => BinaryOp::LogicalOr,
                    },
                    lhs: unrolled,
                    rhs,
                    span: span.clone(),
                },
                bool_ty.clone(),
            );
        }
    }

    Ok(unrolled)
}

/// Given an `Predicate`, unroll all generator expressions and replace them in the
/// expressions map with their unrolled version
pub(crate) fn unroll_generators(
    handler: &Handler,
    contract: &mut Contract,
) -> Result<(), ErrorEmitted> {
    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        // Perform a depth first iteration of all expressions searching for generators, ensuring that
        // dependencies are detected.
        //
        // With nested generators, e.g.,
        //     forall i in xs {
        //         forall j in ys {
        //             ...
        //         }
        //     }
        // by explicitly going depth first and child-before-parent we'll always encounter the inner
        // generator (`j in ys`) before the outer (`i in xs`).

        let mut generators = Vec::new();
        contract.visitor(
            pred_key,
            VisitorKind::DepthFirstChildrenBeforeParents,
            |expr_key: ExprKey, expr: &Expr| {
                // Only collect generators and only by key since unrolling nested generators will
                // update the outer generators.
                if matches!(expr, Expr::Generator { .. }) {
                    generators.push(expr_key);
                }
            },
        );

        for old_generator_key in generators {
            // On success, update the key of the generator to map to the new unrolled expression.
            let generator = old_generator_key.get(contract).clone();
            if let Ok(unrolled_generator_key) =
                unroll_generator(handler, contract, generator)
            {
                contract.replace_exprs(pred_key, old_generator_key, unrolled_generator_key);
            }
        }

        if handler.has_errors() {
            return Err(handler.cancel());
        }
    }

    Ok(())
}
