use fxhash::{FxHashMap, FxHashSet};

use crate::{
    error::Handler,
    expr::{evaluate::Evaluator, BinaryOp, Expr, Immediate},
    predicate::{ConstraintDecl, Contract, ExprKey, StateKey},
    span::empty_span,
    warning::Warning,
};

/// In a given contract, remove any code that is not reachable or used.
///
/// If an error occurs, the specific optimization process is aborted to ensure the contract remains
/// functional.
pub(crate) fn dead_code_elimination(handler: &Handler, contract: &mut Contract) {
    dead_state_elimination(contract);
    dead_constraint_elimination(handler, contract);
    dead_select_elimination(contract);
}

/// Remove all unused States in their respective predicates.
pub(crate) fn dead_state_elimination(contract: &mut Contract) {
    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        let live_paths = contract
            .exprs(pred_key)
            .filter_map(|expr| {
                if let Expr::Path(name, _) = expr.get(contract) {
                    Some(name.to_string())
                } else {
                    None
                }
            })
            .collect::<FxHashSet<String>>();

        let pred_states = contract
            .preds
            .get(pred_key)
            .expect("pred guaranteed to exist")
            .states();

        let dead_state_decls = pred_states
            .filter(|&(_, state)| (!live_paths.contains(&state.name)))
            .map(|(state_key, _)| state_key)
            .collect::<FxHashSet<StateKey>>();

        if let Some(pred) = contract.preds.get_mut(pred_key) {
            for dead_state in dead_state_decls {
                pred.states.remove(dead_state);
            }
        }
    }
}

/// Remove all trivial Constraints in their respective predicates.
///
/// Simplify any constraint conditions that consist of constant binary operations. Ex:
/// true || <expr> is true
/// true && <expr> is <expr>
/// false || <expr> is <expr>
/// false && <expr> is <false>
///
/// If any constraint evaluates to false, all constraints are removed and replaced with a single
/// instance of `constraint false`
pub(crate) fn dead_constraint_elimination(handler: &Handler, contract: &mut Contract) {
    let evaluator = Evaluator::new(contract);

    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        // Simplify constant binary operation conditions
        if let Some(pred) = contract.preds.get(pred_key) {
            let constraints_to_evaluate = pred
                .constraints
                .iter()
                .enumerate()
                .filter_map(|(i, constraint)| {
                    let expr = constraint.expr.get(contract);

                    if let Expr::BinaryOp { op, lhs, rhs, .. } = expr {
                        let lhs_imm = evaluator.evaluate_key(lhs, &Handler::default(), contract);
                        let rhs_imm = evaluator.evaluate_key(rhs, &Handler::default(), contract);

                        match op {
                            BinaryOp::LogicalAnd => match (lhs_imm, rhs_imm) {
                                (Ok(Immediate::Bool(true)), Err(_)) => {
                                    return Some((i, *rhs));
                                }

                                (Err(_), Ok(Immediate::Bool(true))) => {
                                    return Some((i, *lhs));
                                }

                                (Ok(Immediate::Bool(false)), Err(_)) => {
                                    return Some((i, *lhs));
                                }

                                (Err(_), Ok(Immediate::Bool(false))) => {
                                    return Some((i, *rhs));
                                }

                                _ => None,
                            },

                            BinaryOp::LogicalOr => match (lhs_imm, rhs_imm) {
                                (Ok(Immediate::Bool(true)), Err(_)) => {
                                    return Some((i, *lhs));
                                }

                                (Err(_), Ok(Immediate::Bool(true))) => {
                                    return Some((i, *rhs));
                                }

                                (Ok(Immediate::Bool(false)), Err(_)) => {
                                    return Some((i, *rhs));
                                }

                                (Err(_), Ok(Immediate::Bool(false))) => {
                                    return Some((i, *lhs));
                                }

                                _ => None,
                            },

                            _ => {
                                return None;
                            }
                        }
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();

            if let Some(pred) = contract.preds.get_mut(pred_key) {
                constraints_to_evaluate.iter().for_each(|(i, new_expr)| {
                    if let Some(constraint) = pred.constraints.get_mut(*i) {
                        constraint.expr = *new_expr;
                    }
                });
            }
        }

        // Evaluate for and remove trivial constraints
        if let Some(pred) = contract.preds.get(pred_key) {
            let mut has_false_constraint = false;

            let dead_constraints = pred
                .constraints
                .iter()
                .enumerate()
                .filter_map(|(i, constraint)| {
                    // If the evaluator succeeds, we're only expecting true or false. If it doesn't
                    // then we don't care about the constraint.
                    //
                    // We also don't care about the errors emitted by the evaluator
                    if let Ok(Immediate::Bool(b)) =
                        evaluator.evaluate_key(&constraint.expr, &Handler::default(), contract)
                    {
                        if !b {
                            has_false_constraint = true;
                            handler.emit_warn(Warning::AlwaysFalseConstraint {
                                span: constraint.span.clone(),
                            });
                        }

                        Some(i)
                    } else {
                        None
                    }
                })
                .collect::<Vec<usize>>();

            if has_false_constraint {
                // replace all constraints with one `constraint false`
                if let Some(pred) = contract.preds.get_mut(pred_key) {
                    pred.constraints = vec![ConstraintDecl {
                        expr: contract.exprs.insert_bool(false),
                        // ideally we would collect the spans of all the constraints, but we don't
                        // have the ability to do that right now
                        span: empty_span(),
                    }]
                }
            } else {
                // retain only useful constraints
                if let Some(pred) = contract.preds.get_mut(pred_key) {
                    // Remove dead constraints in reverse to avoid removing the wrong indices from
                    // shifting elements. This assumes dead_constraints is sorted, which it is based
                    // on how it is collected above
                    dead_constraints.iter().rev().for_each(|i| {
                        pred.constraints.remove(*i);
                    });
                }
            }
        }
    }
}

/// Remove all trivial Select expressions in their respective predicates.
///
/// If any select condition is a const the appropriate branch replaces the select expression
pub(crate) fn dead_select_elimination(contract: &mut Contract) {
    let evaluator = Evaluator::new(contract);
    let mut replace_map: FxHashMap<ExprKey /* select */, ExprKey /* branch */> =
        FxHashMap::default();

    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        for expr_key in contract.exprs(pred_key) {
            if let Expr::Select {
                condition,
                then_expr,
                else_expr,
                ..
            } = expr_key.get(contract)
            {
                if let Ok(Immediate::Bool(b)) =
                    evaluator.evaluate_key(condition, &Handler::default(), contract)
                {
                    if b {
                        replace_map.insert(expr_key, *then_expr);
                    } else {
                        replace_map.insert(expr_key, *else_expr);
                    }
                }
            }
        }

        for (select_expr, branch_expr) in &replace_map {
            contract.replace_exprs(Some(pred_key), *select_expr, *branch_expr);
        }
    }
}
