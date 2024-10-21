use fxhash::{FxHashMap, FxHashSet};

use crate::{
    error::Handler,
    expr::{evaluate::Evaluator, BinaryOp, Expr, Immediate},
    predicate::{ConstraintDecl, Contract, ExprKey, PredKey, StateKey},
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
    dead_thing_elimination(contract);
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
/// If any constraint evaluates to false, all constraints are removed and replaced with a single
/// instance of `constraint false`
pub(crate) fn dead_constraint_elimination(handler: &Handler, contract: &mut Contract) {
    let evaluator = Evaluator::new(contract);

    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
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

// TODO: Documentation
// TODO: Change name
// TODO: handle either lhs or rhs
// Goal: Transform any constraint matching the following (regardless of const on rhs or lhs):
// true || <expr> is true -- done
// true && <expr> is <expr>
// false || <expr> is <expr> -- done
// false && <expr> is <false>
// 1. check constraint expr_key is binary op
// 2. check binary op is LogicalAnd or LogicalOr
// 3. evaluate each side of the binary op to see if one is an immediate bool
// 4. replace expr_key in constraint decl with the appropriate side of the binary op
// TODO: Need to make this pass independent of the others, so likely have to simplify true and false constraints
pub(crate) fn dead_thing_elimination(contract: &mut Contract) {
    let evaluator = Evaluator::new(contract);
    let mut constraints_to_evaluate: Vec<(usize, ExprKey, PredKey)> = vec![];

    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        if let Some(pred) = contract.preds.get(pred_key) {
            constraints_to_evaluate = pred
                .constraints
                .iter()
                .enumerate()
                .filter_map(|(i, constraint)| {
                    let expr = constraint.expr.get(contract);

                    println!("expr: {:#?}", expr);

                    if let Expr::BinaryOp { op, lhs, rhs, span } = expr {
                        println!("found that op");

                        let lhs_imm = evaluator.evaluate_key(&lhs, &Handler::default(), contract);
                        let rhs_imm = evaluator.evaluate_key(&rhs, &Handler::default(), contract);

                        match op {
                            BinaryOp::LogicalAnd => {
                                println!("and found");
                                if let Ok(Immediate::Bool(true)) = lhs_imm {
                                    println!("true found");
                                    return Some((i, rhs.clone(), pred_key));
                                } else if let Ok(Immediate::Bool(false)) = lhs_imm {
                                    println!("false found");
                                    return Some((i, lhs.clone(), pred_key));
                                } else {
                                    None
                                }
                            }
                            BinaryOp::LogicalOr => {
                                println!("or found");
                                if let Ok(Immediate::Bool(true)) = lhs_imm {
                                    println!("true found");
                                    return Some((i, lhs.clone(), pred_key));
                                } else if let Ok(Immediate::Bool(false)) = lhs_imm {
                                    println!("false found");
                                    return Some((i, rhs.clone(), pred_key));
                                } else {
                                    None
                                }
                            }
                            _ => {
                                println!("other op found");
                                return None;
                            }
                        }
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();

            if let Some(pred) = contract.preds.get_mut(pred_key) {
                constraints_to_evaluate.iter().for_each(|(i, new_expr, _)| {
                    let constraint = pred
                        .constraints
                        .get_mut(*i)
                        .expect("test, guaranteed to exist");
                    constraint.expr = *new_expr;
                });
            }
        }
    }
}
