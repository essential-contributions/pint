use fxhash::FxHashSet;

use crate::{
    error::{Handler, WarningEmitted},
    expr::{evaluate::Evaluator, Expr, Immediate},
    predicate::{ConstraintDecl, Contract, StateKey},
    span::empty_span,
    warning::Warning,
};

/// In a given contract, remove any code that is not reachable or used.
///
/// If an error occurs, the specific optimization process is aborted to ensure the contract remains functional.
pub(crate) fn dead_code_elimination(
    contract: &mut Contract,
    handler: &Handler,
) -> Result<(), WarningEmitted> {
    dead_state_elimination(contract);
    dead_constraint_elimination(contract, handler);

    if handler.has_warnings() {
        Err(handler.continue_with_warning())
    } else {
        Ok(())
    }
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
/// If any constraint evaluates to false, all constraints are removed and replaced with a single instance of `constraint false`
pub(crate) fn dead_constraint_elimination(contract: &mut Contract, handler: &Handler) {
    let evaluator = Evaluator::new(&contract.enums);

    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        if let Some(pred) = contract.preds.get(pred_key) {
            let dead_constraints = pred
                .constraints
                .iter()
                .enumerate()
                .filter_map(|(i, constraint)| {
                    // If the evaluator succeeds, we're only expecting true or false. If it doesn't then we don't care about the constraint.
                    // We also don't care about the errors emitted by the evaluator
                    if let Ok(Immediate::Bool(b)) =
                        evaluator.evaluate_key(&constraint.expr, &Handler::default(), contract)
                    {
                        // TODO: Refactor, this is just to test ----
                        if !b {
                            handler.emit_warn(Warning::AlwaysFalseConstraint {
                                span: constraint.span.clone(),
                            });
                        } else {
                            handler.emit_warn(Warning::TrivialConstraint {
                                span: constraint.span.clone(),
                            });
                        }
                        // ---
                        Some((i, b))
                    } else {
                        None
                    }
                })
                .collect::<Vec<(usize, bool)>>();

            if dead_constraints.iter().any(|(_, b)| !(*b)) {
                // replace all constraints with one `constraint false`
                if let Some(pred) = contract.preds.get_mut(pred_key) {
                    pred.constraints = vec![ConstraintDecl {
                        expr: contract.exprs.insert_bool(false),
                        // ideally we would collect the spans of all the constraints, but we don't have the ability to do that right now
                        span: empty_span(),
                    }]
                }
            } else {
                // retain only useful constraints
                if let Some(pred) = contract.preds.get_mut(pred_key) {
                    // Remove dead constraints in reverse to avoid removing the wrong indices from shifting elements
                    // This assumes dead_constraints is sorted, which it is based on how it is collected above
                    dead_constraints.iter().rev().for_each(|(i, _)| {
                        pred.constraints.remove(*i);
                    });
                }
            }
        }
    }
}
