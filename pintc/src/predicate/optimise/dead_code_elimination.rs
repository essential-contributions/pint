use fxhash::FxHashSet;

use crate::{
    error::Handler,
    expr::{evaluate::Evaluator, Expr},
    predicate::{ConstraintDecl, Contract, StateKey},
};

/// In a given contract, remove any code that is not reachable or used.
///
/// If an error occurs, the specific optimization process is aborted to ensure the contract remains functional.
pub(crate) fn dead_code_elimination(contract: &mut Contract) {
    dead_state_elimination(contract);
    dead_constraint_elimination(contract);
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
            .filter_map(|(state_key, state)| (!live_paths.contains(&state.name)).then(|| state_key))
            .collect::<FxHashSet<StateKey>>();

        if let Some(pred) = contract.preds.get_mut(pred_key) {
            for dead_state in dead_state_decls {
                pred.states.remove(dead_state);
            }
        }
    }
}

/// Remove all trivial Constraints in their respective predicates.
pub(crate) fn dead_constraint_elimination(contract: &mut Contract) {
    let evaluator = Evaluator::new(&contract.enums);
    let handler = Handler::default();

    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        if let Some(pred) = contract.preds.get(pred_key) {
            let live_constraints = pred
                .constraints
                .iter()
                .filter_map(|constraint| {
                    evaluator
                        .evaluate_key(&constraint.expr, &handler, &contract)
                        .is_err()
                        .then_some(constraint.clone())
                })
                .collect::<Vec<ConstraintDecl>>();

            if let Some(pred) = contract.preds.get_mut(pred_key) {
                pred.constraints = live_constraints;
            }
        }
    }
}
