use fxhash::FxHashSet;

use crate::{
    expr::Expr,
    predicate::{Contract, StateKey},
};

/// In a given contract, remove any code that is not reachable or used.
pub(crate) fn dead_code_elimination(contract: &mut Contract) {
    dead_state_elimination(contract);
}

/// Remove all unused States in their respective predicates.
pub(crate) fn dead_state_elimination(contract: &mut Contract) {
    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        let live_paths = contract
            .exprs(pred_key)
            .filter_map(|expr| {
                if let Expr::Path(name, _) = expr.get(contract) {
                    return Some(name.to_string());
                }
                None
            })
            .collect::<FxHashSet<String>>();

        let pred_states = contract
            .preds
            .get(pred_key)
            .expect("pred guaranteed to exist")
            .states();

        let dead_state_decls = pred_states
            .filter_map(|(state_key, state)| {
                if !live_paths.contains(&state.name) {
                    return Some(state_key);
                }
                None
            })
            .collect::<FxHashSet<StateKey>>();

        if let Some(pred) = contract.preds.get_mut(pred_key) {
            for dead_state in dead_state_decls {
                pred.states.remove(dead_state);
            }
        }
    }
}
