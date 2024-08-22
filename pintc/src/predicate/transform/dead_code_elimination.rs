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
        let mut live_paths: FxHashSet<String> = FxHashSet::default();
        for expr in contract.exprs(pred_key) {
            if let Expr::Path(name, _) = expr.get(contract) {
                live_paths.insert(name.to_string());
            }
        }

        let pred_states = contract
            .preds
            .get(pred_key)
            .expect("pred guaranteed to exist")
            .states();
        let mut dead_state_decls: FxHashSet<StateKey> = FxHashSet::default();
        for (state_key, state) in pred_states {
            if !live_paths.contains(&state.name) {
                dead_state_decls.insert(state_key);
            }
        }

        if let Some(pred) = contract.preds.get_mut(pred_key) {
            for dead_state in dead_state_decls {
                pred.states.remove(dead_state);
            }
        }
    }
}
