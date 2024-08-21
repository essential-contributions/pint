use crate::{
    expr::Expr,
    predicate::{Contract, StateKey},
};

pub(crate) fn dead_code_elimination(contract: &mut Contract) {
    dead_state_elimination(contract);
}

// TODO: Tests
// TODO: Documentation for function

// go through all the state exprs and see if they're used
// mark dead ones
// remove the dead ones in a separate loop after (you can change items with a mutable iterator, but not remove elements of the vector)
pub(crate) fn dead_state_elimination(contract: &mut Contract) {
    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        let mut live_paths: Vec<String> = Vec::new();
        for expr in contract.exprs(pred_key) {
            if let Expr::Path(name, _) = expr.get(contract) {
                live_paths.push(name.to_string());
            }
        }

        let pred_states = contract
            .preds
            .get(pred_key)
            .expect("pred guaranteed to exist")
            .states();
        let mut dead_state_decls: Vec<StateKey> = Vec::new();
        for state in pred_states {
            if !live_paths.contains(&state.1.name) {
                dead_state_decls.push(state.0);
            }
        }

        if let Some(pred) = contract.preds.get_mut(pred_key) {
            for dead_state in dead_state_decls {
                pred.states.remove(dead_state);
            }
        }
    }
}
