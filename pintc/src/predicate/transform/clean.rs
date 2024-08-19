use crate::{
    expr::Expr,
    predicate::{Contract, ExprKey, StateKey},
};

// go through all the state exprs and see if they're used
// mark dead ones
// remove the dead ones in a separate loop after (you can change items with a mutable iterator, but not remove elements of the vector)
pub(crate) fn clean_dead_state_decls(contract: &mut Contract) {
    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        println!("contract: {:#?}", contract);

        let mut live_paths: Vec<String> = Vec::new(); // TODO: consider a set
        for expr in contract.exprs(pred_key) {
            if let Expr::Path(name, _) = expr.get(contract) {
                live_paths.push(name.to_string());
            }
        }

        let pred_states = contract.preds.get(pred_key).unwrap().states();
        let mut dead_state_decls: Vec<StateKey> = Vec::new(); // also maybe set? need to iterate but order doesn't matter
        for state in pred_states {
            if !live_paths.contains(&state.1.name) {
                dead_state_decls.push(state.0);
            }
        }

        let mut state_expr: Option<ExprKey> = None;

        if let Some(pred) = contract.preds.get_mut(pred_key) {
            for dead_state in dead_state_decls {
                // if dead_state.get(pred)
                state_expr = Some(dead_state.get(pred).expr);

                pred.states.remove(dead_state);
            }
        }

        if let Some(expr) = state_expr {
            println!("expr: {}", contract.with_ctrct(expr));
        } // TODO: Find a way to deal with the states being mut. For now we are not ignoring them and will remove them if they are unused

        // contract.exprs iterator will give you the accessible exprs from the root
        // no need to recurse, we get back all the exprs
        // aka its the live expressions
        // it will not give you back a dead expr
        // just go through the exprs iterator and check if the path goes to the state variable
        // check it is in the symbols and then check that path is in the state
        // then check if the state is mutable -- although all that stuff should be handled by now
        // delete with .remove

        // add tests after ofc
        // make sure in the flattened version its gone
        // try using the states in all sorts of ways, not just a trivial use of state
        // try using state variables in other places than constraints
        // ex declare a state var and use it in the decl of a predicate instance
        // the address to specify and instance can be a state or state tuple field
    }
}
