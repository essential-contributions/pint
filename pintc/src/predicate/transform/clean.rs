use crate::{expr::Expr, predicate::Contract};

// go through all the state exprs and see if they're used
// mark dead ones
// remove the dead ones in a separate loop after (you can change items with a mutable iterator, but not remove elements of the vector)
pub(crate) fn clean_dead_state_decls(contract: &mut Contract) {
    println!("contract: {:#?}", contract);

    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        let exprs = contract.exprs(pred_key);
        for state_expr_key in contract.exprs(pred_key) {
            if let Some(Expr::StorageAccess {
                name,
                mutable,
                span,
            }) = state_expr_key.try_get(contract)
            {}
        }
    }
}
