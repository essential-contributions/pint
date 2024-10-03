use fxhash::FxHashMap;

use crate::{
    error::Handler,
    expr::{evaluate::Evaluator, Expr},
    predicate::{Contract, ExprKey},
    span::Spanned,
    types::Type,
};

/// In a given contract, simplify all sub-expressions that evaluate to constants.
pub(crate) fn const_folding(contract: &mut Contract) {
    fold_consts(contract);
}

pub(crate) fn fold_consts(contract: &mut Contract) {
    let mut replace_map: FxHashMap<ExprKey, (Expr, Type)> = FxHashMap::default();

    let evaluator = Evaluator::new(&contract.enums);

    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        for expr_key in contract.exprs(pred_key) {
            if let Ok(imm) = evaluator.evaluate_key(&expr_key, &Handler::default(), contract) {
                // check if the imm is different or not -- how to do?
                // currently replacing every single expr even if it is the same -- not ideal, lots of unnecessary dupes

                let simplified_expr = Expr::Immediate {
                    value: imm.clone(),
                    span: expr_key.get(contract).span().clone(),
                };
                replace_map.insert(
                    expr_key,
                    (simplified_expr, expr_key.get_ty(contract).clone()),
                );
            }
        }

        for (old_expr_key, (simplified_expr, simplified_type)) in &replace_map {
            let simplified_expr_key = contract
                .exprs
                .insert(simplified_expr.clone(), simplified_type.clone());

            println!(
                "replacing --- \nold expr: {}\nwith\nnew expr: {}\n--- ",
                contract.with_ctrct(old_expr_key),
                contract.with_ctrct(simplified_expr_key),
            );

            contract.replace_exprs(Some(pred_key), *old_expr_key, simplified_expr_key);
        }
    }
}
