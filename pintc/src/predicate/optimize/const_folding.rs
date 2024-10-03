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
                // probably not the issue we're facing though, it should still be able to be replaced

                // this will solve the issue though --

                let simplified_expr = Expr::Immediate {
                    value: imm.clone(),
                    span: expr_key.get(contract).span().clone(),
                };

                // let simplified_type = imm.get_ty(Some(&expr_key.get(contract).span().clone())); // assigning NONE to the range, if an array
                // causing a panic when the asm builder tries to access the rang
                // could clone the original key's type, but would that break stuff?
                // Does simplifying to an immediate, ever change the type of the expr?
                // -- confirmed it does not break any tests. So simplifying the expr never alters the type

                let simplified_type = expr_key.get_ty(contract);

                // println!("simplified type: {:#?}", simplified_type);

                replace_map.insert(expr_key, (simplified_expr, simplified_type.clone()));
            }
        }

        for (old_expr_key, (simplified_expr, simplified_type)) in &replace_map {
            let simplified_expr_key = contract
                .exprs
                .insert(simplified_expr.clone(), simplified_type.clone());

            // println!(
            //     "replacing --- \nold expr: {}\nold expr ty: {:#?}\nwith\nnew expr: {}\n new expr ty: {:#?}\n--- ",
            //     contract.with_ctrct(old_expr_key),
            //     old_expr_key.get_ty(contract),
            //     contract.with_ctrct(simplified_expr_key),
            //     simplified_type,
            // );

            contract.replace_exprs(Some(pred_key), *old_expr_key, simplified_expr_key);

            let new_test_ty = simplified_expr_key.get_ty(contract);
            // println!("new_test_ty: {:#?}", new_test_ty);
            // println!(
            //     "new_test_ty size: {:#?}",
            //     new_test_ty.size(&Handler::default(), contract)
            // );
        }
    }
}
