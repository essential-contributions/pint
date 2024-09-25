use crate::{
    error::Handler,
    expr::{evaluate::Evaluator, Expr},
    predicate::Contract,
};

// todo - ian, add documentation
/// In a given contract, ...
pub(crate) fn simplify_exprs(contract: &mut Contract) {
    simplify_consts(contract);
}

// todo - ian, add documentation
/// ..
pub(crate) fn simplify_consts(contract: &mut Contract) {
    let evaluator = Evaluator::new(&contract.enums);

    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        // we should only collect and iterate through the keys that could potentially be simplified
        // this avoids borrowing the contract immutably and lets us stuff in the simplified expr as we find it

        // todo - ian, see what exprs can be simplified with const folding
        // @mohammad creating a list of exprs in advance so that the 'happy path' of the optimizer doesn't have to do anything and can move on
        let exprs_to_check = contract
            .exprs(pred_key)
            .filter_map(|expr_key| {
                if let Some(Expr::BinaryOp { span, .. }) = expr_key.try_get(contract) {
                    Some((expr_key, span.clone()))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        for (expr_key, span) in exprs_to_check {
            if let Ok(imm) = evaluator.evaluate_key(&expr_key, &Handler::default(), contract) {
                let expr = Expr::Immediate {
                    value: imm.clone(),
                    span: span.clone(),
                };

                let simplified_expr_key = contract.exprs.insert(expr, imm.get_ty(Some(&span)));

                contract.replace_exprs(Some(pred_key), expr_key, simplified_expr_key);
            }
        }
    }
}
