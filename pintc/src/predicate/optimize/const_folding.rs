use fxhash::FxHashMap;

use crate::{
    error::Handler,
    expr::{evaluate::Evaluator, BinaryOp, Expr, Immediate},
    predicate::{Contract, ExprKey},
    span::Spanned,
    types::Type,
};

/// In a given contract, simplify all sub-expressions that evaluate to constants.
pub(crate) fn const_folding(contract: &mut Contract) {
    fold_consts(contract);
    fold_boolean_identities(contract);
}

pub(crate) fn fold_consts(contract: &mut Contract) {
    let mut replace_map: FxHashMap<ExprKey, (Expr, Type)> = FxHashMap::default();

    let evaluator = Evaluator::new(contract);

    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        for expr_key in contract.exprs(pred_key) {
            if expr_key.get(contract).is_immediate() {
                continue;
            }

            if let Ok(imm) = evaluator.evaluate_key(&expr_key, &Handler::default(), contract) {
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

            contract.replace_exprs(Some(pred_key), *old_expr_key, simplified_expr_key);
        }
    }
}

/// Simplify any constraint conditions that consist of constant binary operations. Ex:
/// true || <expr> is true
/// true && <expr> is <expr>
/// false || <expr> is <expr>
/// false && <expr> is <false>
pub(crate) fn fold_boolean_identities(contract: &mut Contract) {
    let evaluator = Evaluator::new(contract);

    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        if let Some(pred) = contract.preds.get(pred_key) {
            let constraints_to_evaluate = pred
                .constraints
                .iter()
                .enumerate()
                .filter_map(|(i, constraint)| {
                    let expr = constraint.expr.get(contract);

                    if let Expr::BinaryOp { op, lhs, rhs, .. } = expr {
                        let lhs_imm = evaluator.evaluate_key(lhs, &Handler::default(), contract);
                        let rhs_imm = evaluator.evaluate_key(rhs, &Handler::default(), contract);

                        match op {
                            BinaryOp::LogicalAnd => match (lhs_imm, rhs_imm) {
                                (Ok(Immediate::Bool(true)), Err(_)) => Some((i, *rhs)),

                                (Err(_), Ok(Immediate::Bool(true))) => Some((i, *lhs)),

                                (Ok(Immediate::Bool(false)), Err(_)) => Some((i, *lhs)),

                                (Err(_), Ok(Immediate::Bool(false))) => Some((i, *rhs)),

                                _ => None,
                            },

                            BinaryOp::LogicalOr => match (lhs_imm, rhs_imm) {
                                (Ok(Immediate::Bool(true)), Err(_)) => Some((i, *lhs)),

                                (Err(_), Ok(Immediate::Bool(true))) => Some((i, *rhs)),

                                (Ok(Immediate::Bool(false)), Err(_)) => Some((i, *rhs)),

                                (Err(_), Ok(Immediate::Bool(false))) => Some((i, *lhs)),

                                _ => None,
                            },

                            _ => None,
                        }
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();

            if let Some(pred) = contract.preds.get_mut(pred_key) {
                constraints_to_evaluate.iter().for_each(|(i, new_expr)| {
                    if let Some(constraint) = pred.constraints.get_mut(*i) {
                        constraint.expr = *new_expr;
                    }
                });
            }
        }
    }
}
