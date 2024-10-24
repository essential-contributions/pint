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
    fold_identities(contract);
    fold_consts(contract);
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

/// In a given contract, simplify any binary ops that consist of boolean or arithmetic (real and int) identities.
///
/// Ex:
/// true || <expr> is true
/// true && <expr> is <expr>
/// false || <expr> is <expr>
/// false && <expr> is <false>
/// x + 0 is x
/// x - 0 is x
/// x * 0 is 0
/// x * 1 is x
/// 0 / x is 0
/// x / 1 is x
/// x % 1 is x
///
/// Performs a depth-first search to find all folding opportunities.
pub(crate) fn fold_identities(contract: &mut Contract) {
    fn fold_identity(
        contract: &Contract,
        evaluator: &Evaluator,
        replace_map: &mut FxHashMap<ExprKey, ExprKey>,
        expr_key: ExprKey,
    ) -> Option<ExprKey> {
        if let Expr::BinaryOp { op, lhs, rhs, .. } = expr_key.get(contract) {
            // The lhs and rhs must be updated and evaluated with modifying the contract's exprs b/c we are in a loop
            // The replacement map is updated with the keys that need to be replaced at the end of the pass
            // while the appropriate lhs and rhs are bubbled up to the parent for evaluation
            let mut lhs = *lhs;
            let mut rhs = *rhs;

            if let Expr::BinaryOp { .. } = lhs.get(contract) {
                if let Some(folded_key) = fold_identity(contract, evaluator, replace_map, lhs) {
                    lhs = folded_key;
                }
            }

            if let Expr::BinaryOp { .. } = rhs.get(contract) {
                if let Some(folded_key) = fold_identity(contract, evaluator, replace_map, rhs) {
                    rhs = folded_key;
                }
            }

            let lhs_imm = evaluator.evaluate_key(&lhs, &Handler::default(), contract);
            let rhs_imm = evaluator.evaluate_key(&rhs, &Handler::default(), contract);

            match op {
                BinaryOp::LogicalAnd => match (lhs_imm, rhs_imm) {
                    (Ok(Immediate::Bool(true)), Err(_)) => {
                        replace_map.insert(expr_key, rhs);
                        Some(rhs)
                    }

                    (Err(_), Ok(Immediate::Bool(true))) => {
                        replace_map.insert(expr_key, lhs);
                        Some(lhs)
                    }

                    (Ok(Immediate::Bool(false)), Err(_)) => {
                        replace_map.insert(expr_key, lhs);
                        Some(lhs)
                    }

                    (Err(_), Ok(Immediate::Bool(false))) => {
                        replace_map.insert(expr_key, rhs);
                        Some(rhs)
                    }

                    _ => None,
                },

                BinaryOp::LogicalOr => match (lhs_imm, rhs_imm) {
                    (Ok(Immediate::Bool(true)), Err(_)) => {
                        replace_map.insert(expr_key, lhs);
                        Some(lhs)
                    }

                    (Err(_), Ok(Immediate::Bool(true))) => {
                        replace_map.insert(expr_key, rhs);
                        Some(rhs)
                    }

                    (Ok(Immediate::Bool(false)), Err(_)) => {
                        replace_map.insert(expr_key, rhs);
                        Some(rhs)
                    }

                    (Err(_), Ok(Immediate::Bool(false))) => {
                        replace_map.insert(expr_key, lhs);
                        Some(lhs)
                    }

                    _ => None,
                },

                BinaryOp::Add => match (lhs_imm, rhs_imm) {
                    (Ok(Immediate::Int(0)), Err(_)) => {
                        replace_map.insert(expr_key, rhs);
                        Some(rhs)
                    }

                    (Err(_), Ok(Immediate::Int(0))) => {
                        replace_map.insert(expr_key, lhs);
                        Some(lhs)
                    }

                    (Ok(Immediate::Real(0.0)), Err(_)) => {
                        replace_map.insert(expr_key, rhs);
                        Some(rhs)
                    }

                    (Err(_), Ok(Immediate::Real(0.0))) => {
                        replace_map.insert(expr_key, lhs);
                        Some(lhs)
                    }

                    _ => None,
                },

                BinaryOp::Sub => match (lhs_imm, rhs_imm) {
                    (Err(_), Ok(Immediate::Int(0))) => {
                        replace_map.insert(expr_key, lhs);
                        Some(lhs)
                    }

                    (Err(_), Ok(Immediate::Real(0.0))) => {
                        replace_map.insert(expr_key, lhs);
                        Some(lhs)
                    }

                    _ => None,
                },

                BinaryOp::Mul => match (lhs_imm, rhs_imm) {
                    (Ok(Immediate::Int(0)), Err(_)) => {
                        replace_map.insert(expr_key, lhs);
                        Some(lhs)
                    }

                    (Err(_), Ok(Immediate::Int(0))) => {
                        replace_map.insert(expr_key, rhs);
                        Some(rhs)
                    }

                    (Ok(Immediate::Real(0.0)), Err(_)) => {
                        replace_map.insert(expr_key, lhs);
                        Some(lhs)
                    }

                    (Err(_), Ok(Immediate::Real(0.0))) => {
                        replace_map.insert(expr_key, rhs);
                        Some(rhs)
                    }

                    _ => None,
                },

                BinaryOp::Div => match (lhs_imm, rhs_imm) {
                    (Ok(Immediate::Int(0)), Err(_)) => {
                        replace_map.insert(expr_key, lhs);
                        Some(lhs)
                    }

                    (Err(_), Ok(Immediate::Int(1))) => {
                        replace_map.insert(expr_key, lhs);
                        Some(lhs)
                    }

                    (Ok(Immediate::Real(0.0)), Err(_)) => {
                        replace_map.insert(expr_key, lhs);
                        Some(lhs)
                    }

                    (Err(_), Ok(Immediate::Real(1.0))) => {
                        replace_map.insert(expr_key, lhs);
                        Some(lhs)
                    }

                    _ => None,
                },

                BinaryOp::Mod => match (lhs_imm, rhs_imm) {
                    (Err(_), Ok(Immediate::Int(1))) => {
                        replace_map.insert(expr_key, lhs);
                        Some(lhs)
                    }

                    (Err(_), Ok(Immediate::Real(1.0))) => {
                        replace_map.insert(expr_key, lhs);
                        Some(lhs)
                    }

                    _ => None,
                },

                _ => None,
            }
        } else {
            None
        }
    }

    let mut replace_map: FxHashMap<ExprKey, ExprKey> = FxHashMap::default();
    let evaluator = Evaluator::new(contract);

    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        for expr_key in contract.exprs(pred_key) {
            fold_identity(contract, &evaluator, &mut replace_map, expr_key);
        }

        for (old_expr_key, new_expr_key) in &replace_map {
            contract.replace_exprs(Some(pred_key), *old_expr_key, *new_expr_key);
        }
    }
}
