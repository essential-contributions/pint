use fxhash::FxHashMap;

use crate::{
    error::Handler,
    expr::{evaluate::Evaluator, BinaryOp, Expr, Immediate, UnaryOp},
    predicate::{Contract, ExprKey},
    span::{empty_span, Spanned},
    types::Type,
};

/// In a given contract, simplify all sub-expressions that evaluate to constants.
///
/// Continues re-running all passes until the contract has no more candidates to fold.
pub(crate) fn const_folding(handler: &Handler, contract: &mut Contract) {
    // This is an unbound loop which breaks if when there are no more consts to fold.
    // It will also break after a gazillion iterations in case of an infinite loop bug.
    for loop_count in 0.. {
        if !fold_consts(contract) && !fold_identities(contract) {
            break;
        }

        // If the loop has gone for too long then there's an internal error. Arbitrary limit...
        if loop_count > 10_000 {
            handler.emit_internal_err("Infinite loop in const_folding", empty_span());
            break;
        }
    }
}

/// In a given contract, replace any expression that can be evaluated to a const, with it's const value.
/// Returns whether or not any consts were folded.
///
/// Ex:
/// 1 + 2 is 3
/// 1 + 2 < 4 is true
/// 1 + ::x is 6 where ::x is 5
pub(crate) fn fold_consts(contract: &mut Contract) -> bool {
    let mut did_fold_const: bool = false;

    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        let mut expr_keys_to_replace: FxHashMap<ExprKey, (Expr, Type)> = FxHashMap::default();
        let mut scope_values: FxHashMap<String, Immediate> = FxHashMap::default();

        // Create a map of variables that were declared as immediates for folding later
        if let Some(pred) = contract.preds.get(pred_key) {
            for (_, var) in pred.variables() {
                if let Expr::Immediate { value, .. } = var.expr.get(contract) {
                    scope_values.insert(var.name.clone(), value.clone());
                }
            }
        }

        let evaluator = Evaluator::from_values(contract, scope_values.clone());

        for expr_key in contract.exprs(pred_key) {
            if expr_key.get(contract).is_immediate() {
                continue;
            }

            if let Ok(imm) = evaluator.evaluate_key(&expr_key, &Handler::default(), contract, false)
            {
                let simplified_expr = Expr::Immediate {
                    value: imm.clone(),
                    span: expr_key.get(contract).span().clone(),
                };

                expr_keys_to_replace.insert(
                    expr_key,
                    (simplified_expr, expr_key.get_ty(contract).clone()),
                );
            }
        }

        for (old_expr_key, (simplified_expr, simplified_type)) in &expr_keys_to_replace {
            let simplified_expr_key = contract
                .exprs
                .insert(simplified_expr.clone(), simplified_type.clone());

            contract.replace_exprs(Some(pred_key), *old_expr_key, simplified_expr_key);
        }

        if !expr_keys_to_replace.is_empty() {
            did_fold_const = true;
        }
    }

    did_fold_const
}

/// In a given contract, simplify any binary ops that consist of boolean or arithmetic (real and int) identities.
/// Returns whether or not any identities were folded.
///
/// Ex:
/// true || <expr> is true
/// true && <expr> is <expr>
/// false || <expr> is <expr>
/// false && <expr> is <false>
/// x + 0 is x
/// x - 0 is x
/// 0 - x is -x
/// x * 0 is 0
/// x * 1 is x
/// 0 / x is 0
/// x / 1 is x
/// x % 1 is x
/// 1 % x is x
pub(crate) fn fold_identities(contract: &mut Contract) -> bool {
    let mut expr_keys_to_replace: Vec<(ExprKey, ExprKey)> = vec![];
    let mut new_exprs_to_replace: Vec<(ExprKey, Expr, Type)> = vec![];
    let mut did_fold_const: bool = false;

    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        for expr_key in contract.exprs(pred_key) {
            if let Expr::BinaryOp { op, lhs, rhs, span } = expr_key.get(contract) {
                let lhs_imm = if let Expr::Immediate { value, .. } = lhs.get(contract) {
                    Some(value)
                } else {
                    None
                };

                let rhs_imm = if let Expr::Immediate { value, .. } = rhs.get(contract) {
                    Some(value)
                } else {
                    None
                };

                // First, look for any folding opportunities with just expr_key swaps
                let replacement_expr_key = match (op, lhs_imm, rhs_imm) {
                    (BinaryOp::LogicalAnd, Some(Immediate::Bool(true)), _) => Some(*rhs),

                    (BinaryOp::LogicalAnd, _, Some(Immediate::Bool(true))) => Some(*lhs),

                    (BinaryOp::LogicalAnd, Some(Immediate::Bool(false)), _) => Some(*lhs),

                    (BinaryOp::LogicalAnd, _, Some(Immediate::Bool(false))) => Some(*rhs),

                    (BinaryOp::LogicalOr, Some(Immediate::Bool(true)), _) => Some(*lhs),

                    (BinaryOp::LogicalOr, _, Some(Immediate::Bool(true))) => Some(*rhs),

                    (BinaryOp::LogicalOr, Some(Immediate::Bool(false)), _) => Some(*rhs),

                    (BinaryOp::LogicalOr, _, Some(Immediate::Bool(false))) => Some(*lhs),

                    (BinaryOp::Add, Some(Immediate::Int(0)), _) => Some(*rhs),

                    (BinaryOp::Add, _, Some(Immediate::Int(0))) => Some(*lhs),

                    (BinaryOp::Add, Some(Immediate::Real(0.0)), _) => Some(*rhs),

                    (BinaryOp::Add, _, Some(Immediate::Real(0.0))) => Some(*lhs),

                    (BinaryOp::Sub, _, Some(Immediate::Int(0))) => Some(*lhs),

                    (BinaryOp::Sub, _, Some(Immediate::Real(0.0))) => Some(*lhs),

                    (BinaryOp::Mul, Some(Immediate::Int(0)), _) => Some(*lhs),

                    (BinaryOp::Mul, _, Some(Immediate::Int(0))) => Some(*rhs),

                    (BinaryOp::Mul, Some(Immediate::Real(0.0)), _) => Some(*lhs),

                    (BinaryOp::Mul, _, Some(Immediate::Real(0.0))) => Some(*rhs),

                    (BinaryOp::Div, Some(Immediate::Int(0)), _) => Some(*lhs),

                    (BinaryOp::Div, _, Some(Immediate::Int(1))) => Some(*lhs),

                    (BinaryOp::Div, Some(Immediate::Real(0.0)), _) => Some(*lhs),

                    (BinaryOp::Div, _, Some(Immediate::Real(1.0))) => Some(*lhs),

                    (BinaryOp::Mod, _, Some(Immediate::Int(1))) => Some(*lhs),

                    (BinaryOp::Mod, Some(Immediate::Int(1)), _) => Some(*lhs),

                    _ => None,
                };

                if let Some(replacement_expr_key) = replacement_expr_key {
                    expr_keys_to_replace.push((expr_key, replacement_expr_key));
                }

                // Next, look for folding opportunities where new or modified exprs need to be inserted
                let replacement_expr = match (op, lhs_imm, rhs_imm) {
                    (BinaryOp::Sub, Some(Immediate::Int(0)), _) => {
                        let new_expr = Expr::UnaryOp {
                            op: UnaryOp::Neg,
                            expr: *rhs,
                            span: span.clone(),
                        };
                        Some((new_expr, lhs.get_ty(contract).clone()))
                    }

                    (BinaryOp::Sub, Some(Immediate::Real(0.0)), _) => {
                        let new_expr = Expr::UnaryOp {
                            op: UnaryOp::Neg,
                            expr: *rhs,
                            span: span.clone(),
                        };
                        Some((new_expr, lhs.get_ty(contract).clone()))
                    }

                    _ => None,
                };

                if let Some(replacement_expr) = replacement_expr {
                    new_exprs_to_replace.push((expr_key, replacement_expr.0, replacement_expr.1))
                }
            }
        }

        // Insert new/modified exprs now that the contract is mutable outside of the loop and before performing the expr_key replacement
        for (old_expr_key, new_expr, new_expr_ty) in &new_exprs_to_replace {
            let new_expr_key = contract.exprs.insert(new_expr.clone(), new_expr_ty.clone());
            expr_keys_to_replace.push((*old_expr_key, new_expr_key));
        }

        for (old_expr_key, new_expr_key) in &expr_keys_to_replace {
            contract.replace_exprs(Some(pred_key), *old_expr_key, *new_expr_key);
        }

        if !expr_keys_to_replace.is_empty() {
            did_fold_const = true;
        }
        expr_keys_to_replace = vec![];
    }

    did_fold_const
}
