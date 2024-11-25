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
        if !fold_consts(contract) && !fold_identities(contract) && !fold_const_lets(contract) {
            break;
        }

        // If the loop has gone for too long then there's an internal error. Arbitrary limit...
        if loop_count > 10_000 {
            handler.emit_internal_err("Infinite loop in const_folding".to_string(), empty_span());
            break;
        }
    }
}

// todo - ian - when solved, roll it into fold_consts
// ex:
// storage {
//     y: int,
// }
//
// predicate ::Foo(
// ) {
//     let ::x: int = 5;
//     let ::y: int = (__storage_get({0}) + ::x);
//     constraint __eq_set(__mut_keys(), {0});
// }
//
// becomes
//
// storage {
//     y: int,
// }
//
// predicate ::Foo(
// ) {
//     let ::y: int = (__storage_get({0}) + 5);
//     constraint __eq_set(__mut_keys(), {0});
// }
// so we know that ::x is a const somewhere. Likely means we need to evaluate on ::x but the evaluator requires an immediate which ::x is not
// so what is the middle step that allows us to evaluate ::x to see if it resolves to an immediate?

// create a map of paths to immediates (that we know) <String, Immediate>
// then we do const folding
// change the evaluator to receive the maps (can be empty). The map will be used when we see a path to replace it with an immediate
// what about the case of let x = 5;, let z = x + 5;? Running the evaluator will only get one. But we're already in an optimization loop so we just keep going and on pass 2 we will resolve let z = x + 5 as let z = 10 as 10;

// new solution structure has hardcoded addresses. When I change the optimization, the bytecode for some tests may change and I'll get an error. Need to go build the pint code contract again and update each address
// mindless but easy
// error 'solution for test does not solve an predicate'

// TODO - ian - open issue to return address with pintc instead of just pint cli. And assign to self next.
pub(crate) fn fold_const_lets(contract: &mut Contract) -> bool {
    // println!("contract: {:#?}", contract);
    let mut scope_values: FxHashMap<String, Immediate> = FxHashMap::default();
    let mut expr_keys_to_replace: FxHashMap<ExprKey, (Expr, Type)> = FxHashMap::default();
    let mut did_fold_const: bool = false;

    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        for var in contract.preds.get(pred_key).unwrap().variables() {
            // check if expr is immediate
            // if so, store in scope_values both name (path) and value (imm)
            let expr = var.1.expr.get(&contract);
            if let Expr::Immediate { value, .. } = expr {
                scope_values.insert(var.1.name.clone(), value.clone());
            }
        }

        // loop over exprs and feed in scope values to the evaluator
        // then *crossed fingers*, the evaluator will return 5 for x?

        let evaluator = Evaluator::from_values(contract, scope_values.clone());

        for expr_key in contract.exprs(pred_key) {
            if let Expr::Path(..) = expr_key.get(&contract) {
                // TODO - ian - see if we need to filter by path or not.

                if let Ok(imm) = evaluator.evaluate_key(&expr_key, &Handler::default(), contract) {
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
        expr_keys_to_replace = FxHashMap::default();
    }

    did_fold_const
}

/// In a given contract, replace any expression that can be evaluated to a const, with it's const value.
/// Returns whether or not any consts were folded.
///
/// Ex:
/// 1 + 2 is 3
/// 1 + 2 < 4 is true
pub(crate) fn fold_consts(contract: &mut Contract) -> bool {
    let mut expr_keys_to_replace: FxHashMap<ExprKey, (Expr, Type)> = FxHashMap::default();
    let mut did_fold_const: bool = false;

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
        expr_keys_to_replace = FxHashMap::default();
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
