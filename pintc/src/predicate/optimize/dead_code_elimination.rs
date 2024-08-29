use fxhash::FxHashSet;

use crate::{
    error::Handler,
    expr::{evaluate::Evaluator, Expr, Immediate},
    predicate::{ConstraintDecl, Contract, StateKey},
    span::empty_span,
    types::Type,
};

/// In a given contract, remove any code that is not reachable or used.
///
/// If an error occurs, the specific optimization process is aborted to ensure the contract remains functional.
pub(crate) fn dead_code_elimination(contract: &mut Contract) {
    dead_state_elimination(contract);
    dead_constraint_elimination(contract);
}

/// Remove all unused States in their respective predicates.
pub(crate) fn dead_state_elimination(contract: &mut Contract) {
    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        let live_paths = contract
            .exprs(pred_key)
            .filter_map(|expr| {
                if let Expr::Path(name, _) = expr.get(contract) {
                    Some(name.to_string())
                } else {
                    None
                }
            })
            .collect::<FxHashSet<String>>();

        let pred_states = contract
            .preds
            .get(pred_key)
            .expect("pred guaranteed to exist")
            .states();

        let dead_state_decls = pred_states
            .filter(|&(_, state)| (!live_paths.contains(&state.name)))
            .map(|(state_key, _)| state_key)
            .collect::<FxHashSet<StateKey>>();

        if let Some(pred) = contract.preds.get_mut(pred_key) {
            for dead_state in dead_state_decls {
                pred.states.remove(dead_state);
            }
        }
    }
}

/// Remove all trivial Constraints in their respective predicates.
///
/// If any constraint evaluates to false, all constraints are removed and replaced with a single instance of `constraint false`
pub(crate) fn dead_constraint_elimination(contract: &mut Contract) {
    let evaluator = Evaluator::new(&contract.enums);
    let handler = Handler::default();

    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        if let Some(pred) = contract.preds.get(pred_key) {
            let dead_constraints = pred
                .constraints
                .iter()
                .filter_map(|constraint| {
                    let evaluation = evaluator.evaluate_key(&constraint.expr, &handler, contract);
                    match evaluation {
                        Ok(imm) => Some((constraint.clone(), imm)),
                        Err(_) => None,
                    }
                })
                .collect::<Vec<(ConstraintDecl, Immediate)>>();

            if dead_constraints
                .iter()
                .find(|(_, imm)| *imm == Immediate::Bool(false))
                .is_some()
            {
                // replace all constraints with one `constraint false`
                if let Some(pred) = contract.preds.get_mut(pred_key) {
                    let false_expr_key = contract.exprs.insert(
                        Expr::Immediate {
                            value: Immediate::Bool(false),
                            span: empty_span(),
                        },
                        Type::Primitive {
                            kind: crate::types::PrimitiveKind::Bool,
                            span: empty_span(),
                        },
                    );
                    pred.constraints = vec![ConstraintDecl {
                        expr: false_expr_key,
                        span: empty_span(),
                    }]
                }
            } else {
                // retain only useful constraints
                if let Some(pred) = contract.preds.get_mut(pred_key) {
                    pred.constraints.retain(|constraint| {
                        dead_constraints
                            .iter()
                            .find(|(dead_constraint, _)| dead_constraint == constraint)
                            .is_none()
                    })
                }
            }
        }
    }
}
