use fxhash::{FxHashMap, FxHashSet};
use itertools::Itertools;

use crate::{
    error::Handler,
    expr::{evaluate::Evaluator, Expr, Immediate},
    predicate::{ConstraintDecl, Contract, ExprKey, VariableKey},
    span::empty_span,
    warning::Warning,
};

/// In a given contract, remove any code that is not reachable or used.
///
/// If an error occurs, the specific optimization process is aborted to ensure the contract remains
/// functional.
pub(crate) fn dead_code_elimination(handler: &Handler, contract: &mut Contract) {
    // todo - ian - if we don't run dead variable elim, then we panic on accessing a path. Need to make these passes completely independent
    dead_variable_elimination(contract);
    dead_constraint_elimination(handler, contract);
    dead_select_elimination(contract);

    duplicate_variable_elimination(contract);
    // duplicate_constraint_elimination(contract);
}

/// Remove all unused variables in their respective predicates.
pub(crate) fn dead_variable_elimination(contract: &mut Contract) {
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

        let pred_variables = contract
            .preds
            .get(pred_key)
            .expect("pred guaranteed to exist")
            .variables();

        let dead_variable_decls = pred_variables
            .filter(|&(_, variable)| (!live_paths.contains(&variable.name)))
            .map(|(variable_key, _)| variable_key)
            .collect::<FxHashSet<VariableKey>>();

        if let Some(pred) = contract.preds.get_mut(pred_key) {
            for dead_variable in dead_variable_decls {
                pred.variables.remove(dead_variable);
            }
        }
    }
}

/// Remove all trivial Constraints in their respective predicates.
///
/// If any constraint evaluates to false, all constraints are removed and replaced with a single
/// instance of `constraint false`
pub(crate) fn dead_constraint_elimination(handler: &Handler, contract: &mut Contract) {
    let evaluator = Evaluator::new(contract);

    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        if let Some(pred) = contract.preds.get(pred_key) {
            let mut has_false_constraint = false;

            let dead_constraints = pred
                .constraints
                .iter()
                .enumerate()
                .filter_map(|(i, constraint)| {
                    // If the evaluator succeeds, we're only expecting true or false. If it doesn't
                    // then we don't care about the constraint.
                    //
                    // We also don't care about the errors emitted by the evaluator
                    if let Ok(Immediate::Bool(b)) =
                        evaluator.evaluate_key(&constraint.expr, &Handler::default(), contract)
                    {
                        if !b {
                            has_false_constraint = true;
                            handler.emit_warn(Warning::AlwaysFalseConstraint {
                                span: constraint.span.clone(),
                            });
                        }

                        Some(i)
                    } else {
                        None
                    }
                })
                .collect::<Vec<usize>>();

            if has_false_constraint {
                // replace all constraints with one `constraint false`
                if let Some(pred) = contract.preds.get_mut(pred_key) {
                    pred.constraints = vec![ConstraintDecl {
                        expr: contract.exprs.insert_bool(false),
                        // ideally we would collect the spans of all the constraints, but we don't
                        // have the ability to do that right now
                        span: empty_span(),
                    }]
                }
            } else {
                // retain only useful constraints
                if let Some(pred) = contract.preds.get_mut(pred_key) {
                    // Remove dead constraints in reverse to avoid removing the wrong indices from
                    // shifting elements. This assumes dead_constraints is sorted, which it is based
                    // on how it is collected above
                    dead_constraints.iter().rev().for_each(|i| {
                        pred.constraints.remove(*i);
                    });
                }
            }
        }
    }
}

/// Remove all trivial Select expressions in their respective predicates.
///
/// If any select condition is a const the appropriate branch replaces the select expression
pub(crate) fn dead_select_elimination(contract: &mut Contract) {
    let evaluator = Evaluator::new(contract);
    let mut replace_map: FxHashMap<ExprKey /* select */, ExprKey /* branch */> =
        FxHashMap::default();

    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        for expr_key in contract.exprs(pred_key) {
            if let Expr::Select {
                condition,
                then_expr,
                else_expr,
                ..
            } = expr_key.get(contract)
            {
                if let Ok(Immediate::Bool(b)) =
                    evaluator.evaluate_key(condition, &Handler::default(), contract)
                {
                    if b {
                        replace_map.insert(expr_key, *then_expr);
                    } else {
                        replace_map.insert(expr_key, *else_expr);
                    }
                }
            }
        }

        for (select_expr, branch_expr) in &replace_map {
            contract.replace_exprs(Some(pred_key), *select_expr, *branch_expr);
        }
    }
}

/// Replace all uses of, and remove all duplicate Variable declarations
///
/// Any variable declaration that contains an equivalent expression is removed, regardless of span
///
/// Ex.
/// let a = x + 1;
/// let b = x + 1;
/// let d = b + 1;
/// becomes
/// let a = x + 1;
/// let d = a + 1;
pub(crate) fn duplicate_variable_elimination(contract: &mut Contract) {
    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        let pred_variables = contract
            .preds
            .get(pred_key)
            .expect("pred guaranteed to exist")
            .variables()
            .collect::<Vec<_>>();

        let mut dupe_var_decls: Vec<(VariableKey, VariableKey)> = vec![]; // (original_key, dupe_key)
        for (i, (var_key, var)) in pred_variables.iter().enumerate() {
            // avoid double checking any variable that has already been marked as a duplicate
            if dupe_var_decls
                .iter()
                .any(|(original_var_key, dupe_var_key)| {
                    original_var_key == var_key || dupe_var_key == var_key
                })
            {
                continue;
            };

            for (subsequent_var_key, subsequent_var) in pred_variables.iter().skip(i + 1) {
                if let Some(pred) = contract.preds.get(pred_key) {
                    if !var_key
                        .get_ty(pred)
                        .eq(contract, subsequent_var_key.get_ty(pred))
                    {
                        // println!("var key is not the same type as subsequent var key");
                        continue;
                    }
                }

                if var
                    .expr
                    .get(contract)
                    .eq(contract, &subsequent_var.expr.get(&contract))
                {
                    println!(
                        "{} is a dupe of {}",
                        contract.with_ctrct(subsequent_var.expr),
                        contract.with_ctrct(var.expr),
                    );
                    dupe_var_decls.push((*var_key, *subsequent_var_key));
                }
            }
        }

        // collect all of the duplicate var exprs and duplicate paths to be cleared out
        let mut dupe_var_exprs: Vec<(ExprKey, ExprKey)> = vec![];
        let mut dupe_paths: Vec<(ExprKey, ExprKey)> = vec![];
        if let Some(pred) = contract.preds.get(pred_key) {
            for (original_var_key, dupe_var_key) in &dupe_var_decls {
                let original_expr_key = original_var_key.get(pred).expr;
                let dupe_expr_key = dupe_var_key.get(pred).expr;
                dupe_var_exprs.push((original_expr_key, dupe_expr_key));

                let original_path_expr = contract
                    .exprs(pred_key)
                    .find(|expr| {
                        if let Expr::Path(name, _) = expr.get(contract) {
                            *name == original_var_key.get(pred).name
                        } else {
                            false
                        }
                    })
                    .expect("original path is guaranteed to exist");

                contract.exprs(pred_key).for_each(|expr| {
                    if let Expr::Path(name, _) = expr.get(contract) {
                        if *name == dupe_var_key.get(pred).name {
                            dupe_paths.push((original_path_expr, expr))
                        }
                    }
                });
            }
        }

        // replace all uses of the duplicate var exprs with the originals
        for (original_expr_key, dupe_expr_key) in dupe_var_exprs {
            contract.replace_exprs(Some(pred_key), dupe_expr_key, original_expr_key);
        }

        // replace all uses of the duplicate var paths with the originals
        for (original_expr_key, dupe_expr_key) in dupe_paths {
            contract.replace_exprs(Some(pred_key), dupe_expr_key, original_expr_key);
        }

        // remove all duplicate variables
        if let Some(pred) = contract.preds.get_mut(pred_key) {
            for (_, dupe_var_key) in dupe_var_decls {
                pred.variables.remove(dupe_var_key);
            }
        }
    }
}

/// Remove all duplicate Constraints in their respective predicates.
pub(crate) fn duplicate_constraint_elimination(contract: &mut Contract) {
    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        if let Some(pred) = contract.preds.get(pred_key) {
            let mut duplicate_constraints: Vec<usize> = vec![];

            for (i, constraint) in pred.constraints.iter().enumerate() {
                if duplicate_constraints.contains(&i) {
                    continue;
                }

                for (j, subsequent_constraint) in pred.constraints.iter().skip(i + 1).enumerate() {
                    if constraint
                        .expr
                        .get(contract)
                        .eq(contract, &subsequent_constraint.expr.get(&contract))
                    {
                        duplicate_constraints.push(i + j + 1);
                    }
                }
            }

            if let Some(pred) = contract.preds.get_mut(pred_key) {
                // Remove duplicate constraints in reverse to avoid removing the wrong indices from
                // shifting elements.
                duplicate_constraints.iter().sorted().rev().for_each(|i| {
                    pred.constraints.remove(*i);
                });
            }
        }
    }
}
