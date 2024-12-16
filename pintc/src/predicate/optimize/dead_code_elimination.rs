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
    dead_variable_elimination(contract);
    dead_constraint_elimination(handler, contract);
    dead_select_elimination(contract);
    // @mohammad - I originally thought removing dupes should be rolled into their respective dead code elim functions.
    // I no longer think that. I think there is enough unique logic with the duplicate removals that it's better to keep it separate
    duplicate_variable_elimination(contract);
    duplicate_constraint_elimination(contract);
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

// todo - ian - documentation
// todo - ian - make names for vars more consistent. Ex. refer to keys as keys everywhere, vars, exprs, no mixing
pub(crate) fn duplicate_variable_elimination(contract: &mut Contract) {
    for pred_key in contract.preds.keys().collect::<Vec<_>>() {
        let pred_variables = contract
            .preds
            .get(pred_key)
            .expect("pred guaranteed to exist")
            .variables()
            .collect::<Vec<_>>();

        // for loop for now, use filter in the future if possible
        // need to be able to visit every variable with another to tell if it is a duplicate
        // could also try a hashset... nevermind it wouldn't be able to tell if the expr is the same or not
        let mut dupe_variable_decls: Vec<(VariableKey, VariableKey)> = vec![]; // (original_key, dupe_key)
        if pred_variables.len() > 0 {
            for index in 0..pred_variables.len() - 1 {
                // todo - ian - understand when to stop the search, at some point there won't be any originals left and the checks will be redundant
                // could keep track of indexes, but may be more overhead than it's worth
                // could just skip it the index if it's already in the dupe_variable_decls list
                let original_var = if dupe_variable_decls
                    .iter()
                    .any(|(original_var_key, _)| original_var_key == &pred_variables[index].0)
                {
                    continue;
                } else {
                    &pred_variables[index]
                };
                let remaining_vars = &pred_variables[index + 1..pred_variables.len()];

                // check for duplicate var exprs
                for (key, var) in remaining_vars.into_iter() {
                    // @mohammad is this an appropriate way to check if they're equal?
                    // The alternative would be to traverse through all the expressions and nested expressions, gather the end of each branch, then compare that the values are the same
                    // The complication is that all the expr keys are different, we can only reliably tell that the exprs are the same by checking the final values at the end of each branch
                    if contract.with_ctrct(var.expr).to_string()
                        == contract.with_ctrct(original_var.1.expr).to_string()
                    {
                        dupe_variable_decls.push((original_var.0, *key));
                    }
                }
            }
        }

        // collect all of the duplicate var exprs and duplicate paths to be cleared out
        let mut dupe_var_exprs: Vec<(ExprKey, ExprKey)> = vec![];
        let mut dupe_paths: Vec<(ExprKey, ExprKey)> = vec![];
        if let Some(pred) = contract.preds.get(pred_key) {
            for (original_var_key, dupe_var_key) in &dupe_variable_decls {
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

        // replace all uses of the duplicate var exprs and paths with the originals
        for (original_expr_key, dupe_expr_key) in dupe_var_exprs {
            contract.replace_exprs(Some(pred_key), dupe_expr_key, original_expr_key);
        }

        // replace all uses of the duplicate var exprs and paths with the originals
        for (original_expr_key, dupe_expr_key) in dupe_paths {
            contract.replace_exprs(Some(pred_key), dupe_expr_key, original_expr_key);
        }

        // remove duplicate variables
        if let Some(pred) = contract.preds.get_mut(pred_key) {
            for (_, dupe_var_key) in dupe_variable_decls {
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

            for (index, original_constraint) in pred.constraints.iter().enumerate() {
                if duplicate_constraints.contains(&index) {
                    continue;
                }

                let subsequent_constraints = &pred.constraints[index + 1..];
                for (i, constraint) in subsequent_constraints.into_iter().enumerate() {
                    // @mohammad, once again just doing a string comparison here
                    // I don't think it's that unsafe. Though I don't know for sure
                    // in my mind, we're using the same display trait for both expr keys, so no matter the changes, it will
                    // output the same result
                    // this method fails to see that the x + 1 and 1 + x are the same expression. i.e. order matters
                    // do we start with this then expand later?
                    if contract.with_ctrct(constraint.expr).to_string()
                        == contract.with_ctrct(original_constraint.expr).to_string()
                    {
                        duplicate_constraints.push(index + i + 1);
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
