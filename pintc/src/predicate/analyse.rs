mod array_check;
mod type_check;

use super::{Const, Contract, Expr, ExprKey, Ident};
use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::evaluate::Evaluator,
    span::{empty_span, Span},
    types::Type,
};

#[derive(Debug)]
enum Inference {
    Ignored,
    Type(Type),
    Types {
        ty: Type,
        others: Vec<(ExprKey, Type)>,
    },
    Dependant(ExprKey),
    Dependencies(Vec<ExprKey>),
    BoundDependencies {
        deps: Vec<ExprKey>,
        bound_deps: Vec<(Ident, Type, Vec<ExprKey>)>,
    },
}

impl Contract {
    pub fn type_check(mut self, handler: &Handler) -> Result<Self, ErrorEmitted> {
        // Evaluate all the constant decls to ensure they're all immediates. Each Const expr is
        // updated and has its type set.
        handler.scope(|handler| self.evaluate_all_consts(handler))?;

        for pred_key in self.preds.keys() {
            for expr_key in self.exprs(pred_key) {
                if let Some(span) = self.removed_macro_calls.get(expr_key) {
                    // This expression was actually a macro call which expanded to just declarations,
                    // not another expression. We can set a specific error in this case.
                    handler.emit_err(Error::Compile {
                        error: CompileError::MacroCallWasNotExpression { span: span.clone() },
                    });
                }
            }
        }

        if handler.has_errors() {
            return Err(handler.cancel());
        }

        handler.scope(|handler| self.lower_custom_types(handler))?;

        // TODO: remove the following, merge into lower_custom_types()?
        let _ = handler.scope(|handler| self.check_undefined_types(handler));
        let _ = handler.scope(|handler| self.check_storage_types(handler));
        let _ = handler.scope(|handler| self.type_check_all(handler));
        let _ = handler.scope(|handler| self.check_types_of_variables(handler));
        let _ = handler.scope(|handler| self.check_inits(handler));
        let _ = handler.scope(|handler| self.check_constraint_types(handler));

        if !handler.has_errors() {
            // If we haven't caught any issues so far, it's safe to start looking for cyclical
            // dependencies between predicates. It makes no sense to check this if some predicates
            // have errors. Ideally, this check would live outside the `type_check(..)` function.
            let _ = handler.scope(|handler| self.check_cyclical_predicate_dependencies(handler));
        }

        handler.result(self)
    }

    pub fn array_check(self, handler: &Handler) -> Result<Self, ErrorEmitted> {
        for pred_key in self.preds.keys() {
            self.check_array_lengths(handler, pred_key);
            self.check_array_indexing(handler, pred_key);
            self.check_array_compares(handler, pred_key);
        }

        handler.result(self)
    }

    fn evaluate_all_consts(&mut self, handler: &Handler) -> Result<(), ErrorEmitted> {
        // Evaluate every const initialiser which isn't already an immediate.
        //
        // Some consts may depend on others and so there may be a non-trivial evaluation order to
        // shake out all the final immediate values.  But determining this order is also hard.
        //
        // Worst case is every constant depends on another, except for one.  (If all consts depend
        // on another then there must be some sort of recursive loop which must fail.)  So
        // performing N-1 evaluation passes for N consts should resolve all dependencies and in
        // most cases will be done in only 1 or 2 passes.
        //
        // An added complexity is initialisers which have an erroneous, internally inconsistent
        // type, like `false in 1..2`.  We must abort for any const decls with bad types, but we
        // can't let a type error stall the iterative dependency resolution.

        // Perform a type check for each const intialiser first.  Some may fail due to
        // dependencies, but those which are genuinely busted should be caught here.  Store any
        // errors in its own handler.  If errors are found we'll only report them if we failed to
        // evaluate any initialisers too.
        let ty_chk_handler = Handler::default();
        self.consts
            .iter()
            .map(|cnst| cnst.1.expr)
            .collect::<Vec<ExprKey>>()
            .into_iter()
            .for_each(|cnst_expr_key| {
                let _ = self.type_check_single_expr(&ty_chk_handler, None, cnst_expr_key);
            });

        let mut evaluator = Evaluator::new(self);
        let mut new_immediates = Vec::default();

        // Use a temporary error handler to manage in-progress errors.
        let eval_handler = Handler::default();

        let const_count = self.consts.len();
        for loop_idx in 0..const_count {
            for (path, cnst) in &self.consts {
                let expr = cnst.expr.get(self);

                // There's no need to re-evaluate known immediates.
                if !evaluator.contains_path(&path.name) {
                    if let Expr::Immediate { value, .. } = expr {
                        evaluator.insert_value(path.name.clone(), value.clone());
                    } else if let Ok(imm) = evaluator.evaluate(cnst.expr, &eval_handler, self) {
                        evaluator.insert_value(path.name.clone(), imm);

                        // Take note of this const as we need to update the const declaration
                        // with the new evaluated immediate value.
                        new_immediates.push(path.name.clone());
                    }
                }
            }

            if !eval_handler.has_errors() {
                // We evaluated all the consts without error.  Stop now.
                break;
            }

            if loop_idx != (const_count - 1) {
                // This isn't the last iteration.  Clear the temporary errors.
                eval_handler.clear_errors();
            }
        }

        // Gather errors from both the type checks and the evaluations.  We're a bit conservative
        // here:
        // - Ignore all internal errors from the type checker.
        // - Ignore internal errors from the evaluator, but only if there are other errors.
        // - Only emit the type errors if there are evaluation errors too.

        ty_chk_handler.remove_internal(true);
        eval_handler.remove_internal(false);

        if eval_handler.has_errors() {
            handler.append(ty_chk_handler);
        }
        handler.append(eval_handler);

        // There's little point in continuing if we have errors at this stage.
        if handler.has_errors() {
            return Err(handler.cancel());
        }

        // For each of the newly evaluated intialisers we need to update the expressions in the
        // consts map.
        let all_const_immediates = evaluator.into_values();
        for new_path in new_immediates {
            let init_span: Span = self
                .consts
                .iter()
                .find_map(|(id, cnst)| {
                    (id.name == new_path).then(|| self.expr_key_to_span(cnst.expr))
                })
                .unwrap_or_else(empty_span);

            if let Some(imm_value) = all_const_immediates.get(&new_path) {
                let new_expr = Expr::Immediate {
                    value: imm_value.clone(),
                    span: init_span,
                };

                let new_expr_key = self.exprs.insert(new_expr, imm_value.get_ty(None));

                if let Some((_, cnst)) = self.consts.iter_mut().find(|(id, _)| id.name == new_path)
                {
                    cnst.expr = new_expr_key;
                } else {
                    handler
                        .emit_internal_err("missing const decl for immediate update", empty_span());
                }
            } else {
                handler.emit_internal_err(
                    "missing immediate value for const expr update",
                    empty_span(),
                );
            }
        }

        // And for all const decls with an unknown decl type we need to update that too.
        let mut type_replacements: Vec<(String, Type)> = Vec::default();
        for (path, Const { expr, decl_ty, .. }) in &self.consts {
            if decl_ty.is_unknown() {
                if let Some(imm_value) = all_const_immediates.get(&path.name) {
                    // Check the type is valid.
                    let span = self.expr_key_to_span(*expr);
                    match self.infer_immediate(handler, imm_value, &span) {
                        Inference::Type(new_ty) => {
                            type_replacements.push((path.name.clone(), new_ty))
                        }

                        Inference::Ignored
                        | Inference::Types { .. } // todo: should probably handle this? 
                        | Inference::Dependant(_)
                        | Inference::Dependencies(_)
                        | Inference::BoundDependencies { .. } => {
                            handler
                                .emit_internal_err("const inferred a dependant type", empty_span());
                        }
                    }
                } else {
                    handler.emit_internal_err(
                        "missing immediate value for const decl_ty update",
                        empty_span(),
                    );
                }
            }
        }

        for (path, new_ty) in type_replacements {
            self.consts
                .iter_mut()
                .find_map(|(id, cnst)| (id.name == path).then_some(cnst))
                .unwrap()
                .decl_ty = new_ty;
        }

        handler.result(())
    }
}
