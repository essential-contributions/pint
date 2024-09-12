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
    Dependant(ExprKey),
    Dependencies(Vec<ExprKey>),
    BoundDependencies {
        deps: Vec<ExprKey>,
        bound_deps: Vec<(Ident, Type, Vec<ExprKey>)>,
    },
}

impl Contract {
    pub fn type_check(mut self, handler: &Handler) -> Result<Self, ErrorEmitted> {
        // Ensure that all storage accesses are used legally, i.e., in state initializers only.
        let _ = handler.scope(|handler| self.check_storage_accesses(handler));

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

        let _ = handler.scope(|handler| self.check_undefined_types(handler));
        let _ = handler.scope(|handler| self.lower_custom_types(handler));
        let _ = handler.scope(|handler| self.check_storage_types(handler));
        let _ = handler.scope(|handler| self.type_check_all(handler));
        let _ = handler.scope(|handler| self.check_types_of_variables(handler));
        let _ = handler.scope(|handler| self.check_inits(handler));
        let _ = handler.scope(|handler| self.check_constraint_types(handler));

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

        let mut evaluator = Evaluator::new(&self.enums);
        let mut new_immediates = Vec::default();

        // Use a temporary error handler to manage in-progress errors.
        let tmp_handler = Handler::default();

        let const_count = self.consts.len();
        for loop_idx in 0..const_count {
            for (path, cnst) in &self.consts {
                let expr = cnst.expr.get(self);

                // There's no need to re-evaluate known immediates.
                if !evaluator.contains_path(path) {
                    if let Expr::Immediate { value, .. } = expr {
                        evaluator.insert_value(path.clone(), value.clone());
                    } else if let Ok(imm) = evaluator.evaluate_key(&cnst.expr, &tmp_handler, self) {
                        evaluator.insert_value(path.clone(), imm);

                        // Take note of this const as we need to update the const declaration
                        // with the new evaluated immediate value.
                        new_immediates.push(path.clone());
                    }
                }
            }

            if !tmp_handler.has_errors() {
                // We evaluated all the consts without error.  Stop now.
                break;
            }

            if loop_idx != (const_count - 1) {
                // This isn't the last iteration.  Clear the temporary errors.
                tmp_handler.clear_errors();
            }
        }

        // There's little point in continuing if we weren't able to lower all consts.
        handler.append(tmp_handler);
        if handler.has_errors() {
            return Err(handler.cancel());
        }

        let emit_internal = |msg| {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg,
                    span: empty_span(),
                },
            })
        };

        // For each of the newly evaluated intialisers we need to update the expressions in the
        // consts map.
        let all_const_immediates = evaluator.into_values();
        for new_path in new_immediates {
            let init_span: Span = self
                .consts
                .get(&new_path)
                .map(|cnst| self.expr_key_to_span(cnst.expr))
                .unwrap_or_else(empty_span);

            if let Some(imm_value) = all_const_immediates.get(&new_path) {
                // Before we overwrite the unevaluated value with the immediate value we want to
                // preserve enum variant types, since they'll become `int`s and lose this
                // information below.
                let mut preserved_enum_type = None;
                if let Some(Const { expr, decl_ty }) = self.consts.get(&new_path) {
                    if decl_ty.is_unknown() {
                        if let Expr::Path(path, _) = expr.get(self) {
                            // We have an unknown-typed const initialised with a path.  E.g.,
                            // const a = MyEnum::MyVariant;
                            if let Ok(Inference::Type(variant_ty)) =
                                self.infer_enum_variant_by_name(path)
                            {
                                // It's definitely an enum.  Update the decl type below.
                                preserved_enum_type = Some(variant_ty);
                            }
                        }
                    }
                }

                let new_expr = Expr::Immediate {
                    value: imm_value.clone(),
                    span: init_span,
                };

                let new_expr_key = self.exprs.insert(new_expr, imm_value.get_ty(None));

                if let Some(cnst) = self.consts.get_mut(&new_path) {
                    cnst.expr = new_expr_key;
                    if let Some(variant_ty) = preserved_enum_type {
                        cnst.decl_ty = variant_ty;
                    }
                } else {
                    emit_internal("missing const decl for immediate update");
                }
            } else {
                emit_internal("missing immediate value for const expr update");
            }
        }

        // And for all const decls with an unknown decl type we need to update that too.
        let mut type_replacements: Vec<(String, Type)> = Vec::default();
        for (path, Const { expr, decl_ty, .. }) in &self.consts {
            if decl_ty.is_unknown() {
                if let Some(imm_value) = all_const_immediates.get(path) {
                    // Check the type is valid.
                    let span = self.expr_key_to_span(*expr);
                    match self.infer_immediate(handler, imm_value, &span) {
                        Inference::Type(new_ty) => type_replacements.push((path.clone(), new_ty)),

                        Inference::Ignored
                        | Inference::Dependant(_)
                        | Inference::Dependencies(_)
                        | Inference::BoundDependencies { .. } => {
                            emit_internal("const inferred a dependant type");
                        }
                    }
                } else {
                    emit_internal("missing immediate value for const decl_ty update");
                }
            }
        }

        for (path, new_ty) in type_replacements {
            self.consts.get_mut(&path).unwrap().decl_ty = new_ty;
        }

        handler.result(())
    }
}
