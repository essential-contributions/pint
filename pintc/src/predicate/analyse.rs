mod array_check;
mod intrinsics;
mod type_check;

use super::{
    BlockStatement, Const, ConstraintDecl, Contract, Expr, ExprKey, Ident, IfDecl,
    InterfaceInstance, Predicate, PredicateInstance, VarKey,
};
use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::evaluate::Evaluator,
    span::{empty_span, Span, Spanned},
    types::Type,
};

enum Inference {
    Ignored,
    Type(Type),
    Dependant(ExprKey),
    Dependencies(Vec<ExprKey>),
}

impl Contract {
    pub fn type_check(mut self, handler: &Handler) -> Result<Self, ErrorEmitted> {
        self = handler.scope(|handler| self.check_contract(handler))?;

        // Evaluate all the constant decls to ensure they're all immediates. Each Const expr is
        // updated and has its type set.
        handler.scope(|handler| self.evaluate_all_consts(handler))?;

        // We're temporarily blocking non-primitive consts until we refactor all expressions back
        // into the Contract (Predicates will contain only their local vars, nothing more).
        self.reject_non_primitive_consts(handler)?;

        for pred in self.preds.values() {
            for expr_key in self.exprs(pred) {
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

        self.check_undefined_types(handler);
        self.lower_newtypes(handler)?;
        self.check_storage_types(handler);
        self.type_check_all_exprs(handler);
        self.check_inits(handler);
        self.check_constraint_types(handler);

        if handler.has_errors() {
            Err(handler.cancel())
        } else {
            Ok(self)
        }
    }

    pub fn array_check(self, handler: &Handler) -> Result<Self, ErrorEmitted> {
        for pred in self.preds.values() {
            pred.check_array_lengths(&self, handler);
            pred.check_array_indexing(&self, handler);
            pred.check_array_compares(&self, handler);
        }

        if handler.has_errors() {
            Err(handler.cancel())
        } else {
            Ok(self)
        }
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

        let mut evaluator = Evaluator::new(self.root_pred());
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
                    } else if let Ok(imm) = evaluator.evaluate_key(
                        &cnst.expr,
                        &tmp_handler,
                        self,
                        &self.root_pred().name,
                    ) {
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
                tmp_handler.clear();
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
                        if let Expr::PathByName(path, _) = expr.get(self) {
                            // We have an unknown-typed const initialised with a path.  E.g.,
                            // const a = MyEnum::MyVariant;
                            if let Ok(Inference::Type(variant_ty)) =
                                Self::infer_enum_variant_by_name(
                                    self.root_pred(),
                                    path,
                                    &empty_span(),
                                )
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
                    match self.infer_immediate(self.root_pred(), imm_value, &span) {
                        Ok(inferred) => match inferred {
                            Inference::Type(new_ty) => {
                                type_replacements.push((path.clone(), new_ty))
                            }

                            Inference::Ignored
                            | Inference::Dependant(_)
                            | Inference::Dependencies(_) => {
                                emit_internal("const inferred a dependant type");
                            }
                        },

                        Err(err) => {
                            handler.emit_err(err);
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

        if handler.has_errors() {
            Err(handler.cancel())
        } else {
            Ok(())
        }
    }

    fn reject_non_primitive_consts(&self, handler: &Handler) -> Result<(), ErrorEmitted> {
        // Called after we've evaluated them all and resolved their types.
        for (path, Const { expr, decl_ty }) in &self.consts {
            if !decl_ty.is_any_primitive() && !decl_ty.is_enum(self.root_pred()) {
                handler.emit_err(Error::Compile {
                    error: CompileError::TemporaryNonPrimitiveConst {
                        name: path.to_string(),
                        span: expr.get(self).span().clone(),
                    },
                });
            }
        }

        if handler.has_errors() {
            Err(handler.cancel())
        } else {
            Ok(())
        }
    }
}
