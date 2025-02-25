pub(super) mod check_decls;
pub(super) mod check_exprs;
pub(super) mod check_paths;
pub(super) mod custom_types;
pub(super) mod validate;

use super::Inference;
use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler, LargeTypeError},
    predicate::{Const, ConstraintDecl, Contract, Expr, ExprKey, Ident, VisitorKind},
    span::{Span, Spanned},
    types::Type,
};
use fxhash::{FxHashMap, FxHashSet};

impl Contract {
    pub(super) fn type_check_all(&mut self, handler: &Handler) -> Result<(), ErrorEmitted> {
        // Check all the const expr decls before predicates.
        let const_expr_keys = self
            .consts
            .iter()
            .map(|(_, Const { expr, .. })| *expr)
            .collect::<Vec<_>>();
        for expr_key in const_expr_keys {
            let _ = self.type_check_single_expr(handler, None, expr_key);
        }

        for pred_key in self.preds.keys().collect::<Vec<_>>() {
            // Check all the 'root' exprs (constraints, variable init exprs) one at a time, gathering
            // errors as we go. Copying the keys out first to avoid borrowing conflict.
            let all_expr_keys = self.preds[pred_key]
                .constraints
                .iter()
                .map(|ConstraintDecl { expr: key, .. }| *key)
                .chain(
                    self.preds[pred_key]
                        .variables()
                        .map(|(_, variable)| variable.expr),
                )
                .collect::<Vec<_>>();

            for expr_key in all_expr_keys {
                let _ = self.type_check_single_expr(handler, Some(pred_key), expr_key);
            }

            // Now check all `if` declarations.  NOTE: to avoid cloning we could put all IfDecls
            // and MatchDecls into the Contract and use a slotmap to refer to them.
            self.preds[pred_key]
                .if_decls
                .clone()
                .iter()
                .for_each(|if_decl| self.type_check_if_decl(handler, Some(pred_key), if_decl));

            // And all `match` declarations.
            self.preds[pred_key]
                .match_decls
                .clone()
                .iter()
                .for_each(|match_decl| {
                    self.type_check_match_decl(handler, Some(pred_key), match_decl)
                });

            let mut init_exprs_types = FxHashMap::default();

            // Confirm now that all variable variables are typed.
            let mut variable_key_to_new_type = FxHashMap::default();
            for (variable_key, variable) in self.preds[pred_key].variables() {
                let variable_ty = variable_key.get_ty(&self.preds[pred_key]);
                let expr_ty = variable.expr.get_ty(self);

                match (variable_ty, expr_ty) {
                    (Type::Unknown(_), Type::Unknown(_)) => {
                        handler.emit_err(Error::Compile {
                            error: CompileError::UnknownType {
                                span: variable.span.clone(),
                            },
                        });
                    }
                    (Type::Unknown(_), _) => {
                        // Infer variable type from its initializer
                        variable_key_to_new_type.insert(variable_key, expr_ty.clone());
                    }
                    (_, Type::Unknown(_)) => {
                        handler.emit_err(Error::Compile {
                            error: CompileError::UnknownType {
                                span: variable.span.clone(),
                            },
                        });
                    }
                    (_, Type::Any(_)) => {
                        // Infer expression type from the specified type of the variable
                        init_exprs_types.insert(variable.expr, variable_ty.clone());
                    }
                    _ => {
                        if !variable_ty.eq(self, expr_ty) {
                            // Mismatch type error
                            handler.emit_err(Error::Compile {
                                error: CompileError::VarInitTypeError {
                                    large_err: Box::new(LargeTypeError::VarInitTypeError {
                                        expected_ty: self.with_ctrct(variable_ty).to_string(),
                                        found_ty: self.with_ctrct(expr_ty).to_string(),
                                        span: self.expr_key_to_span(variable.expr),
                                        expected_span: Some(variable_ty.span().clone()),
                                    }),
                                },
                            });
                        }
                    }
                }
            }

            // Now update the types of the initializer exprs if we couldn't infer them before
            for (expr, ty) in init_exprs_types {
                expr.set_ty(ty, self);
            }

            self.preds
                .get_mut(pred_key)
                .unwrap()
                .variables
                .update_types(|variable_key, ty| {
                    if let Some(new_ty) = variable_key_to_new_type.get(&variable_key) {
                        *ty = new_ty.clone()
                    }
                });

            // Last thing we have to do is to type check all the range expressions in array types and
            // make sure they are integers or enums
            let mut checked_range_exprs = FxHashSet::default();
            for range_expr in self.preds[pred_key]
                .params
                .iter()
                .filter_map(|param| param.ty.get_array_range_expr())
                .chain(
                    self.preds[pred_key]
                        .variables()
                        .filter_map(|(variable_key, _)| {
                            variable_key
                                .get_ty(&self.preds[pred_key])
                                .get_array_range_expr()
                        }),
                )
                .chain(
                    self.exprs(pred_key)
                        .filter_map(|expr_key| expr_key.get_ty(self).get_array_range_expr()),
                )
                .collect::<Vec<_>>()
                .iter()
            {
                if self
                    .type_check_single_expr(handler, Some(pred_key), *range_expr)
                    .is_ok()
                    && !(range_expr.get_ty(self).is_int()
                        || range_expr.get_ty(self).is_enumeration_union(self)
                        || checked_range_exprs.contains(range_expr))
                {
                    handler.emit_err(Error::Compile {
                        error: CompileError::InvalidArrayRangeType {
                            found_ty: self.with_ctrct(range_expr.get_ty(self)).to_string(),
                            span: self.expr_key_to_span(*range_expr),
                        },
                    });
                    // Make sure to not collect too many duplicate errors
                    checked_range_exprs.insert(range_expr);
                }
            }
        }

        handler.result(())
    }

    fn set_path_exprs_to_type(&mut self, name: &Ident, ty: &Type, expr_key: ExprKey) {
        let mut path_exprs = Vec::default();

        self.visitor_from_key(
            VisitorKind::DepthFirstParentsBeforeChildren,
            expr_key,
            &mut |path_expr_key, expr| {
                if let Expr::Path(path, _span) = expr {
                    // Paths have the `::` prefix.
                    if path.len() > 2 && path[2..] == name.name {
                        path_exprs.push(path_expr_key);
                    }
                }
            },
        );

        for path_expr_key in path_exprs {
            path_expr_key.set_ty(ty.clone(), self);
        }
    }

    fn type_check_match_binding(
        &self,
        handler: &Handler,
        union_ty: &Type,
        variant_name: &String,
        name_span: &Span,
        binding: &Option<Ident>,
    ) -> Result<Option<Type>, ErrorEmitted> {
        // If the binding exists find the type in the union.
        if let Ok(binding_ty) = union_ty.get_union_variant_ty(self, variant_name) {
            if binding_ty.is_some() && binding.is_none() {
                #[allow(clippy::unnecessary_unwrap)]
                handler.emit_err(Error::Compile {
                    error: CompileError::MissingUnionExprValue {
                        name: variant_name.to_string(),
                        variant_ty: self.with_ctrct(binding_ty.unwrap()).to_string(),
                        span: name_span.clone(),
                    },
                });
            } else if binding_ty.is_none() && binding.is_some() {
                handler.emit_err(Error::Compile {
                    error: CompileError::SuperfluousUnionExprValue {
                        name: variant_name.to_string(),
                        span: name_span.clone(),
                    },
                });
            } else {
                // Everything seems OK.
                return Ok(binding_ty.cloned());
            }
        } else {
            handler.emit_err(Error::Compile {
                error: CompileError::MatchVariantUnknown {
                    variant: variant_name.clone(),
                    union_name: union_ty
                        .get_union_name(self)
                        .cloned()
                        .unwrap_or("<unknown union>".to_string()),
                    actual_variants: union_ty.get_union_variant_names(self),
                    span: name_span.clone(),
                },
            });
        }

        // If we get this far then there was an error.
        Err(handler.cancel())
    }
}
