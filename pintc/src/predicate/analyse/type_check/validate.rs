use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler, LargeTypeError},
    predicate::{Const, Contract, Expr, Interface, Param, StorageVar},
    span::{empty_span, Spanned},
    types::{NewTypeDecl, PrimitiveKind, Type, UnionDecl},
};
use fxhash::FxHashSet;

impl Contract {
    // Ensure that every `Type::Custom` in the program has been removed.  They should already be
    // lowered into aliases or unions.
    pub(in crate::predicate::analyse) fn check_undefined_types(
        &mut self,
        handler: &Handler,
    ) -> Result<(), ErrorEmitted> {
        // Helper function that searches for nested undefined type in a given type.
        fn check_custom_type(ty: &Type, handler: &Handler) {
            match ty {
                Type::Array { ty, .. } => check_custom_type(ty, handler),
                Type::Tuple { fields, .. } => fields
                    .iter()
                    .for_each(|(_, field)| check_custom_type(field, handler)),
                Type::Custom { span, .. } => {
                    handler.emit_err(Error::Compile {
                        error: CompileError::UndefinedType { span: span.clone() },
                    });
                }
                Type::Alias { ty, .. } => check_custom_type(ty, handler),
                Type::Map { ty_from, ty_to, .. } => {
                    check_custom_type(ty_from, handler);
                    check_custom_type(ty_to, handler);
                }
                Type::Vector { ty, .. } => check_custom_type(ty, handler),
                Type::Error(_)
                | Type::Unknown(_)
                | Type::Any(_)
                | Type::Primitive { .. }
                | Type::Union { .. } => {}
            }
        }

        // Now, check predicate parameters, state variables, and cast expressions, in every predicate
        for (pred_key, pred) in self.preds.iter() {
            pred.states()
                .for_each(|(state_key, _)| check_custom_type(state_key.get_ty(pred), handler));

            pred.params
                .iter()
                .for_each(|param| check_custom_type(&param.ty, handler));

            self.exprs(pred_key).for_each(|expr_key| {
                check_custom_type(expr_key.get_ty(self), handler);

                if let Some(Expr::Cast { ty, .. }) = expr_key.try_get(self) {
                    check_custom_type(ty, handler);
                }
            });
        }

        // Check all constants
        self.consts
            .values()
            .for_each(|Const { decl_ty, .. }| check_custom_type(decl_ty, handler));

        // Check all unions.
        for UnionDecl { variants, .. } in self.unions.values() {
            for variant in variants {
                if let Some(ty) = &variant.ty {
                    check_custom_type(ty, handler);
                }
            }
        }

        // Check all storage variables in the contract
        if let Some((storage_vars, _)) = self.storage.as_ref() {
            storage_vars
                .iter()
                .for_each(|StorageVar { ty, .. }| check_custom_type(ty, handler))
        }

        // Check storage variables and predicate parameters in every interface
        self.interfaces.iter().for_each(
            |Interface {
                 storage,
                 predicate_interfaces,
                 ..
             }| {
                if let Some((storage_vars, _)) = &storage {
                    storage_vars
                        .iter()
                        .for_each(|StorageVar { ty, .. }| check_custom_type(ty, handler));
                }

                predicate_interfaces.iter().for_each(|predicate_interface| {
                    predicate_interface
                        .params
                        .iter()
                        .for_each(|Param { ty, .. }| check_custom_type(ty, handler));
                });
            },
        );

        // Check every type alias declaration
        self.new_types
            .iter()
            .for_each(|NewTypeDecl { ty, .. }| check_custom_type(ty, handler));

        handler.result(())
    }

    pub(in crate::predicate::analyse) fn check_constraint_types(
        &self,
        handler: &Handler,
    ) -> Result<(), ErrorEmitted> {
        for pred in self.preds.values() {
            // After all expression types are inferred, then all constraint expressions must be of
            // type bool
            pred.constraints.iter().for_each(|constraint_decl| {
                let expr_type = constraint_decl.expr.get_ty(self);
                if !expr_type.is_bool() {
                    handler.emit_err(Error::Compile {
                        error: CompileError::ConstraintExpressionTypeError {
                            large_err: Box::new(LargeTypeError::ConstraintExpressionTypeError {
                                expected_ty: self
                                    .with_ctrct(Type::Primitive {
                                        kind: PrimitiveKind::Bool,
                                        span: empty_span(),
                                    })
                                    .to_string(),
                                found_ty: self.with_ctrct(expr_type).to_string(),
                                span: constraint_decl.span.clone(),
                                expected_span: Some(self.expr_key_to_span(constraint_decl.expr)),
                            }),
                        },
                    });
                }
            })
        }

        handler.result(())
    }

    /// Check that all types used in storage are allowed. This should be removed as soon as all
    /// types are supported in storage.
    pub(in crate::predicate::analyse) fn check_storage_types(
        &self,
        handler: &Handler,
    ) -> Result<(), ErrorEmitted> {
        if let Some((storage_vars, _)) = self.storage.as_ref() {
            storage_vars.iter().for_each(|StorageVar { ty, .. }| {
                if !ty.is_allowed_in_storage() {
                    handler.emit_err(Error::Compile {
                        error: CompileError::TypeNotAllowedInStorage {
                            ty: self.with_ctrct(ty).to_string(),
                            span: ty.span().clone(),
                        },
                    });
                }
            })
        }

        handler.result(())
    }

    /// Collect all storage accesses that are *not* in a state var initializer. For each of those
    /// accesses, emit an error. These accesses are not legal.
    pub(in crate::predicate::analyse) fn check_storage_accesses(
        &self,
        handler: &Handler,
    ) -> Result<(), ErrorEmitted> {
        let mut bad_storage_accesses = FxHashSet::default();
        for expr in self.root_array_range_exprs() {
            bad_storage_accesses.extend(expr.collect_storage_accesses(self));
        }

        for Const { expr, .. } in self.consts.values() {
            bad_storage_accesses.extend(expr.collect_storage_accesses(self));
        }

        for pred in self.preds.values() {
            for constraint in &pred.constraints {
                bad_storage_accesses.extend(constraint.expr.collect_storage_accesses(self));
            }

            for if_decl in &pred.if_decls {
                for constraint in &if_decl.get_constraints() {
                    bad_storage_accesses.extend(constraint.expr.collect_storage_accesses(self));
                }
            }

            for match_decl in &pred.match_decls {
                for constraint in &match_decl.get_constraints() {
                    bad_storage_accesses.extend(constraint.expr.collect_storage_accesses(self));
                }
            }
        }

        for expr in bad_storage_accesses {
            handler.emit_err(Error::Compile {
                error: CompileError::InvalidStorageAccess {
                    span: expr
                        .try_get(self)
                        .map(|expr| expr.span().clone())
                        .unwrap_or(empty_span()),
                },
            });
        }

        handler.result(())
    }

    /// Check that all predicate parameters and state variables do not have storage only types like
    /// storage maps and storage vectors
    pub(in crate::predicate::analyse) fn check_types_of_variables(
        &self,
        handler: &Handler,
    ) -> Result<(), ErrorEmitted> {
        for pred in self.preds.values() {
            // Disallow predicate parameters from having storage only types
            pred.params.iter().for_each(|param| {
                let ty = &param.ty;
                if let Some(nested_ty) = ty.get_storage_only_ty() {
                    handler.emit_err(Error::Compile {
                        error: CompileError::ParamHasStorageType {
                            ty: self.with_ctrct(ty).to_string(),
                            nested_ty: self.with_ctrct(nested_ty).to_string(),
                            span: param.span.clone(),
                        },
                    });
                }
            });

            // Disallow state variables from having storage only types
            pred.states().for_each(|(state_key, state)| {
                let ty = state_key.get_ty(pred);
                if let Some(nested_ty) = ty.get_storage_only_ty() {
                    handler.emit_err(Error::Compile {
                        error: CompileError::ParamHasStorageType {
                            ty: self.with_ctrct(ty).to_string(),
                            nested_ty: self.with_ctrct(nested_ty).to_string(),
                            span: state.span.clone(),
                        },
                    });
                }
            });
        }

        handler.result(())
    }

    // Confirm that all const init exprs match their declared type, if they have one.
    pub(in crate::predicate::analyse) fn check_inits(
        &self,
        handler: &Handler,
    ) -> Result<(), ErrorEmitted> {
        // Now confirm that every const initialiser type matches the const decl type.
        for Const {
            expr: init_expr_key,
            decl_ty,
            ..
        } in self.consts.values()
        {
            let init_ty = init_expr_key.get_ty(self);

            if !init_ty.eq(self, decl_ty) {
                handler.emit_err(Error::Compile {
                    error: CompileError::InitTypeError {
                        init_kind: "const",
                        large_err: Box::new(LargeTypeError::InitTypeError {
                            init_kind: "const",
                            expected_ty: self.with_ctrct(decl_ty).to_string(),
                            found_ty: self.with_ctrct(init_ty).to_string(),
                            expected_ty_span: decl_ty.span().clone(),
                            init_span: self.expr_key_to_span(*init_expr_key),
                        }),
                    },
                });
            }
        }

        handler.result(())
    }
}
