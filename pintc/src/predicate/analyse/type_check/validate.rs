use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler, LargeTypeError},
    predicate::{Const, Contract, Expr, Interface, Param, StorageVar},
    span::{empty_span, Spanned},
    types::{NewTypeDecl, PrimitiveKind, Type, UnionDecl},
};

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
                Type::FixedArray { ty, .. } | Type::UnsizedArray { ty, .. } => {
                    check_custom_type(ty, handler)
                }
                Type::Tuple { fields, .. } => fields
                    .iter()
                    .for_each(|(_, field)| check_custom_type(field, handler)),
                Type::Custom { span, .. } => {
                    handler.emit_err(Error::Compile {
                        error: CompileError::UndefinedType { span: span.clone() },
                    });
                }
                Type::Alias { ty, .. } => check_custom_type(ty, handler),
                Type::Optional { ty, .. } => check_custom_type(ty, handler),
                Type::Map { ty_from, ty_to, .. } => {
                    check_custom_type(ty_from, handler);
                    check_custom_type(ty_to, handler);
                }
                Type::Error(_)
                | Type::Unknown(_)
                | Type::Any(_)
                | Type::Nil(_)
                | Type::KeyValue(_)
                | Type::Primitive { .. }
                | Type::Union { .. } => {}
            }
        }

        // Now, check predicate parameters, variable variables, and cast expressions, in every predicate
        for (pred_key, pred) in self.preds.iter() {
            pred.variables().for_each(|(variable_key, _)| {
                check_custom_type(variable_key.get_ty(pred), handler)
            });

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
        for (_id, Const { decl_ty, .. }) in &self.consts {
            check_custom_type(decl_ty, handler);
        }

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
                if !expr_type.is_bool() && !expr_type.is_key_value() {
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

    /// Check that all predicate parameters and variable variables do not have storage only types
    /// like storage maps
    pub(in crate::predicate::analyse) fn check_types_of_variables(
        &self,
        handler: &Handler,
    ) -> Result<(), ErrorEmitted> {
        for pred in self.preds.values() {
            // Disallow predicate parameters from having storage only types
            pred.params.iter().for_each(|param| {
                let ty = &param.ty;
                if let Some(nested_ty) = ty.get_storage_only_ty(self) {
                    handler.emit_err(Error::Compile {
                        error: CompileError::ParamHasStorageType {
                            ty: self.with_ctrct(ty).to_string(),
                            nested_ty: self.with_ctrct(nested_ty).to_string(),
                            span: param.span.clone(),
                        },
                    });
                }
            });

            // Disallow local variables from having storage only types
            pred.variables().for_each(|(variable_key, variable)| {
                let ty = variable_key.get_ty(pred);
                if let Some(nested_ty) = ty.get_storage_only_ty(self) {
                    handler.emit_err(Error::Compile {
                        error: CompileError::VarHasStorageType {
                            ty: self.with_ctrct(ty).to_string(),
                            nested_ty: self.with_ctrct(nested_ty).to_string(),
                            span: variable.span.clone(),
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
        for (
            _id,
            Const {
                expr: init_expr_key,
                decl_ty,
                ..
            },
        ) in &self.consts
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

    pub(in crate::predicate::analyse) fn check_uninferrable_types(
        &mut self,
        handler: &Handler,
    ) -> Result<(), ErrorEmitted> {
        fn check_any_type(ty: &Type, handler: &Handler) {
            match ty {
                Type::Nil(span) => {
                    handler.emit_err(Error::Compile {
                        error: CompileError::UninferrableType { span: span.clone() },
                    });
                }
                Type::FixedArray { ty, .. } | Type::UnsizedArray { ty, .. } => {
                    check_any_type(ty, handler)
                }
                Type::Tuple { fields, .. } => fields
                    .iter()
                    .for_each(|(_, field)| check_any_type(field, handler)),
                Type::Alias { ty, .. } => check_any_type(ty, handler),
                Type::Optional { ty, .. } => check_any_type(ty, handler),
                Type::Map { ty_from, ty_to, .. } => {
                    check_any_type(ty_from, handler);
                    check_any_type(ty_to, handler);
                }
                Type::Error(_)
                | Type::Any(_)
                | Type::Unknown(_)
                | Type::KeyValue(_)
                | Type::Custom { .. }
                | Type::Primitive { .. }
                | Type::Union { .. } => {}
            }
        }

        // Check all variables and expressions
        for (pred_key, _) in self.preds.iter() {
            self.exprs(pred_key).for_each(|expr_key| {
                check_any_type(expr_key.get_ty(self), handler);
            });
        }

        // TODO: Check consts. We're not type checking constants at all currently so that has to be
        // handled first

        handler.result(())
    }
}
