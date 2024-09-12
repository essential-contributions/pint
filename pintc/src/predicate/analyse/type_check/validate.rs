use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler, LargeTypeError},
    predicate::{Const, Contract, Expr, Interface, InterfaceVar, StorageVar},
    span::{empty_span, Spanned},
    types::{NewTypeDecl, PrimitiveKind, Type, UnionDecl},
};
use fxhash::FxHashSet;

impl Contract {
    // Ensure that every `Type::Custom` in the program is actually defined as an enum or as a type
    // alias. Otherwise, collect an error for every violation.
    pub(in crate::predicate::analyse) fn check_undefined_types(
        &mut self,
        handler: &Handler,
    ) -> Result<(), ErrorEmitted> {
        // Helper function that searches for nested undefined type in a given type. It relies on
        // `valid_custom_tys`, which is a set of all custom types that are defined in the contract.
        fn check_custom_type(ty: &Type, handler: &Handler, valid_custom_tys: &FxHashSet<&String>) {
            match ty {
                Type::Array { ty, .. } => check_custom_type(ty, handler, valid_custom_tys),
                Type::Tuple { fields, .. } => fields
                    .iter()
                    .for_each(|(_, field)| check_custom_type(field, handler, valid_custom_tys)),
                Type::Custom { path, span, .. } => {
                    if !valid_custom_tys.contains(&path) {
                        handler.emit_err(Error::Compile {
                            error: CompileError::UndefinedType { span: span.clone() },
                        });
                    }
                }
                Type::Alias { ty, .. } => check_custom_type(ty, handler, valid_custom_tys),
                Type::Map { ty_from, ty_to, .. } => {
                    check_custom_type(ty_from, handler, valid_custom_tys);
                    check_custom_type(ty_to, handler, valid_custom_tys);
                }
                Type::Vector { ty, .. } => check_custom_type(ty, handler, valid_custom_tys),
                Type::Error(_)
                | Type::Unknown(_)
                | Type::Any(_)
                | Type::Primitive { .. }
                | Type::Union { .. } => {}
            }
        }

        // The set of all available custom types. These are either enums, unions or type aliases.
        let valid_custom_tys: FxHashSet<&String> = FxHashSet::from_iter(
            self.enums
                .iter()
                .map(|ed| &ed.name.name)
                .chain(self.unions.iter().map(|un| &un.name.name))
                .chain(self.new_types.iter().map(|ntd| &ntd.name.name)),
        );

        // Now, check decision variables, state variables, and cast expressions, in every predicate
        for (pred_key, pred) in self.preds.iter() {
            pred.states().for_each(|(state_key, _)| {
                check_custom_type(state_key.get_ty(pred), handler, &valid_custom_tys)
            });

            pred.vars().for_each(|(var_key, _)| {
                check_custom_type(var_key.get_ty(pred), handler, &valid_custom_tys)
            });

            self.exprs(pred_key).for_each(|expr_key| {
                check_custom_type(expr_key.get_ty(self), handler, &valid_custom_tys);

                if let Some(Expr::Cast { ty, .. }) = expr_key.try_get(self) {
                    check_custom_type(ty, handler, &valid_custom_tys);
                }
            });
        }

        // Check all constants
        self.consts.values().for_each(|Const { decl_ty, .. }| {
            check_custom_type(decl_ty, handler, &valid_custom_tys)
        });

        // Check all unions.
        for UnionDecl { variants, .. } in &self.unions {
            for variant in variants {
                if let Some(ty) = &variant.ty {
                    check_custom_type(ty, handler, &valid_custom_tys);
                }
            }
        }

        // Check all storage variables in the contract
        if let Some((storage_vars, _)) = self.storage.as_ref() {
            storage_vars
                .iter()
                .for_each(|StorageVar { ty, .. }| check_custom_type(ty, handler, &valid_custom_tys))
        }

        // Check storage variables and public decision variables in every interface
        self.interfaces.iter().for_each(
            |Interface {
                 storage,
                 predicate_interfaces,
                 ..
             }| {
                if let Some((storage_vars, _)) = &storage {
                    storage_vars.iter().for_each(|StorageVar { ty, .. }| {
                        check_custom_type(ty, handler, &valid_custom_tys)
                    });
                }

                predicate_interfaces.iter().for_each(|predicate_interface| {
                    predicate_interface
                        .vars
                        .iter()
                        .for_each(|InterfaceVar { ty, .. }| {
                            check_custom_type(ty, handler, &valid_custom_tys)
                        });
                });
            },
        );

        // Check every type alias declaration
        self.new_types
            .iter()
            .for_each(|NewTypeDecl { ty, .. }| check_custom_type(ty, handler, &valid_custom_tys));

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
    pub(in crate::predicate::analyse) fn check_storage_accesses(&self, handler: &Handler) -> Result<(), ErrorEmitted> {
        let mut bad_storage_accesses = FxHashSet::default();
        for expr in self.root_exprs() {
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

            for interface_instance in &pred.interface_instances {
                bad_storage_accesses
                    .extend(interface_instance.address.collect_storage_accesses(self));
            }

            for predicate_instance in &pred.predicate_instances {
                if let Some(address) = predicate_instance.address {
                    bad_storage_accesses.extend(address.collect_storage_accesses(self));
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

    /// Check that all decision variables and state variables do not have storage only types like
    /// storage maps and storage vectors
    pub(in crate::predicate::analyse) fn check_types_of_variables(
        &self,
        handler: &Handler,
    ) -> Result<(), ErrorEmitted> {
        for pred in self.preds.values() {
            // Disallow decision variables from having storage only types
            pred.vars().for_each(|(var_key, var)| {
                let ty = var_key.get_ty(pred);
                if let Some(nested_ty) = ty.get_storage_only_ty() {
                    handler.emit_err(Error::Compile {
                        error: CompileError::VarHasStorageType {
                            ty: self.with_ctrct(ty).to_string(),
                            nested_ty: self.with_ctrct(nested_ty).to_string(),
                            span: var.span.clone(),
                        },
                    });
                }
            });

            // Disallow state variables from having storage only types
            pred.states().for_each(|(state_key, state)| {
                let ty = state_key.get_ty(pred);
                if let Some(nested_ty) = ty.get_storage_only_ty() {
                    handler.emit_err(Error::Compile {
                        error: CompileError::VarHasStorageType {
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

    // Confirm that all var init exprs and const init exprs match their declared type, if they have
    // one.
    pub(in crate::predicate::analyse) fn check_inits(
        &self,
        handler: &Handler,
    ) -> Result<(), ErrorEmitted> {
        for pred in self.preds.values() {
            // Confirm types for all the variable initialisers first.
            for (var_key, init_expr_key) in &pred.var_inits {
                let var_decl_ty = var_key.get_ty(pred);

                // Reporting an error that we're expecting 'Unknown' is not useful.
                if !var_decl_ty.is_unknown() {
                    let init_ty = init_expr_key.get_ty(self);

                    if !var_decl_ty.eq(&self.new_types, init_ty) {
                        handler.emit_err(Error::Compile {
                            error: CompileError::InitTypeError {
                                init_kind: "variable",
                                large_err: Box::new(LargeTypeError::InitTypeError {
                                    init_kind: "variable",
                                    expected_ty: self.with_ctrct(var_decl_ty).to_string(),
                                    found_ty: self.with_ctrct(init_ty).to_string(),
                                    expected_ty_span: var_decl_ty.span().clone(),
                                    init_span: self.expr_key_to_span(*init_expr_key),
                                }),
                            },
                        });
                    }
                }
            }
        }

        // Now confirm that every const initialiser type matches the const decl type.
        for Const {
            expr: init_expr_key,
            decl_ty,
            ..
        } in self.consts.values()
        {
            let init_ty = init_expr_key.get_ty(self);

            // Special case for enum variants -- they'll have an init_ty of `int`.  So we have
            // an error if the types mismatch and they're _not_ an enum/int combo exception.
            if !(init_ty.eq(&self.new_types, decl_ty)
                || decl_ty.is_enum(&self.enums) && init_ty.is_int())
            {
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
