use super::{
    BlockStatement, Const, ConstraintDecl, Contract, Expr, ExprKey, Ident, IfDecl, Inference,
    InterfaceInstance, MatchDecl, Predicate, PredicateInstance, VarKey,
};
use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler, LargeTypeError},
    expr::{
        BinaryOp, ExternalIntrinsic, GeneratorKind, Immediate, IntrinsicKind, MatchBranch,
        MatchElse, TupleAccess, UnaryOp,
    },
    predicate::{Interface, InterfaceVar, MatchDeclBranch, PredKey, StorageVar, VisitorKind},
    span::{empty_span, Span, Spanned},
    types::{
        b256, EnumDecl, EphemeralDecl, NewTypeDecl, Path, PrimitiveKind, Type, UnionDecl,
        UnionVariant,
    },
    warning::Warning,
};
use fxhash::{FxHashMap, FxHashSet};

impl Contract {
    /// Lower every `Type::Custom` type in a contract to a `Type::Alias` type if the custom type
    /// actually matches one of the new type declarations in the contract.
    pub(super) fn lower_custom_types(&mut self, handler: &Handler) -> Result<(), ErrorEmitted> {
        self.check_recursive_newtypes(handler)?;
        self.lower_custom_types_in_newtypes(handler)?;
        self.lower_custom_types_in_contract();

        Ok(())
    }

    fn check_recursive_newtypes(&self, handler: &Handler) -> Result<(), ErrorEmitted> {
        fn inspect_type_names<'a>(
            handler: &Handler,
            contract: &'a Contract,
            seen_names: &mut FxHashMap<&'a str, &'a Span>,
            ty: &'a Type,
        ) -> Result<(), ErrorEmitted> {
            match ty {
                Type::Array { ty, .. } => inspect_type_names(handler, contract, seen_names, ty),

                Type::Tuple { fields, .. } => fields
                    .iter()
                    .try_for_each(|(_, ty)| inspect_type_names(handler, contract, seen_names, ty)),

                Type::Union { path, .. } => {
                    // This was a custom type which has been confirmed to be a union.
                    let Some(union_decl) = contract.unions.iter().find(|ud| &ud.name.name == path)
                    else {
                        unreachable!("union type with unknown path");
                    };

                    for variant in &union_decl.variants {
                        if let Some(ty) = &variant.ty {
                            inspect_type_names(handler, contract, seen_names, ty)?;
                        }
                    }

                    Ok(())
                }

                Type::Custom {
                    path,
                    span: custom_ty_span,
                } => {
                    // Look-up the name to confirm it's a new-type.
                    if let Some((new_ty, new_ty_span)) =
                        contract
                            .new_types
                            .iter()
                            .find_map(|NewTypeDecl { name, ty, span }| {
                                (path == &name.name).then_some((ty, span))
                            })
                    {
                        // This is a new-type; have we seen it before?
                        if let Some(seen_span) = seen_names.get(path.as_str()) {
                            // We have!  Bad news.
                            Err(handler.emit_err(Error::Compile {
                                error: CompileError::RecursiveNewType {
                                    name: path.to_string(),
                                    decl_span: (*seen_span).clone(),
                                    use_span: custom_ty_span.clone(),
                                },
                            }))
                        } else {
                            // We need to add then remove the new path to the 'seen' list; if we
                            // don't remove it we'll get false positives.
                            seen_names.insert(path.as_str(), new_ty_span);
                            let res = inspect_type_names(handler, contract, seen_names, new_ty);
                            seen_names.remove(path.as_str());
                            res
                        }
                    } else {
                        // Will be a path to an enum or variant.
                        Ok(())
                    }
                }

                Type::Alias { ty, .. } => inspect_type_names(handler, contract, seen_names, ty),

                Type::Map { ty_from, ty_to, .. } => {
                    inspect_type_names(handler, contract, seen_names, ty_from)?;
                    inspect_type_names(handler, contract, seen_names, ty_to)
                }

                Type::Vector { ty, .. } => inspect_type_names(handler, contract, seen_names, ty),

                Type::Error(_) | Type::Unknown(_) | Type::Any(_) | Type::Primitive { .. } => Ok(()),
            }
        }

        for NewTypeDecl { name, ty, span } in &self.new_types {
            let mut seen_names = FxHashMap::from_iter(std::iter::once((name.name.as_str(), span)));

            let _ = inspect_type_names(handler, self, &mut seen_names, ty);
        }

        handler.result(())
    }

    /// Lower every `Type::Custom` type in a new type declarations to a `Type::Alias` type if the
    /// custom type actually matches one of the new type declarations in the contract. All
    /// remaining custom types after this method runs should refer to enums.
    fn lower_custom_types_in_newtypes(&mut self, handler: &Handler) -> Result<(), ErrorEmitted> {
        // Search for a custom type with a specific name and return a mut ref to it.
        fn get_custom_type_mut_ref<'a>(
            custom_path: &str,
            ty: &'a mut Type,
        ) -> Option<&'a mut Type> {
            match ty {
                Type::Array { ty, .. } => get_custom_type_mut_ref(custom_path, ty),
                Type::Tuple { fields, .. } => fields
                    .iter_mut()
                    .find_map(|(_, fld_ty)| get_custom_type_mut_ref(custom_path, fld_ty)),
                Type::Custom { path, .. } => (path == custom_path).then_some(ty),
                Type::Alias { ty, .. } => get_custom_type_mut_ref(custom_path, ty),
                Type::Map { ty_from, ty_to, .. } => get_custom_type_mut_ref(custom_path, ty_from)
                    .or_else(|| get_custom_type_mut_ref(custom_path, ty_to)),
                Type::Vector { ty, .. } => get_custom_type_mut_ref(custom_path, ty),
                Type::Error(_)
                | Type::Unknown(_)
                | Type::Any(_)
                | Type::Primitive { .. }
                | Type::Union { .. } => None,
            }
        }

        // Any custom types referred to *within new types* need to be converted to type aliases.
        // E.g.,
        //   type A = int;
        //   type B = { A, A };
        //   // B will have `Type::Custom("A")` which need to be `Type::Alias("A", int)`

        for new_type_idx in 0..self.new_types.len() {
            // We're replacing only a single new type at a time, if found in other new-types.
            let new_type = self.new_types[new_type_idx].clone();

            // Replace the next found custom type which needs to be replaced with a an alias.
            // There may be multiple replacements required per iteration, so we'll visit every
            // current new-type decl per iteration until none are updated.
            for loop_check in 0.. {
                let mut modified = false;

                for NewTypeDecl { ref mut ty, .. } in &mut self.new_types {
                    if let Some(custom_ty) = get_custom_type_mut_ref(&new_type.name.name, ty) {
                        *custom_ty = Type::Alias {
                            path: new_type.name.name.clone(),
                            ty: Box::new(new_type.ty.clone()),
                            span: new_type.span.clone(),
                        };

                        modified = true;
                    }
                }

                if !modified {
                    break;
                }

                if loop_check > 10_000 {
                    return Err(handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "infinite loop in lower_custom_types_in_newtypes()",
                            span: empty_span(),
                        },
                    }));
                }
            }
        }

        Ok(())
    }

    /// Lower every `Type::Custom` type in a contract to a `Type::Alias` type if the custom type
    /// actually matches one of the new type declarations in the contract. All remaining custom
    /// types after this method runs should refer to enums.
    fn lower_custom_types_in_contract(&mut self) {
        use std::borrow::BorrowMut;

        // Given a mutable reference to a `Type` and a list of new type declarations `new_types`,
        // replace it or its subtypes with a `Type::Alias` when a `Type::Custom` is encountered
        // that matches the name of a declaration in `new_types`.
        fn replace_custom_type(new_types: &[NewTypeDecl], union_names: &[String], ty: &mut Type) {
            match ty {
                Type::Array { ty, .. } => {
                    replace_custom_type(new_types, union_names, ty.borrow_mut())
                }
                Type::Tuple { fields, .. } => fields
                    .iter_mut()
                    .for_each(|(_, ty)| replace_custom_type(new_types, union_names, ty)),
                Type::Custom { path, span } => {
                    let path = path.clone();
                    if let Some((new_ty, new_span)) =
                        new_types.iter().find_map(|NewTypeDecl { name, ty, span }| {
                            (name.name == path).then_some((ty, span))
                        })
                    {
                        *ty = Type::Alias {
                            path,
                            ty: Box::new(new_ty.clone()),
                            span: new_span.clone(),
                        };
                    } else if union_names.iter().any(|name| name == &path) {
                        *ty = Type::Union {
                            path,
                            span: span.clone(),
                        }
                    }
                }
                Type::Alias { ty, .. } => replace_custom_type(new_types, union_names, ty),
                Type::Map { ty_from, ty_to, .. } => {
                    replace_custom_type(new_types, union_names, ty_from);
                    replace_custom_type(new_types, union_names, ty_to);
                }
                Type::Vector { ty, .. } => replace_custom_type(new_types, union_names, ty),
                Type::Error(_)
                | Type::Unknown(_)
                | Type::Any(_)
                | Type::Primitive { .. }
                | Type::Union { .. } => {}
            }
        }

        let union_names = self
            .unions
            .iter()
            .map(|UnionDecl { name, .. }| name.name.clone())
            .collect::<Vec<_>>();

        for UnionDecl { variants, .. } in &mut self.unions {
            for variant in variants {
                if let Some(variant_ty) = &mut variant.ty {
                    replace_custom_type(&self.new_types, &union_names, variant_ty);
                }
            }
        }

        // Replace custom types in predicates
        self.preds.values_mut().for_each(|pred| {
            // Replace custom types in vars and states
            pred.vars
                .update_types(|_, ty| replace_custom_type(&self.new_types, &union_names, ty));
            pred.states
                .update_types(|_, ty| replace_custom_type(&self.new_types, &union_names, ty));

            // Replace custom types in any `as` cast expression
            self.exprs.update_exprs(|_, expr| {
                if let Expr::Cast { ty, .. } = expr {
                    replace_custom_type(&self.new_types, &union_names, ty.borrow_mut());
                }
            });
        });

        // Replace custom types in `const` declarations
        self.consts.values_mut().for_each(|Const { decl_ty, .. }| {
            replace_custom_type(&self.new_types, &union_names, decl_ty)
        });

        // Replace custom types in every storage variable in the contract
        if let Some((storage_vars, _)) = self.storage.as_mut() {
            storage_vars.iter_mut().for_each(|StorageVar { ty, .. }| {
                replace_custom_type(&self.new_types, &union_names, ty);
            })
        }

        // Replace custom types in interfaces
        self.interfaces.iter_mut().for_each(
            |Interface {
                 storage,
                 predicate_interfaces,
                 ..
             }| {
                // Replace custom types in every storage variable in the interface
                if let Some((storage_vars, _)) = storage.as_mut() {
                    storage_vars.iter_mut().for_each(|StorageVar { ty, .. }| {
                        replace_custom_type(&self.new_types, &union_names, ty);
                    });
                }

                // Replace custom types in every decision variable in the interface. These belong
                // to the various predicate interfaces
                predicate_interfaces
                    .iter_mut()
                    .for_each(|predicate_interface| {
                        predicate_interface.vars.iter_mut().for_each(
                            |InterfaceVar { ty, .. }| {
                                replace_custom_type(&self.new_types, &union_names, ty)
                            },
                        );
                    });
            },
        );
    }

    // Ensure that every `Type::Custom` in the program is actually defined as an enum or as a type
    // alias. Otherwise, collect an error for every violation.
    pub(super) fn check_undefined_types(&mut self, handler: &Handler) -> Result<(), ErrorEmitted> {
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

    pub(super) fn check_constraint_types(&self, handler: &Handler) -> Result<(), ErrorEmitted> {
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
    pub(super) fn check_storage_types(&self, handler: &Handler) -> Result<(), ErrorEmitted> {
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

    /// Check that all decision variables and state variables do not have storage only types like
    /// storage maps and storage vectors
    pub(super) fn check_types_of_variables(&self, handler: &Handler) -> Result<(), ErrorEmitted> {
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

    fn set_path_exprs_in_block_to_type(&mut self, name: &Ident, ty: &Type, block: &BlockStatement) {
        match block {
            BlockStatement::Constraint(ConstraintDecl { expr, .. }) => {
                self.set_path_exprs_to_type(name, ty, *expr)
            }

            BlockStatement::If(IfDecl {
                condition,
                then_block,
                else_block,
                ..
            }) => {
                self.set_path_exprs_to_type(name, ty, *condition);

                for stmt in then_block {
                    self.set_path_exprs_in_block_to_type(name, ty, stmt);
                }

                if let Some(else_block) = else_block {
                    for stmt in else_block {
                        self.set_path_exprs_in_block_to_type(name, ty, stmt);
                    }
                }
            }

            BlockStatement::Match(MatchDecl {
                match_expr,
                match_branches,
                else_branch,
                ..
            }) => {
                self.set_path_exprs_to_type(name, ty, *match_expr);

                for MatchDeclBranch {
                    binding: _, block, ..
                } in match_branches
                {
                    // TODO: don't set type if binding shadows this one?  Do we allow it?
                    for stmt in block {
                        self.set_path_exprs_in_block_to_type(name, ty, stmt);
                    }
                }

                if let Some(else_block) = else_branch {
                    for stmt in else_block {
                        self.set_path_exprs_in_block_to_type(name, ty, stmt);
                    }
                }
            }
        }
    }

    pub(super) fn type_check_all_exprs(&mut self, handler: &Handler) -> Result<(), ErrorEmitted> {
        // Check all address expressions in instances.
        self.check_iface_inst_addrs(handler);
        self.check_pred_inst_addrs(handler);

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
            // Check all the 'root' exprs (constraints, state init exprs, and var init exprs) one at a
            // time, gathering errors as we go. Copying the keys out first to avoid borrowing conflict.
            let all_expr_keys = self.preds[pred_key]
                .constraints
                .iter()
                .map(|ConstraintDecl { expr: key, .. }| *key)
                .chain(self.preds[pred_key].states().map(|(_, state)| state.expr))
                .chain(self.preds[pred_key].var_inits.iter().map(|(_, expr)| *expr))
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

            // Confirm now that all decision variables are typed.
            let mut var_key_to_new_type = FxHashMap::default();
            for (var_key, var) in self.preds[pred_key].vars() {
                let ty = var_key.get_ty(&self.preds[pred_key]);
                if ty.is_unknown() {
                    if let Some(init_expr_key) = self.preds[pred_key].var_inits.get(var_key) {
                        let ty = init_expr_key.get_ty(self);
                        if !ty.is_unknown() {
                            var_key_to_new_type.insert(var_key, ty.clone());
                        } else {
                            handler.emit_err(Error::Compile {
                                error: CompileError::UnknownType {
                                    span: var.span.clone(),
                                },
                            });
                        }
                    } else {
                        handler.emit_err(Error::Compile {
                            error: CompileError::Internal {
                                msg: "untyped variable has no initialiser",
                                span: var.span.clone(),
                            },
                        });
                    }
                }
            }

            self.preds
                .get_mut(pred_key)
                .unwrap()
                .vars
                .update_types(|var_key, ty| {
                    if let Some(new_ty) = var_key_to_new_type.get(&var_key) {
                        *ty = new_ty.clone()
                    }
                });

            // Confirm now that all state variables are typed.
            let mut state_key_to_new_type = FxHashMap::default();
            for (state_key, state) in self.preds[pred_key].states() {
                let state_ty = state_key.get_ty(&self.preds[pred_key]);
                if !state_ty.is_unknown() {
                    let expr_ty = state.expr.get_ty(self);
                    if !expr_ty.is_unknown() {
                        if !state_ty.eq(&self.new_types, expr_ty) {
                            handler.emit_err(Error::Compile {
                                error: CompileError::StateVarInitTypeError {
                                    large_err: Box::new(LargeTypeError::StateVarInitTypeError {
                                        expected_ty: self.with_ctrct(state_ty).to_string(),
                                        found_ty: self.with_ctrct(expr_ty).to_string(),
                                        span: self.expr_key_to_span(state.expr),
                                        expected_span: Some(state_ty.span().clone()),
                                    }),
                                },
                            });
                        }
                    } else {
                        handler.emit_err(Error::Compile {
                            error: CompileError::UnknownType {
                                span: state.span.clone(),
                            },
                        });
                    }
                } else {
                    let expr_ty = state.expr.get_ty(self).clone();
                    if !expr_ty.is_unknown() {
                        state_key_to_new_type.insert(state_key, expr_ty.clone());
                    } else {
                        handler.emit_err(Error::Compile {
                            error: CompileError::UnknownType {
                                span: state.span.clone(),
                            },
                        });
                    }
                }
            }

            self.preds
                .get_mut(pred_key)
                .unwrap()
                .states
                .update_types(|state_key, ty| {
                    if let Some(new_ty) = state_key_to_new_type.get(&state_key) {
                        *ty = new_ty.clone()
                    }
                });

            // Last thing we have to do is to type check all the range expressions in array types and
            // make sure they are integers or enums
            let mut checked_range_exprs = FxHashSet::default();
            for range_expr in self.preds[pred_key]
                .vars()
                .filter_map(|(var_key, _)| {
                    var_key.get_ty(&self.preds[pred_key]).get_array_range_expr()
                })
                .chain(self.preds[pred_key].states().filter_map(|(state_key, _)| {
                    state_key
                        .get_ty(&self.preds[pred_key])
                        .get_array_range_expr()
                }))
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
                        || range_expr.get_ty(self).is_enum(&self.enums)
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

    fn check_iface_inst_addrs(&mut self, handler: &Handler) {
        for pred_key in self.preds.keys().collect::<Vec<_>>() {
            // Type check all interface instance declarations.
            let mut addr_keys = Vec::default();
            for InterfaceInstance {
                interface,
                address,
                span,
                ..
            } in &self.preds[pred_key].interface_instances
            {
                if self
                    .interfaces
                    .iter()
                    .any(|e| e.name.to_string() == *interface)
                {
                    // OK. Type check this address below.
                    addr_keys.push(*address);
                } else {
                    handler.emit_err(Error::Compile {
                        error: CompileError::MissingInterface {
                            name: interface.clone(),
                            span: span.clone(),
                        },
                    });
                }
            }

            self.check_instance_addresses(handler, Some(pred_key), &addr_keys);
        }
    }

    fn check_pred_inst_addrs(&mut self, handler: &Handler) {
        for pred_key in self.preds.keys().collect::<Vec<_>>() {
            // Type check all predicate instance declarations.
            let mut addr_keys = Vec::default();
            for PredicateInstance {
                interface_instance,
                predicate,
                address,
                span,
                ..
            } in &self.preds[pred_key].predicate_instances
            {
                if let Some(interface_instance) = interface_instance {
                    // Make sure that an appropriate interface instance exists and an appropriate
                    // predicate interface exists.
                    if let Some(interface_instance) = self.preds[pred_key]
                        .interface_instances
                        .iter()
                        .find(|e| e.name.to_string() == *interface_instance)
                    {
                        if let Some(interface) = self
                            .interfaces
                            .iter()
                            .find(|e| e.name.to_string() == *interface_instance.interface)
                        {
                            if interface
                                .predicate_interfaces
                                .iter()
                                .any(|e| e.name.to_string() == *predicate.to_string())
                            {
                                // OK. Type check this address below.
                                if let Some(address) = *address {
                                    addr_keys.push(address);
                                }
                            } else {
                                handler.emit_err(Error::Compile {
                                    error: CompileError::MissingPredicate {
                                        pred_name: predicate.name.to_string(),
                                        interface_name: Some(interface.name.to_string()),
                                        span: span.clone(),
                                    },
                                });
                            }
                        }
                    } else {
                        handler.emit_err(Error::Compile {
                            error: CompileError::MissingInterfaceInstance {
                                name: interface_instance.clone(),
                                span: span.clone(),
                            },
                        });
                    }
                } else {
                    // This predicate instance must reference a local predicate since
                    // `interface_instance` is `None`. If not, emit an error.

                    // Self referential predicates are not allowed.
                    if "::".to_owned() + &predicate.name == self.preds[pred_key].name {
                        handler.emit_err(Error::Compile {
                            error: CompileError::SelfReferencialPredicate {
                                pred_name: predicate.name.to_string(),
                                span: span.clone(),
                            },
                        });
                    }

                    // If the predicate does not exist, emit an error
                    if self
                        .preds
                        .iter()
                        .all(|(_, pred)| pred.name != "::".to_owned() + &predicate.name)
                    {
                        handler.emit_err(Error::Compile {
                            error: CompileError::MissingPredicate {
                                pred_name: predicate.name.to_string(),
                                interface_name: None,
                                span: span.clone(),
                            },
                        });
                    }
                }
            }

            self.check_instance_addresses(handler, Some(pred_key), &addr_keys);
        }
    }

    fn check_instance_addresses(
        &mut self,
        handler: &Handler,
        pred_key: Option<PredKey>,
        addr_keys: &[ExprKey],
    ) {
        for address in addr_keys {
            if self
                .type_check_single_expr(handler, pred_key, *address)
                .is_ok()
            {
                let ty = address.get_ty(self);
                if !ty.is_b256() {
                    handler.emit_err(Error::Compile {
                        error: CompileError::AddressExpressionTypeError {
                            large_err: Box::new(LargeTypeError::AddressExpressionTypeError {
                                expected_ty: self.with_ctrct(b256()).to_string(),
                                found_ty: self.with_ctrct(ty).to_string(),
                                span: self.expr_key_to_span(*address),
                                expected_span: Some(self.expr_key_to_span(*address)),
                            }),
                        },
                    });
                }
            }
        }
    }

    // Type check an if statement and all of its sub-statements including other ifs. This is a
    // recursive function.
    fn type_check_if_decl(
        &mut self,
        handler: &Handler,
        pred_key: Option<PredKey>,
        if_decl: &IfDecl,
    ) {
        let IfDecl {
            condition,
            then_block,
            else_block,
            ..
        } = if_decl;

        // Make sure the condition is a `bool`
        if self
            .type_check_single_expr(handler, pred_key, *condition)
            .is_ok()
        {
            let cond_ty = condition.get_ty(self);
            if !cond_ty.is_bool() {
                handler.emit_err(Error::Compile {
                    error: CompileError::NonBoolConditional {
                        ty: self.with_ctrct(cond_ty).to_string(),
                        conditional: "`if` statement".to_string(),
                        span: self.expr_key_to_span(*condition),
                    },
                });
            }
        }

        // Type check each block statement in the "then" block
        then_block.iter().for_each(|block_statement| {
            self.type_check_block_statement(handler, pred_key, block_statement);
        });

        // Type check each block statement in the "else" block, if available
        else_block.iter().flatten().for_each(|block_statement| {
            self.type_check_block_statement(handler, pred_key, block_statement);
        });
    }

    fn type_check_match_decl(
        &mut self,
        handler: &Handler,
        pred_key: Option<PredKey>,
        match_decl: &MatchDecl,
    ) {
        let MatchDecl {
            match_expr,
            match_branches,
            else_branch,
            span,
        } = match_decl;

        if self
            .type_check_single_expr(handler, pred_key, *match_expr)
            .is_ok()
        {
            let union_ty = match_expr.get_ty(self).clone();

            if !union_ty.is_union(&self.unions) {
                handler.emit_err(Error::Compile {
                    error: CompileError::MatchExprNotUnion {
                        found_ty: self.with_ctrct(union_ty).to_string(),
                        span: self.expr_key_to_span(*match_expr),
                    },
                });
            } else {
                let mut variants_set = FxHashSet::default();

                for MatchDeclBranch {
                    name,
                    name_span,
                    binding,
                    block,
                } in match_branches
                {
                    if let Ok(binding_ty) =
                        self.type_check_match_binding(handler, &union_ty, name, name_span, binding)
                    {
                        // Set all sub-exprs to the bound type.
                        if let (Some(binding_id), Some(binding_ty)) = (binding, binding_ty) {
                            for stmt in block {
                                self.set_path_exprs_in_block_to_type(binding_id, &binding_ty, stmt);
                            }
                        }

                        // Then recurse for the block.
                        for stmt in block {
                            self.type_check_block_statement(handler, pred_key, stmt)
                        }
                    }

                    if !variants_set.insert(name[2..].to_string()) {
                        // Variant is already used.
                        handler.emit_err(Error::Compile {
                            error: CompileError::MatchBranchReused {
                                name: name.clone(),
                                span: name_span.clone(),
                            },
                        });
                    }
                }

                if let Some(else_branch) = else_branch {
                    for else_block in else_branch {
                        self.type_check_block_statement(handler, pred_key, else_block);
                    }
                }

                let variant_count = variants_set.len();
                if let Some(union_variant_count) = union_ty.get_union_variant_count(&self.unions) {
                    if variant_count < union_variant_count && else_branch.is_none() {
                        // We don't have all variants covered.
                        let mut missing_variants = union_ty.get_union_variant_names(&self.unions);
                        missing_variants.retain(|var_name| !variants_set.contains(var_name));
                        handler.emit_err(Error::Compile {
                            error: CompileError::MatchBranchMissing {
                                union_name: self.with_ctrct(union_ty).to_string(),
                                missing_variants,
                                span: span.clone(),
                            },
                        });
                    }
                    if variant_count == union_variant_count
                        && else_branch.is_some()
                        && !handler.has_errors()
                    {
                        // We have all variants accounted for and a superfluous else.
                        // NOTE: It can get confused so we only emit a warning when there are no
                        // other errors.
                        handler.emit_warn(Warning::MatchUnneededElse { span: span.clone() });
                    }
                }
            }
        }
    }

    fn type_check_match_binding(
        &self,
        handler: &Handler,
        union_ty: &Type,
        variant_name: &Path,
        name_span: &Span,
        binding: &Option<Ident>,
    ) -> Result<Option<Type>, ErrorEmitted> {
        // If the binding exists find the type in the union.
        if let Ok(binding_ty) = union_ty.get_union_variant_ty(&self.unions, variant_name) {
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
                        .get_union_name(&self.unions)
                        .cloned()
                        .unwrap_or("<unknown union>".to_string()),
                    actual_variants: union_ty.get_union_variant_names(&self.unions),
                    span: name_span.clone(),
                },
            });
        }

        // If we get this far then there was an error.
        Err(handler.cancel())
    }

    fn type_check_block_statement(
        &mut self,
        handler: &Handler,
        pred_key: Option<PredKey>,
        block_statement: &BlockStatement,
    ) {
        match block_statement {
            BlockStatement::Constraint(ConstraintDecl { expr, .. }) => {
                let _ = self.type_check_single_expr(handler, pred_key, *expr);
            }

            BlockStatement::If(if_decl) => self.type_check_if_decl(handler, pred_key, if_decl),

            BlockStatement::Match(match_decl) => {
                self.type_check_match_decl(handler, pred_key, match_decl)
            }
        }
    }

    fn type_check_single_expr(
        &mut self,
        handler: &Handler,
        pred_key: Option<PredKey>,
        expr_key: ExprKey,
    ) -> Result<(), ErrorEmitted> {
        // Attempt to infer all the types of each expr.
        let mut queue = Vec::new();

        // Utility to check for recursion in the queue.  A macro to avoid borrowing self.
        macro_rules! push_to_queue {
            ($handler: expr, $dependant_key: ident, $dependency_key: ident) => {
                if queue.contains(&$dependency_key) {
                    Err($handler.emit_err(Error::Compile {
                        error: CompileError::ExprRecursion {
                            dependant_span: self.expr_key_to_span($dependant_key),
                            dependency_span: self.expr_key_to_span($dependency_key),
                        },
                    }))
                } else {
                    queue.push($dependency_key);
                    Ok(())
                }
            };
        }

        // Start with this key.
        queue.push(expr_key);

        // Infer each expr type, or their dependencies.
        while let Some(next_key) = queue.last().cloned() {
            // We may already know this expr type, in which case we can pop it from the queue,
            // or we can infer it.
            if !next_key.get_ty(self).is_unknown() {
                queue.pop();
            } else {
                match self.infer_expr_key_type(
                    handler,
                    pred_key.map(|pred_key| &self.preds[pred_key]),
                    next_key,
                ) {
                    // Successfully inferred its type.  Save it and pop it from the queue.
                    Ok(Inference::Type(ty)) => {
                        next_key.set_ty(ty, self);
                        queue.pop();
                    }

                    // Some dependencies need to be inferred before we can get back to this
                    // expr.  When pushing dependencies we need to check if they're already
                    // queued, in which case we have a recursive dependency and an error.
                    Ok(Inference::Dependant(dep_expr_key)) => {
                        push_to_queue!(handler, next_key, dep_expr_key)?
                    }
                    Ok(Inference::Dependencies(mut dep_expr_keys)) => {
                        dep_expr_keys.drain(..).try_for_each(|dep_expr_key| {
                            push_to_queue!(handler, next_key, dep_expr_key)
                        })?
                    }

                    // Dependencies in `match` expressions may have bindings.  We need to prime
                    // those bindings before queuing the expressions.
                    Ok(Inference::BoundDependencies {
                        mut deps,
                        mut bound_deps,
                    }) => {
                        deps.drain(..).try_for_each(|dep_expr_key| {
                            push_to_queue!(handler, next_key, dep_expr_key)
                        })?;

                        bound_deps
                            .drain(..)
                            .try_for_each(|(id, ty, mut bound_expr_keys)| {
                                // Set the type for the binding for each expr first.
                                bound_expr_keys.drain(..).try_for_each(|bound_expr_key| {
                                    self.set_path_exprs_to_type(&id, &ty, bound_expr_key);

                                    push_to_queue!(handler, next_key, bound_expr_key)
                                })
                            })?;
                    }

                    // Some expressions (e.g., macro calls) just aren't valid any longer and
                    // are best ignored.
                    Ok(Inference::Ignored) => {
                        queue.pop();
                    }

                    // Some exprs may fail to be inferred. In that case we need to mark the
                    // expr_key as failing so that we don't check it again and report the
                    // error more than once.
                    Err(inference_error) => {
                        next_key.set_ty(Type::Error(next_key.get(self).span().clone()), self);
                        return Err(inference_error);
                    }
                }
            }
        }

        Ok(())
    }

    fn infer_expr_key_type(
        &self,
        handler: &Handler,
        pred: Option<&Predicate>,
        expr_key: ExprKey,
    ) -> Result<Inference, ErrorEmitted> {
        let expr: &Expr = expr_key.try_get(self).ok_or_else(|| {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "orphaned expr key when type checking",
                    span: empty_span(),
                },
            })
        })?;

        match expr {
            Expr::Error(span) => Err(handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "unable to type check from error expression",
                    span: span.clone(),
                },
            })),

            Expr::Immediate { value, span } => Ok(self.infer_immediate(handler, value, span)),

            Expr::Array {
                elements,
                range_expr,
                span,
            } => Ok(self.infer_array_expr(handler, *range_expr, elements, span)),

            Expr::Tuple { fields, span } => Ok(self.infer_tuple_expr(fields, span)),

            Expr::UnionVariant {
                path,
                path_span,
                value,
                span,
            } => Ok(self.infer_union_expr(handler, path, path_span, *value, span)),

            Expr::Path(path, span) => Ok(self.infer_path_by_name(handler, pred, path, span)),

            Expr::StorageAccess { name, span, .. } => {
                Ok(self.infer_storage_access(handler, name, span))
            }

            Expr::ExternalStorageAccess {
                interface_instance,
                name,
                span,
                ..
            } => Ok(self.infer_external_storage_access(
                handler,
                pred,
                interface_instance,
                name,
                span,
            )),

            Expr::UnaryOp {
                op,
                expr: op_expr_key,
                span,
            } => Ok(self.infer_unary_op(handler, pred, *op, *op_expr_key, span)),

            Expr::BinaryOp { op, lhs, rhs, span } => {
                Ok(self.infer_binary_op(handler, pred, *op, *lhs, *rhs, span))
            }

            Expr::MacroCall { .. } => Ok(Inference::Ignored),

            Expr::IntrinsicCall { kind, args, span } => {
                self.infer_intrinsic_call_expr(handler, pred, kind, args, span)
            }

            Expr::Select {
                condition,
                then_expr,
                else_expr,
                span,
            } => Ok(self.infer_select_expr(handler, *condition, *then_expr, *else_expr, span)),

            Expr::Match {
                match_expr,
                match_branches,
                else_branch,
                span,
            } => Ok(self.infer_match_expr(handler, *match_expr, match_branches, else_branch, span)),

            Expr::Index { expr, index, span } => {
                Ok(self.infer_index_expr(handler, *expr, *index, span))
            }

            Expr::TupleFieldAccess { tuple, field, span } => {
                Ok(self.infer_tuple_access_expr(handler, *tuple, field, span))
            }

            Expr::Cast { value, ty, span } => Ok(self.infer_cast_expr(handler, *value, ty, span)),

            Expr::In {
                value,
                collection,
                span,
            } => Ok(self.infer_in_expr(handler, *value, *collection, span)),

            Expr::Range { lb, ub, span } => Ok(self.infer_range_expr(handler, *lb, *ub, span)),

            Expr::Generator {
                kind,
                gen_ranges,
                conditions,
                body,
                span,
            } => Ok(self.infer_generator_expr(handler, kind, gen_ranges, conditions, *body, span)),

            Expr::UnionTagIs { .. } | Expr::UnionValue { .. } => {
                Err(handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "union utility expressions should not exist during type checking",
                        span: empty_span(),
                    },
                }))
            }
        }

        // TODO return err if handler is non-empty?
    }

    pub(super) fn infer_immediate(
        &self,
        handler: &Handler,
        imm: &Immediate,
        span: &Span,
    ) -> Inference {
        if let Immediate::Array(el_imms) = imm {
            // Immediate::get_ty() assumes the array is well formed. We need to
            // confirm here.
            if el_imms.is_empty() {
                handler.emit_err(Error::Compile {
                    error: CompileError::EmptyArrayExpression { span: span.clone() },
                });
                return Inference::Type(Type::Error(span.clone()));
            }

            // Get the assumed type.
            let ary_ty = imm.get_ty(Some(span));
            let Type::Array { ty: el0_ty, .. } = &ary_ty else {
                handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "array immediate does NOT have an array type?",
                        span: span.clone(),
                    },
                });
                return Inference::Type(Type::Error(span.clone()));
            };

            let _ = el_imms.iter().try_for_each(|el_imm| {
                let el_ty = el_imm.get_ty(None);
                if !el_ty.eq(&self.new_types, el0_ty.as_ref()) {
                    handler.emit_err(Error::Compile {
                        error: CompileError::NonHomogeneousArrayElement {
                            expected_ty: self.with_ctrct(el0_ty.as_ref()).to_string(),
                            ty: self.with_ctrct(el_ty).to_string(),
                            span: span.clone(),
                        },
                    });
                    Err(())
                } else {
                    Ok(())
                }
            });

            Inference::Type(ary_ty)
        } else {
            Inference::Type(imm.get_ty(Some(span)))
        }
    }

    fn infer_path_by_key(
        &self,
        handler: &Handler,
        pred: &Predicate,
        var_key: VarKey,
        span: &Span,
    ) -> Inference {
        let ty = var_key.get_ty(pred);
        if !ty.is_unknown() {
            Inference::Type(ty.clone())
        } else if let Some(init_expr_key) = pred.var_inits.get(var_key) {
            let init_expr_ty = init_expr_key.get_ty(self);
            if !init_expr_ty.is_unknown() {
                Inference::Type(init_expr_ty.clone())
            } else {
                // We have a variable with an initialiser but don't know the initialiser type
                // yet.
                Inference::Dependant(*init_expr_key)
            }
        } else {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "untyped variable doesn't have initialiser",
                    span: span.clone(),
                },
            });
            Inference::Type(Type::Error(span.clone()))
        }
    }

    fn infer_path_by_name(
        &self,
        handler: &Handler,
        pred: Option<&Predicate>,
        path: &Path,
        span: &Span,
    ) -> Inference {
        // If we're searching for an enum variant and it appears to be unqualified then we can
        // report some hints.
        let mut hints = Vec::new();

        if let Some(Const { decl_ty, .. }) = self.consts.get(path) {
            if !decl_ty.is_unknown() {
                Inference::Type(decl_ty.clone())
            } else {
                handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "const decl has unknown type *after* evaluation",
                        span: span.clone(),
                    },
                });
                Inference::Type(Type::Error(span.clone()))
            }
        } else if let Some(ty) = self
            .new_types
            .iter()
            .find_map(|NewTypeDecl { name, ty, .. }| (&name.name == path).then_some(ty))
        {
            // It's a fully matched newtype.
            Inference::Type(ty.clone())
        } else {
            // It might be an enum or union variant.  If it isn't we get a handy list of potential
            // variant names we can return in our error.
            let enum_res = self.infer_variant_by_name(handler, path, span);
            if let Ok(inference) = enum_res {
                // Need to translate the type between Results.
                inference
            } else {
                // Save the enums variants list for the SymbolNotFound error if we need it.
                if let Ok(enums_list) = enum_res.unwrap_err() {
                    hints.extend(enums_list);
                }

                // For all other paths we need a predicate.
                if let Some(pred) = pred {
                    if let Some(var_key) = pred
                        .vars()
                        .find_map(|(var_key, var)| (&var.name == path).then_some(var_key))
                    {
                        // It's a var.
                        self.infer_path_by_key(handler, pred, var_key, span)
                    } else if let Some((state_key, state)) =
                        pred.states().find(|(_, state)| (&state.name == path))
                    {
                        // It's state.
                        let state_expr_ty = state.expr.get_ty(self);
                        let state_type = state_key.get_ty(pred);
                        if !state_type.is_unknown() {
                            Inference::Type(state_type.clone())
                        } else if !state_expr_ty.is_unknown() {
                            Inference::Type(state_expr_ty.clone())
                        } else {
                            Inference::Dependant(state.expr)
                        }
                    } else if let Some(EphemeralDecl { ty, .. }) = pred
                        .ephemerals
                        .iter()
                        .find(|eph_decl| &eph_decl.name == path)
                    {
                        // It's an ephemeral value.
                        Inference::Type(ty.clone())
                    } else if let Some(ty) = self.infer_extern_var(pred, path) {
                        // It's an external var
                        ty
                    } else {
                        // None of the above.
                        handler.emit_err(Error::Compile {
                            error: CompileError::SymbolNotFound {
                                name: path.clone(),
                                span: span.clone(),
                                enum_names: hints,
                            },
                        });
                        Inference::Type(Type::Error(span.clone()))
                    }
                } else {
                    handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "attempting to infer item without required predicate ref",
                            span: span.clone(),
                        },
                    });
                    Inference::Type(Type::Error(span.clone()))
                }
            }
        }
    }

    // This has a wacky return type of Result<_, Result<_, _>>.  It's because it wants to return 3
    // different outcomes - an inference, a type error or some hints to use in a different error.
    // An alternative might be to use Result<Result<_, _>, _> or probably better would be
    // Result<Either<_, _>, _>.
    pub(super) fn infer_variant_by_name(
        &self,
        handler: &Handler,
        path: &Path,
        span: &Span,
    ) -> Result<Inference, Result<Vec<String>, ErrorEmitted>> {
        // Check first if the path prefix matches a new type.
        for NewTypeDecl { name, ty, .. } in &self.new_types {
            if let Type::Custom {
                path: enum_or_union_path,
                ..
            } = ty
            {
                // This new type is to an enum or union.  Does the new type path match the passed
                // path?
                if path.starts_with(&name.name) {
                    // Yep, we might have an enum or union wrapped in a new type.
                    let new_type_len = name.name.len();
                    if path.chars().nth(new_type_len) == Some(':') {
                        // Definitely worth trying.  Recurse.
                        let new_path = enum_or_union_path.clone() + &path[new_type_len..];
                        if let ty @ Ok(_) = self.infer_variant_by_name(handler, &new_path, span) {
                            // We found an enum or union variant.
                            return ty;
                        }
                    }
                }
            }
        }

        match self.infer_enum_variant_by_name(path) {
            // No dice.  Try unions, passing the hints along.
            Err(hints) => self.infer_union_variant_by_name(handler, path, span, hints),

            // Wrap the hints in a Result.
            inference => inference.map_err(Ok),
        }
    }

    pub(super) fn infer_enum_variant_by_name(&self, path: &Path) -> Result<Inference, Vec<String>> {
        let mut hints = Vec::default();

        // Find a match in the enums.
        self.enums
            .iter()
            .find_map(
                |EnumDecl {
                     name: enum_name,
                     variants,
                     span,
                 }| {
                    (&enum_name.name == path
                        || variants.iter().any(|variant| {
                            Self::variant_name_matches(path, enum_name, &variant.name, &mut hints)
                        }))
                    .then(|| {
                        Inference::Type(Type::Custom {
                            path: enum_name.name.clone(),
                            span: span.clone(),
                        })
                    })
                },
            )
            .ok_or(hints)
    }

    // This has the same tricky Result<_, Result<_, _>> return type -- see infer_variant_by_name()
    // above.
    fn infer_union_variant_by_name(
        &self,
        handler: &Handler,
        path: &Path,
        path_span: &Span,
        mut hints: Vec<String>,
    ) -> Result<Inference, Result<Vec<String>, ErrorEmitted>> {
        // Try to find a match in the unions.
        let variant_match: Option<Result<Inference, ErrorEmitted>> = self.unions.iter().find_map(
            |UnionDecl {
                 name: union_name,
                 variants,
                 span,
             }| {
                if &union_name.name == path {
                    Some(Ok(Inference::Type(Type::Union {
                        path: union_name.name.clone(),
                        span: span.clone(),
                    })))
                } else {
                    // Return None if not found or Some(Result<..>) depending on if it's valid.
                    variants.iter().find_map(|variant| {
                        Self::variant_name_matches(
                            path,
                            union_name,
                            &variant.variant_name.name,
                            &mut hints,
                        )
                        .then(|| {
                            // A variant was found.  Was it supposed to have a value?  (To get to
                            // this point we have received only a Path with no value.)
                            if variant.ty.is_some() {
                                // This variant *does* require a value.
                                Err(handler.emit_err(Error::Compile {
                                    error: CompileError::MissingUnionExprValue {
                                        name: variant.variant_name.name.to_string(),
                                        variant_ty: self
                                            .with_ctrct(variant.ty.as_ref().unwrap())
                                            .to_string(),
                                        span: path_span.clone(),
                                    },
                                }))
                            } else {
                                Ok(Inference::Type(Type::Union {
                                    path: union_name.name.clone(),
                                    span: span.clone(),
                                }))
                            }
                        })
                    })
                }
            },
        );

        // Translate the result from an Option to Result.
        variant_match
            .map(|res| res.map_err(Err))
            .unwrap_or(Err(Ok(hints)))
    }

    fn variant_name_matches(
        path: &Path,
        type_name: &Ident,
        variant_name: &str,
        hints: &mut Vec<String>,
    ) -> bool {
        if path.len() > 2 && &path[2..] == variant_name {
            hints.push(type_name.name.clone());
        }

        let mut full_variant = type_name.name.clone();
        full_variant.push_str("::");
        full_variant.push_str(variant_name);

        &full_variant == path
    }

    fn infer_storage_access(&self, handler: &Handler, name: &String, span: &Span) -> Inference {
        match self.storage.as_ref() {
            Some(storage) => match storage.0.iter().find(|s_var| s_var.name.name == *name) {
                Some(s_var) => Inference::Type(s_var.ty.clone()),
                None => {
                    handler.emit_err(Error::Compile {
                        error: CompileError::StorageSymbolNotFound {
                            name: name.clone(),
                            span: span.clone(),
                        },
                    });
                    Inference::Type(Type::Error(span.clone()))
                }
            },
            None => {
                handler.emit_err(Error::Compile {
                    error: CompileError::MissingStorageBlock {
                        name: name.clone(),
                        span: span.clone(),
                    },
                });
                Inference::Type(Type::Error(span.clone()))
            }
        }
    }

    fn infer_external_storage_access(
        &self,
        handler: &Handler,
        pred: Option<&Predicate>,
        interface_instance: &Path,
        name: &String,
        span: &Span,
    ) -> Inference {
        if let Some(pred) = pred {
            // Find the interface instance or emit an error
            let Some(interface_instance) = pred
                .interface_instances
                .iter()
                .find(|e| e.name.to_string() == *interface_instance)
            else {
                handler.emit_err(Error::Compile {
                    error: CompileError::MissingInterfaceInstance {
                        name: interface_instance.clone(),
                        span: span.clone(),
                    },
                });
                return Inference::Type(Type::Error(empty_span()));
            };

            // Find the interface declaration corresponding to the interface instance
            let Some(interface) = self
                .interfaces
                .iter()
                .find(|e| e.name.to_string() == *interface_instance.interface)
            else {
                // No need to emit an error here because a `MissingInterface` error should have
                // already been emitted earlier when all interface instances were type checked.
                // Instead, we just return an `Unknown` type knowing that the compilation will fail
                // anyways.
                return Inference::Type(Type::Unknown(empty_span()));
            };

            // Then, look for the storage variable that this access refers to
            match interface.storage.as_ref() {
                Some(storage) => match storage.0.iter().find(|s_var| s_var.name.name == *name) {
                    Some(s_var) => Inference::Type(s_var.ty.clone()),
                    None => {
                        handler.emit_err(Error::Compile {
                            error: CompileError::StorageSymbolNotFound {
                                name: name.clone(),
                                span: span.clone(),
                            },
                        });
                        Inference::Type(Type::Error(empty_span()))
                    }
                },
                None => {
                    handler.emit_err(Error::Compile {
                        error: CompileError::MissingStorageBlock {
                            name: name.clone(),
                            span: span.clone(),
                        },
                    });
                    Inference::Type(Type::Error(empty_span()))
                }
            }
        } else {
            handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "attempting to infer item without required predicate ref",
                    span: span.clone(),
                },
            });
            Inference::Type(Type::Error(empty_span()))
        }
    }

    fn infer_extern_var(&self, pred: &Predicate, path: &Path) -> Option<Inference> {
        // Look through all available predicate instances and their corresponding interfaces for a
        // var with the same path as `path`
        for PredicateInstance {
            name,
            interface_instance,
            predicate,
            ..
        } in &pred.predicate_instances
        {
            if let Some(interface_instance) = interface_instance {
                if let Some(interface_instance) = pred
                    .interface_instances
                    .iter()
                    .find(|e| e.name.to_string() == *interface_instance)
                {
                    if let Some(interface) = self
                        .interfaces
                        .iter()
                        .find(|e| e.name.to_string() == *interface_instance.interface)
                    {
                        if let Some(predicate) = interface
                            .predicate_interfaces
                            .iter()
                            .find(|e| e.name.to_string() == *predicate.to_string())
                        {
                            for var in &predicate.vars {
                                if name.to_string() + "::" + &var.name.name == *path {
                                    return Some(Inference::Type(var.ty.clone()));
                                }
                            }
                        }
                    }
                }
            } else {
                // Search for a local predicate that matches the full name of `predicate`. Also,
                // `predicate` must not reference `pred`.
                let full_predicate_name = "::".to_owned() + &predicate.name;
                if full_predicate_name != pred.name {
                    if let Some((_, predicate)) = self
                        .preds
                        .iter()
                        .find(|(_, e)| e.name == full_predicate_name)
                    {
                        for (var_key, var) in predicate.vars() {
                            if var.is_pub && name.to_string() + &var.name == *path {
                                return Some(Inference::Type(var_key.get_ty(predicate).clone()));
                            }
                        }
                    }
                }
            }
        }

        // No extern var that matches `path`.
        None
    }

    fn infer_unary_op(
        &self,
        handler: &Handler,
        pred: Option<&Predicate>,
        op: UnaryOp,
        rhs_expr_key: ExprKey,
        span: &Span,
    ) -> Inference {
        fn drill_down_to_path(
            contract: &Contract,
            pred: Option<&Predicate>,
            expr_key: &ExprKey,
            span: &Span,
        ) -> Result<(), Error> {
            match expr_key.try_get(contract) {
                Some(Expr::Path(name, span)) => {
                    if pred
                        .map(|pred| pred.states().any(|(_, state)| state.name == *name))
                        .unwrap_or(false)
                    {
                        Ok(())
                    } else {
                        Err(Error::Compile {
                            error: CompileError::InvalidNextStateAccess { span: span.clone() },
                        })
                    }
                }

                // We recurse in three cases:
                // - a'' is equivalent to a'.
                // - a[x]' is equivalent to a'[x].
                // - a.0' is equivalent to a'.0.
                Some(Expr::UnaryOp {
                    op: UnaryOp::NextState,
                    expr,
                    ..
                })
                | Some(Expr::Index { expr, .. })
                | Some(Expr::TupleFieldAccess { tuple: expr, .. }) => {
                    drill_down_to_path(contract, pred, expr, span)
                }

                _ => Err(Error::Compile {
                    error: CompileError::InvalidNextStateAccess { span: span.clone() },
                }),
            }
        }

        match op {
            UnaryOp::Error => {
                handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "unable to type check unary op error",
                        span: span.clone(),
                    },
                });
                Inference::Type(Type::Error(span.clone()))
            }

            UnaryOp::NextState => {
                // Next state access must be a path that resolves to a state variable.  It _may_ be
                // via array indices or tuple fields or even other prime ops.
                match drill_down_to_path(self, pred, &rhs_expr_key, span) {
                    Ok(()) => {
                        let ty = rhs_expr_key.get_ty(self);
                        if !ty.is_unknown() {
                            Inference::Type(ty.clone())
                        } else {
                            Inference::Dependant(rhs_expr_key)
                        }
                    }
                    Err(err) => {
                        handler.emit_err(err);
                        Inference::Type(Type::Error(span.clone()))
                    }
                }
            }

            UnaryOp::Neg => {
                // RHS must be an int or real.
                let ty = rhs_expr_key.get_ty(self);
                if !ty.is_unknown() {
                    if !ty.is_num() && !ty.is_error() {
                        handler.emit_err(Error::Compile {
                            error: CompileError::OperatorTypeError {
                                arity: "unary",
                                large_err: Box::new(LargeTypeError::OperatorTypeError {
                                    op: "-",
                                    expected_ty: "numeric".to_string(),
                                    found_ty: self.with_ctrct(ty).to_string(),
                                    span: span.clone(),
                                    expected_span: None,
                                }),
                            },
                        });
                    }

                    Inference::Type(ty.clone())
                } else {
                    Inference::Dependant(rhs_expr_key)
                }
            }

            UnaryOp::Not => {
                // RHS must be a bool.
                let ty = rhs_expr_key.get_ty(self);
                if !ty.is_unknown() {
                    if !ty.is_bool() && !ty.is_error() {
                        handler.emit_err(Error::Compile {
                            error: CompileError::OperatorTypeError {
                                arity: "unary",
                                large_err: Box::new(LargeTypeError::OperatorTypeError {
                                    op: "!",
                                    expected_ty: "bool".to_string(),
                                    found_ty: self.with_ctrct(ty).to_string(),
                                    span: span.clone(),
                                    expected_span: None,
                                }),
                            },
                        });
                    }

                    Inference::Type(ty.clone())
                } else {
                    Inference::Dependant(rhs_expr_key)
                }
            }
        }
    }

    fn infer_binary_op(
        &self,
        handler: &Handler,
        pred: Option<&Predicate>,
        op: BinaryOp,
        lhs_expr_key: ExprKey,
        rhs_expr_key: ExprKey,
        span: &Span,
    ) -> Inference {
        let check_numeric_args = |lhs_ty: &Type, rhs_ty: &Type, ty_str: &str| {
            if !lhs_ty.is_num() {
                if !lhs_ty.is_error() {
                    handler.emit_err(Error::Compile {
                        error: CompileError::OperatorTypeError {
                            arity: "binary",
                            large_err: Box::new(LargeTypeError::OperatorTypeError {
                                op: op.as_str(),
                                expected_ty: ty_str.to_string(),
                                found_ty: self.with_ctrct(lhs_ty).to_string(),
                                span: self.expr_key_to_span(lhs_expr_key),
                                expected_span: None,
                            }),
                        },
                    });
                }
            } else if !rhs_ty.is_num() {
                if !rhs_ty.is_error() {
                    handler.emit_err(Error::Compile {
                        error: CompileError::OperatorTypeError {
                            arity: "binary",
                            large_err: Box::new(LargeTypeError::OperatorTypeError {
                                op: op.as_str(),
                                expected_ty: ty_str.to_string(),
                                found_ty: self.with_ctrct(rhs_ty).to_string(),
                                span: self.expr_key_to_span(rhs_expr_key),
                                expected_span: None,
                            }),
                        },
                    });
                }
            } else if !lhs_ty.eq(&self.new_types, rhs_ty) {
                // Here we assume the LHS is the 'correct' type.
                handler.emit_err(Error::Compile {
                    error: CompileError::OperatorTypeError {
                        arity: "binary",
                        large_err: Box::new(LargeTypeError::OperatorTypeError {
                            op: op.as_str(),
                            expected_ty: self.with_ctrct(lhs_ty).to_string(),
                            found_ty: self.with_ctrct(rhs_ty).to_string(),
                            span: self.expr_key_to_span(rhs_expr_key),
                            expected_span: Some(self.expr_key_to_span(lhs_expr_key)),
                        }),
                    },
                });
            }
        };

        // Checks if a given `ExprKey` is a path to a state var or a "next state" expression. If
        // not, emit an error.
        let check_state_var_arg = |arg: ExprKey| match arg.try_get(self) {
            Some(Expr::Path(name, _))
                if pred
                    .map(|pred| pred.states().any(|(_, state)| state.name == *name))
                    .unwrap_or(false) => {}
            Some(Expr::UnaryOp {
                op: UnaryOp::NextState,
                ..
            }) => {}
            _ => {
                handler.emit_err(Error::Compile {
                    error: CompileError::CompareToNilError {
                        op: op.as_str(),
                        span: self.expr_key_to_span(arg),
                    },
                });
            }
        };

        let lhs_ty = lhs_expr_key.get_ty(self).clone();
        let rhs_ty = rhs_expr_key.get_ty(self);
        if !lhs_ty.is_unknown() {
            if !rhs_ty.is_unknown() {
                match op {
                    BinaryOp::Add
                    | BinaryOp::Sub
                    | BinaryOp::Mul
                    | BinaryOp::Div
                    | BinaryOp::Mod => {
                        // Both args must be numeric, i.e., ints or reals; binary op type is same
                        // as arg types.
                        check_numeric_args(&lhs_ty, rhs_ty, "numeric");
                        Inference::Type(lhs_ty)
                    }
                    BinaryOp::Equal | BinaryOp::NotEqual => {
                        // If either operands is `nil`, then ensure that the other is a state var
                        // or a "next state" expression
                        if lhs_ty.is_nil() && !rhs_ty.is_nil() {
                            check_state_var_arg(rhs_expr_key);
                        } else if !lhs_ty.is_nil() && rhs_ty.is_nil() {
                            check_state_var_arg(lhs_expr_key);
                        }

                        // We can special case implicit constraints which are injected by variable
                        // initialiser handling.  Each `var a = b` gets a magic `constraint a == b`
                        // which we check for type mismatches elsewhere, and emit a much better
                        // error then.
                        let mut is_init_constraint = false;
                        if !lhs_ty.eq(&self.new_types, rhs_ty)
                            && op == BinaryOp::Equal
                            && pred
                                .map(|pred| {
                                    pred.var_inits
                                        .values()
                                        .any(|init_key| *init_key == rhs_expr_key)
                                })
                                .unwrap_or(false)
                        {
                            is_init_constraint = true;
                        }

                        // Both args must be equatable, which at this stage is any type; binary op
                        // type is bool.
                        if !lhs_ty.eq(&self.new_types, rhs_ty)
                            && !lhs_ty.is_nil()
                            && !rhs_ty.is_nil()
                            && !is_init_constraint
                            && !lhs_ty.is_error()
                            && !rhs_ty.is_error()
                        {
                            handler.emit_err(Error::Compile {
                                error: CompileError::OperatorTypeError {
                                    arity: "binary",
                                    large_err: Box::new(LargeTypeError::OperatorTypeError {
                                        op: op.as_str(),
                                        expected_ty: self.with_ctrct(lhs_ty).to_string(),
                                        found_ty: self.with_ctrct(rhs_ty).to_string(),
                                        span: self.expr_key_to_span(rhs_expr_key),
                                        expected_span: Some(self.expr_key_to_span(lhs_expr_key)),
                                    }),
                                },
                            });
                        }

                        Inference::Type(Type::Primitive {
                            kind: PrimitiveKind::Bool,
                            span: span.clone(),
                        })
                    }

                    BinaryOp::LessThanOrEqual
                    | BinaryOp::LessThan
                    | BinaryOp::GreaterThanOrEqual
                    | BinaryOp::GreaterThan => {
                        // Both args must be ordinal, i.e., ints, reals; binary op type is bool.
                        check_numeric_args(&lhs_ty, rhs_ty, "numeric");
                        Inference::Type(Type::Primitive {
                            kind: PrimitiveKind::Bool,
                            span: span.clone(),
                        })
                    }

                    BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                        // Both arg types and binary op type are all bool.
                        if !lhs_ty.is_bool() {
                            if !lhs_ty.is_error() {
                                handler.emit_err(Error::Compile {
                                    error: CompileError::OperatorTypeError {
                                        arity: "binary",
                                        large_err: Box::new(LargeTypeError::OperatorTypeError {
                                            op: op.as_str(),
                                            expected_ty: "bool".to_string(),
                                            found_ty: self.with_ctrct(lhs_ty).to_string(),
                                            span: self.expr_key_to_span(lhs_expr_key),
                                            expected_span: Some(span.clone()),
                                        }),
                                    },
                                });
                            }
                        } else if !rhs_ty.is_bool() && !rhs_ty.is_error() {
                            handler.emit_err(Error::Compile {
                                error: CompileError::OperatorTypeError {
                                    arity: "binary",
                                    large_err: Box::new(LargeTypeError::OperatorTypeError {
                                        op: op.as_str(),
                                        expected_ty: "bool".to_string(),
                                        found_ty: self.with_ctrct(rhs_ty).to_string(),
                                        span: self.expr_key_to_span(rhs_expr_key),
                                        expected_span: Some(span.clone()),
                                    }),
                                },
                            });
                        }

                        Inference::Type(Type::Primitive {
                            kind: PrimitiveKind::Bool,
                            span: span.clone(),
                        })
                    }
                }
            } else {
                Inference::Dependant(rhs_expr_key)
            }
        } else {
            Inference::Dependant(lhs_expr_key)
        }
    }

    pub(super) fn infer_intrinsic_call_expr(
        &self,
        handler: &Handler,
        pred: Option<&Predicate>,
        (kind, name_span): &(IntrinsicKind, Span),
        args: &[ExprKey],
        span: &Span,
    ) -> Result<Inference, ErrorEmitted> {
        let mut deps = Vec::new();

        args.iter()
            .filter(|arg_key| arg_key.get_ty(self).is_unknown())
            .for_each(|arg_key| deps.push(*arg_key));

        if deps.is_empty() {
            let expected_args = kind.args();
            // Check the type of each argument. The type of arguments must match what
            // `kind.args()` produces.
            for (expected, arg) in expected_args.iter().zip(args.iter()) {
                let found = arg.get_ty(self);
                if !expected.eq(&self.new_types, found) {
                    handler.emit_err(Error::Compile {
                        error: CompileError::MismatchedIntrinsicArgType {
                            expected: format!("{}", self.with_ctrct(expected)),
                            found: format!("{}", self.with_ctrct(found)),
                            intrinsic_span: name_span.clone(),
                            arg_span: arg.get(self).span().clone(),
                        },
                    });
                }
            }

            // Also, ensure that the number of arguments is correct
            if args.len() != expected_args.len() {
                handler.emit_err(Error::Compile {
                    error: CompileError::UnexpectedIntrinsicArgCount {
                        expected: kind.args().len(),
                        found: args.len(),
                        span: span.clone(),
                    },
                });
            }

            // Some intrinsic needs additional semantic checks
            if let IntrinsicKind::External(ExternalIntrinsic::AddressOf) = kind {
                if let Some(arg) = args.first() {
                    if let Some(Expr::Immediate {
                        value: Immediate::String(name),
                        ..
                    }) = arg.try_get(self)
                    {
                        // Ensure that we're not referring to the same predicate that the intrinsic
                        // is used in
                        if let Some(pred) = pred {
                            if pred.name == *name {
                                handler.emit_err(Error::Compile {
                                    error: CompileError::AddressOfSelf {
                                        name: name.to_string(),
                                        span: arg.get(self).span().clone(),
                                    },
                                });
                            }
                        }

                        // Ensure that the intrinsic refers to a predicate in the same contract
                        if self.preds.iter().all(|(_, pred)| pred.name != *name) {
                            handler.emit_err(Error::Compile {
                                error: CompileError::PredicateNameNotFound {
                                    name: name.to_string(),
                                    span: arg.get(self).span().clone(),
                                },
                            });
                        }
                    }
                }
            }

            Ok(Inference::Type(kind.ty()))
        } else {
            Ok(Inference::Dependencies(deps))
        }
    }

    fn infer_select_expr(
        &self,
        handler: &Handler,
        cond_expr_key: ExprKey,
        then_expr_key: ExprKey,
        else_expr_key: ExprKey,
        span: &Span,
    ) -> Inference {
        let cond_ty = cond_expr_key.get_ty(self);
        let then_ty = then_expr_key.get_ty(self);
        let else_ty = else_expr_key.get_ty(self);
        if !cond_ty.is_unknown() {
            if !cond_ty.is_bool() {
                handler.emit_err(Error::Compile {
                    error: CompileError::NonBoolConditional {
                        ty: self.with_ctrct(cond_ty).to_string(),
                        conditional: "select expression".to_string(),
                        span: self.expr_key_to_span(cond_expr_key),
                    },
                });
                Inference::Type(Type::Error(span.clone()))
            } else if !then_ty.is_unknown() {
                if !else_ty.is_unknown() {
                    if !then_ty.eq(&self.new_types, else_ty) {
                        handler.emit_err(Error::Compile {
                            error: CompileError::SelectBranchesTypeMismatch {
                                large_err: Box::new(LargeTypeError::SelectBranchesTypeMismatch {
                                    then_type: self.with_ctrct(then_ty).to_string(),
                                    then_span: self.expr_key_to_span(then_expr_key),
                                    else_type: self.with_ctrct(else_ty).to_string(),
                                    else_span: self.expr_key_to_span(else_expr_key),
                                    span: span.clone(),
                                }),
                            },
                        });
                    }
                    Inference::Type(then_ty.clone())
                } else {
                    Inference::Dependant(else_expr_key)
                }
            } else {
                Inference::Dependant(then_expr_key)
            }
        } else {
            Inference::Dependant(cond_expr_key)
        }
    }

    fn infer_match_expr(
        &self,
        handler: &Handler,
        match_expr_key: ExprKey,
        match_branches: &[MatchBranch],
        else_branch: &Option<MatchElse>,
        span: &Span,
    ) -> Inference {
        let union_ty = match_expr_key.get_ty(self);
        if union_ty.is_unknown() {
            Inference::Dependant(match_expr_key)
        } else if !union_ty.is_union(&self.unions) {
            handler.emit_err(Error::Compile {
                error: CompileError::MatchExprNotUnion {
                    found_ty: self.with_ctrct(union_ty).to_string(),
                    span: self.expr_key_to_span(match_expr_key),
                },
            });

            Inference::Type(Type::Error(span.clone()))
        } else {
            let mut deps = Vec::default();
            let mut bound_deps = Vec::default();
            let mut branch_tys = Vec::default();

            let mut type_check_branch_exprs =
                |constraints: &[ExprKey], expr: ExprKey| -> Vec<ExprKey> {
                    let mut branch_deps = Vec::default();
                    for constr_key in constraints {
                        let constr_ty = constr_key.get_ty(self);
                        if constr_ty.is_unknown() {
                            branch_deps.push(*constr_key);
                        } else if !constr_ty.is_bool() {
                            handler.emit_err(Error::Compile {
                                error: CompileError::ConstraintExpressionTypeError {
                                    large_err: Box::new(
                                        LargeTypeError::ConstraintExpressionTypeError {
                                            expected_ty: self
                                                .with_ctrct(Type::Primitive {
                                                    kind: PrimitiveKind::Bool,
                                                    span: empty_span(),
                                                })
                                                .to_string(),
                                            found_ty: self.with_ctrct(constr_ty).to_string(),
                                            span: self.expr_key_to_span(*constr_key),
                                            expected_span: None,
                                        },
                                    ),
                                },
                            });
                        }
                    }

                    let branch_ty = expr.get_ty(self);

                    if branch_ty.is_unknown() {
                        branch_deps.push(expr);
                    } else {
                        branch_tys.push((expr, branch_ty.clone()));
                    }

                    branch_deps
                };

            let branches_check = handler.scope(|handler| {
                for MatchBranch {
                    name,
                    name_span,
                    binding,
                    constraints,
                    expr,
                } in match_branches
                {
                    if let Ok(binding_ty) =
                        self.type_check_match_binding(handler, union_ty, name, name_span, binding)
                    {
                        let mut branch_deps = type_check_branch_exprs(constraints, *expr);

                        if !branch_deps.is_empty() {
                            if let (Some(binding_ty), Some(binding)) = (binding_ty, binding) {
                                // Variant has a binding; return bound deps.
                                bound_deps.push((binding.clone(), binding_ty.clone(), branch_deps));
                            } else {
                                // Variant has no binding; return regular deps.
                                deps.append(&mut branch_deps);
                            }
                        }
                    }
                }

                if let Some(MatchElse { constraints, expr }) = else_branch {
                    let mut branch_deps = type_check_branch_exprs(constraints, *expr);
                    deps.append(&mut branch_deps);
                }

                let mut variants_set = FxHashSet::default();
                for MatchBranch {
                    name, name_span, ..
                } in match_branches
                {
                    if !variants_set.insert(name[2..].to_string()) {
                        // Variant is already used.
                        handler.emit_err(Error::Compile {
                            error: CompileError::MatchBranchReused {
                                name: name.clone(),
                                span: name_span.clone(),
                            },
                        });
                    }
                }

                let variant_count = variants_set.len();
                if let Some(union_variant_count) = union_ty.get_union_variant_count(&self.unions) {
                    if variant_count < union_variant_count && else_branch.is_none() {
                        // We don't have all variants covered.
                        let mut missing_variants = union_ty.get_union_variant_names(&self.unions);
                        missing_variants.retain(|var_name| !variants_set.contains(var_name));
                        handler.emit_err(Error::Compile {
                            error: CompileError::MatchBranchMissing {
                                union_name: self.with_ctrct(union_ty).to_string(),
                                missing_variants,
                                span: span.clone(),
                            },
                        });
                    }
                    if variant_count == union_variant_count && else_branch.is_some() {
                        // We have all variants accounted for and a superfluous else.
                        // NOTE: To avoid putting this warning in multiple times it's only done
                        // when there are no pending dependencies (i.e., the final time this
                        // method is called).
                        // ALSO: It can get confused, so we only emit the warning if there are
                        // no other errors.
                        if deps.is_empty() && bound_deps.is_empty() && !handler.has_errors() {
                            handler.emit_warn(Warning::MatchUnneededElse { span: span.clone() });
                        }
                    }
                }

                Ok(())
            });

            if branches_check.is_err() {
                Inference::Type(Type::Error(span.clone()))
            } else if branch_tys.len() >= match_branches.len() {
                // We were able to get types for each branch; now confirm they're consistent.
                // NOTE: `branch_tys` might be longer if we have a superfluous `else`.  In that
                // case a warning is emitted but not an error which means we could still land here.
                let (_, match_ty) = &branch_tys[0];

                for (branch_expr, branch_ty) in &branch_tys[1..] {
                    if !branch_ty.eq(&self.new_types, match_ty) {
                        handler.emit_err(Error::Compile {
                            error: CompileError::MatchBranchTypeMismatch {
                                expected_ty: self.with_ctrct(match_ty).to_string(),
                                found_ty: self.with_ctrct(branch_ty).to_string(),
                                span: self.expr_key_to_span(*branch_expr),
                            },
                        });
                    }
                }

                Inference::Type(match_ty.clone())
            } else if bound_deps.is_empty() && !deps.is_empty() {
                Inference::Dependencies(deps)
            } else if !bound_deps.is_empty() {
                Inference::BoundDependencies { deps, bound_deps }
            } else {
                Inference::Type(Type::Error(span.clone()))
            }
        }
    }

    fn infer_range_expr(
        &self,
        handler: &Handler,
        lower_bound_key: ExprKey,
        upper_bound_key: ExprKey,
        span: &Span,
    ) -> Inference {
        let lb_ty = lower_bound_key.get_ty(self);
        let ub_ty = upper_bound_key.get_ty(self);
        if !lb_ty.is_unknown() {
            if !ub_ty.is_unknown() {
                if !lb_ty.eq(&self.new_types, ub_ty) {
                    handler.emit_err(Error::Compile {
                        error: CompileError::RangeTypesMismatch {
                            lb_ty: self.with_ctrct(lb_ty).to_string(),
                            ub_ty: self.with_ctrct(ub_ty).to_string(),
                            span: ub_ty.span().clone(),
                        },
                    });
                } else if !lb_ty.is_num() {
                    handler.emit_err(Error::Compile {
                        error: CompileError::RangeTypesNonNumeric {
                            ty: self.with_ctrct(lb_ty).to_string(),
                            span: span.clone(),
                        },
                    });
                }
                Inference::Type(lb_ty.clone())
            } else {
                Inference::Dependant(upper_bound_key)
            }
        } else {
            Inference::Dependant(lower_bound_key)
        }
    }

    fn infer_cast_expr(
        &self,
        handler: &Handler,
        value_key: ExprKey,
        to_ty: &Type,
        span: &Span,
    ) -> Inference {
        // FROM  TO    ACTION
        // int   int   No-op
        // int   real  Produce the closest possible real
        // real  real  No-op
        // enum  int   Enum cast (performed in lower_enums())
        // bool  int   Boolean to integer cast (performed in lower_bools())

        let from_ty = value_key.get_ty(self);
        if !from_ty.is_unknown() {
            if !to_ty.is_int() && !to_ty.is_real() {
                // We can only cast to ints or reals.
                handler.emit_err(Error::Compile {
                    error: CompileError::BadCastTo {
                        ty: self.with_ctrct(to_ty).to_string(),
                        span: span.clone(),
                    },
                });
            } else if (to_ty.is_int()
                && !from_ty.is_int()
                && !from_ty.is_enum(&self.enums)
                && !from_ty.is_bool())
                || (to_ty.is_real() && !from_ty.is_int() && !from_ty.is_real())
            {
                // We can only cast to ints from ints, enums or bools and to reals from ints or reals.
                handler.emit_err(Error::Compile {
                    error: CompileError::BadCastFrom {
                        ty: self.with_ctrct(from_ty).to_string(),
                        span: span.clone(),
                    },
                });
            }
            Inference::Type(to_ty.clone())
        } else {
            Inference::Dependant(value_key)
        }
    }

    fn infer_in_expr(
        &self,
        handler: &Handler,
        value_key: ExprKey,
        collection_key: ExprKey,
        span: &Span,
    ) -> Inference {
        // If the collection is a range, then it must be between ints or reals and the value must
        // match.  If it's an array it can be any type but still the value must match the array
        // element type.
        let value_ty = value_key.get_ty(self);
        let collection_ty = collection_key.get_ty(self);
        if !value_ty.is_unknown() {
            if !collection_ty.is_unknown() {
                if collection_ty.is_num() {
                    // range - has to match range type too
                    if !value_ty.eq(&self.new_types, collection_ty) {
                        handler.emit_err(Error::Compile {
                            error: CompileError::InExprTypesMismatch {
                                val_ty: self.with_ctrct(value_ty).to_string(),
                                range_ty: self.with_ctrct(collection_ty).to_string(),
                                span: collection_ty.span().clone(),
                            },
                        });
                    }

                    Inference::Type(Type::Primitive {
                        kind: PrimitiveKind::Bool,
                        span: span.clone(),
                    })
                } else if let Some(el_ty) = collection_ty.get_array_el_type() {
                    if !value_ty.eq(&self.new_types, el_ty) {
                        handler.emit_err(Error::Compile {
                            error: CompileError::InExprTypesArrayMismatch {
                                val_ty: self.with_ctrct(value_ty).to_string(),
                                el_ty: self.with_ctrct(el_ty).to_string(),
                                span: el_ty.span().clone(),
                            },
                        });
                    }

                    Inference::Type(Type::Primitive {
                        kind: PrimitiveKind::Bool,
                        span: span.clone(),
                    })
                } else {
                    handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "range ty is not numeric or array?",
                            span: span.clone(),
                        },
                    });
                    Inference::Type(Type::Error(span.clone()))
                }
            } else {
                Inference::Dependant(collection_key)
            }
        } else {
            Inference::Dependant(value_key)
        }
    }

    fn infer_array_expr(
        &self,
        handler: &Handler,
        range_expr_key: ExprKey,
        element_exprs: &[ExprKey],
        span: &Span,
    ) -> Inference {
        if element_exprs.is_empty() {
            handler.emit_err(Error::Compile {
                error: CompileError::EmptyArrayExpression { span: span.clone() },
            });

            // Return an array of Error, which is still an array.
            return Inference::Type(Type::Array {
                ty: Box::new(Type::Error(span.clone())),
                range: Some(range_expr_key),
                size: Some(0),
                span: span.clone(),
            });
        }

        let mut elements = element_exprs.iter();

        let el0 = elements
            .next()
            .expect("already check for elements.is_empty()");

        let mut deps = Vec::new();
        let el0_ty = el0.get_ty(self);
        if !el0_ty.is_unknown() {
            for el_key in elements {
                let el_ty = el_key.get_ty(self);
                if !el_ty.is_unknown() {
                    if !el_ty.eq(&self.new_types, el0_ty) {
                        handler.emit_err(Error::Compile {
                            error: CompileError::NonHomogeneousArrayElement {
                                expected_ty: self.with_ctrct(&el0_ty).to_string(),
                                ty: self.with_ctrct(el_ty).to_string(),
                                span: self.expr_key_to_span(*el_key),
                            },
                        });
                    }
                } else {
                    deps.push(*el_key);
                }
            }

            if range_expr_key.get_ty(self).is_unknown() {
                deps.push(range_expr_key);
            }

            if deps.is_empty() {
                Inference::Type(Type::Array {
                    ty: Box::new(el0_ty.clone()),
                    range: Some(range_expr_key),
                    size: Some(element_exprs.len() as i64),
                    span: span.clone(),
                })
            } else {
                Inference::Dependencies(deps)
            }
        } else {
            Inference::Dependant(*el0)
        }
    }

    fn infer_index_expr(
        &self,
        handler: &Handler,
        array_expr_key: ExprKey,
        index_expr_key: ExprKey,
        span: &Span,
    ) -> Inference {
        let index_ty = index_expr_key.get_ty(self);
        if index_ty.is_unknown() {
            return Inference::Dependant(index_expr_key);
        }

        let ary_ty = array_expr_key.get_ty(self);
        if ary_ty.is_unknown() {
            return Inference::Dependant(array_expr_key);
        }

        if let Some(range_expr_key) = ary_ty.get_array_range_expr() {
            // Is this an array?
            let range_ty = range_expr_key.get_ty(self);
            if range_ty.is_unknown() {
                return Inference::Dependant(range_expr_key);
            }

            if (!index_ty.is_int() && !index_ty.is_enum(&self.enums))
                || !index_ty.eq(&self.new_types, range_ty)
            {
                handler.emit_err(Error::Compile {
                    error: CompileError::ArrayAccessWithWrongType {
                        found_ty: self.with_ctrct(index_ty).to_string(),
                        expected_ty: self.with_ctrct(range_ty).to_string(),
                        span: self.expr_key_to_span(index_expr_key),
                    },
                });
            }
            if let Some(el_ty) = ary_ty.get_array_el_type() {
                Inference::Type(el_ty.clone())
            } else {
                handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "failed to get array element type in infer_index_expr()",
                        span: span.clone(),
                    },
                });

                Inference::Type(Type::Error(span.clone()))
            }
        } else if let Some(el_ty) = ary_ty.get_array_el_type() {
            // Is it an array with an unknown range (probably a const immediate)?
            Inference::Type(el_ty.clone())
        } else if let Some(from_ty) = ary_ty.get_map_ty_from() {
            // Is this a storage map?
            if !from_ty.eq(&self.new_types, index_ty) {
                handler.emit_err(Error::Compile {
                    error: CompileError::StorageMapAccessWithWrongType {
                        found_ty: self.with_ctrct(index_ty).to_string(),
                        expected_ty: self.with_ctrct(from_ty).to_string(),
                        span: self.expr_key_to_span(index_expr_key),
                    },
                });

                Inference::Type(Type::Error(span.clone()))
            } else if let Some(to_ty) = ary_ty.get_map_ty_to() {
                Inference::Type(to_ty.clone())
            } else {
                handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "failed to get array element type \
                          in infer_index_expr()",
                        span: span.clone(),
                    },
                });

                Inference::Type(Type::Error(span.clone()))
            }
        } else if let Some(el_ty) = ary_ty.get_vector_element_ty() {
            if !index_ty.is_int() {
                handler.emit_err(Error::Compile {
                    error: CompileError::ArrayAccessWithWrongType {
                        found_ty: self.with_ctrct(index_ty).to_string(),
                        expected_ty: "int".to_string(),
                        span: self.expr_key_to_span(index_expr_key),
                    },
                });
            }

            Inference::Type(el_ty.clone())
        } else {
            handler.emit_err(Error::Compile {
                error: CompileError::IndexExprNonIndexable {
                    non_indexable_type: self.with_ctrct(ary_ty).to_string(),
                    span: span.clone(),
                },
            });

            Inference::Type(Type::Error(span.clone()))
        }
    }

    fn infer_tuple_expr(&self, fields: &[(Option<Ident>, ExprKey)], span: &Span) -> Inference {
        let mut field_tys = Vec::with_capacity(fields.len());

        let mut deps = Vec::new();
        for (name, field_expr_key) in fields {
            let field_ty = field_expr_key.get_ty(self);
            if !field_ty.is_unknown() {
                field_tys.push((name.clone(), field_ty.clone()));
            } else {
                deps.push(*field_expr_key);
            }
        }

        if deps.is_empty() {
            Inference::Type(Type::Tuple {
                fields: field_tys,
                span: span.clone(),
            })
        } else {
            Inference::Dependencies(deps)
        }
    }

    fn infer_tuple_access_expr(
        &self,
        handler: &Handler,
        tuple_expr_key: ExprKey,
        field: &TupleAccess,
        span: &Span,
    ) -> Inference {
        let tuple_ty = tuple_expr_key.get_ty(self);
        if !tuple_ty.is_unknown() {
            if tuple_ty.is_tuple() {
                match field {
                    TupleAccess::Error => {
                        handler.emit_err(Error::Compile {
                            error: CompileError::Internal {
                                msg: "unable to type check tuple field access error",
                                span: span.clone(),
                            },
                        });
                        Inference::Type(Type::Error(span.clone()))
                    }

                    TupleAccess::Index(idx) => {
                        if let Some(field_ty) = tuple_ty.get_tuple_field_type_by_idx(*idx) {
                            Inference::Type(field_ty.clone())
                        } else {
                            handler.emit_err(Error::Compile {
                                error: CompileError::InvalidTupleAccessor {
                                    accessor: idx.to_string(),
                                    tuple_type: self.with_ctrct(tuple_ty).to_string(),
                                    span: span.clone(),
                                },
                            });

                            Inference::Type(Type::Error(span.clone()))
                        }
                    }

                    TupleAccess::Name(name) => {
                        if let Some(field_ty) = tuple_ty.get_tuple_field_type_by_name(name) {
                            Inference::Type(field_ty.clone())
                        } else {
                            handler.emit_err(Error::Compile {
                                error: CompileError::InvalidTupleAccessor {
                                    accessor: name.name.clone(),
                                    tuple_type: self.with_ctrct(&tuple_ty).to_string(),
                                    span: span.clone(),
                                },
                            });

                            Inference::Type(Type::Error(span.clone()))
                        }
                    }
                }
            } else {
                handler.emit_err(Error::Compile {
                    error: CompileError::TupleAccessNonTuple {
                        non_tuple_type: self.with_ctrct(tuple_ty).to_string(),
                        span: span.clone(),
                    },
                });

                Inference::Type(Type::Error(span.clone()))
            }
        } else {
            Inference::Dependant(tuple_expr_key)
        }
    }

    fn infer_union_expr(
        &self,
        handler: &Handler,
        name: &str,
        name_span: &Span,
        value: Option<ExprKey>,
        span: &Span,
    ) -> Inference {
        // Re-split the path into pre :: union_name :: variant_name.  This is a bit
        // unfortunate, and perhaps we need to revisit how we do paths and idents.
        if let Some(sep_idx) = name.rfind("::") {
            let union_name = &name[0..sep_idx];
            let variant_name = &name[(sep_idx + 2)..];

            // Find the union.
            if let Some(union_decl) = self.unions.iter().find(|ud| ud.name.name == union_name) {
                // Find the variant.
                if let Some(opt_var_ty) = union_decl.variants.iter().find_map(
                    |UnionVariant {
                         variant_name: ud_nm,
                         ty: ud_ty,
                         ..
                     }| { (ud_nm.name == variant_name).then_some(ud_ty) },
                ) {
                    match (value, opt_var_ty) {
                        (None, None) => {
                            // The union variant doesn't have a binding and no value was given.
                            Inference::Type(Type::Union {
                                path: union_name.to_string(),
                                span: union_decl.span.clone(),
                            })
                        }

                        (Some(value), Some(ud_var_ty)) => {
                            // Confirm variant type is value type.
                            let value_ty = value.get_ty(self);
                            if value_ty.is_unknown() {
                                Inference::Dependant(value)
                            } else if !value_ty.eq(&self.new_types, ud_var_ty) {
                                // Type mismatch for variant value.
                                handler.emit_err(Error::Compile {
                                    error: CompileError::UnionVariantTypeMismatch {
                                        found_ty: self.with_ctrct(value_ty).to_string(),
                                        expected_ty: self.with_ctrct(ud_var_ty).to_string(),
                                        span: self.expr_key_to_span(value),
                                    },
                                });

                                Inference::Type(Type::Error(span.clone()))
                            } else {
                                // Provided value has the correct type for the variant.
                                Inference::Type(Type::Union {
                                    path: union_name.to_string(),
                                    span: union_decl.span.clone(),
                                })
                            }
                        }

                        (Some(_), None) => {
                            // A value was provided for a variant which doesn't have a binding.
                            handler.emit_err(Error::Compile {
                                error: CompileError::SuperfluousUnionExprValue {
                                    name: name.to_string(),
                                    span: span.clone(),
                                },
                            });

                            Inference::Type(Type::Error(span.clone()))
                        }

                        (None, Some(ud_var_ty)) => {
                            // A value is required by the variant but was no provided.
                            //
                            // NOTE: This currently can't really occur since an Expr::Union is only
                            // generated by the parser when there *is* a passed value.
                            handler.emit_err(Error::Compile {
                                error: CompileError::MissingUnionExprValue {
                                    name: name.to_string(),
                                    variant_ty: self.with_ctrct(ud_var_ty).to_string(),
                                    span: span.clone(),
                                },
                            });

                            Inference::Type(Type::Error(span.clone()))
                        }
                    }
                } else {
                    // Unable to find the union variant.
                    let valid_variants = union_decl
                        .variants
                        .iter()
                        .map(|v| v.variant_name.to_string())
                        .collect::<Vec<_>>();

                    handler.emit_err(Error::Compile {
                        error: CompileError::UnknownUnionVariant {
                            name: name.to_string(),
                            valid_variants,
                            span: name_span.clone(),
                        },
                    });

                    Inference::Type(Type::Error(span.clone()))
                }
            } else {
                // Unable to find the union declaration.
                handler.emit_err(Error::Compile {
                    error: CompileError::UnknownUnion {
                        name: name.to_string(),
                        span: name_span.clone(),
                    },
                });

                Inference::Type(Type::Error(span.clone()))
            }
        } else {
            // Unable to split the name.
            handler.emit_err(Error::Compile {
                error: CompileError::UnknownUnion {
                    name: name.to_string(),
                    span: name_span.clone(),
                },
            });

            Inference::Type(Type::Error(span.clone()))
        }
    }

    fn infer_generator_expr(
        &self,
        handler: &Handler,
        kind: &GeneratorKind,
        ranges: &[(Ident, ExprKey)],
        conditions: &[ExprKey],
        body_expr_key: ExprKey,
        span: &Span,
    ) -> Inference {
        let mut deps = Vec::new();

        for (_, range_expr_key) in ranges {
            let range_ty = range_expr_key.get_ty(self);
            if !range_ty.is_unknown() {
                if !range_ty.is_int() {
                    handler.emit_err(Error::Compile {
                        error: CompileError::NonIntGeneratorRange {
                            ty: self.with_ctrct(range_ty).to_string(),
                            gen_kind: kind.to_string(),
                            span: self.expr_key_to_span(*range_expr_key),
                        },
                    });
                }
            } else {
                deps.push(*range_expr_key);
            }
        }

        for cond_expr_key in conditions {
            let cond_ty = cond_expr_key.get_ty(self);
            if !cond_ty.is_unknown() {
                if !cond_ty.is_bool() {
                    handler.emit_err(Error::Compile {
                        error: CompileError::NonBoolGeneratorCondition {
                            ty: self.with_ctrct(cond_ty).to_string(),
                            gen_kind: kind.to_string(),
                            span: self.expr_key_to_span(*cond_expr_key),
                        },
                    });
                }
            } else {
                deps.push(*cond_expr_key);
            }
        }

        let body_ty = body_expr_key.get_ty(self);
        if !body_ty.is_unknown() {
            if !body_ty.is_bool() {
                handler.emit_err(Error::Compile {
                    error: CompileError::NonBoolGeneratorBody {
                        ty: self.with_ctrct(body_ty).to_string(),
                        gen_kind: kind.to_string(),
                        span: self.expr_key_to_span(body_expr_key),
                    },
                });
            }
        } else {
            deps.push(body_expr_key);
        }

        if deps.is_empty() {
            Inference::Type(Type::Primitive {
                kind: PrimitiveKind::Bool,
                span: span.clone(),
            })
        } else {
            Inference::Dependencies(deps)
        }
    }

    // Confirm that all var init exprs and const init exprs match their declared type, if they have
    // one.
    pub(super) fn check_inits(&self, handler: &Handler) -> Result<(), ErrorEmitted> {
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
