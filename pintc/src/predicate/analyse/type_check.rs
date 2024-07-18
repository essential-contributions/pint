use super::{
    BlockStatement, Const, ConstraintDecl, Expr, ExprKey, Ident, IfDecl, Inference,
    InterfaceInstance, Predicate, PredicateInstance, VarKey,
};
use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler, LargeTypeError},
    expr::{BinaryOp, GeneratorKind, Immediate, TupleAccess, UnaryOp},
    predicate::StorageVar,
    span::{empty_span, Span, Spanned},
    types::{EnumDecl, EphemeralDecl, NewTypeDecl, Path, PrimitiveKind, Type},
};
use fxhash::{FxHashMap, FxHashSet};

impl Predicate {
    pub(super) fn lower_newtypes(&mut self, handler: &Handler) -> Result<(), ErrorEmitted> {
        self.check_recursive_newtypes(handler)?;
        self.lower_newtypes_in_newtypes(handler)?;
        self.lower_newtypes_in_contract();

        Ok(())
    }

    fn check_recursive_newtypes(&self, handler: &Handler) -> Result<(), ErrorEmitted> {
        fn inspect_type_names<'a>(
            handler: &Handler,
            new_types: &'a [NewTypeDecl],
            seen_names: &mut FxHashMap<&'a str, &'a Span>,
            ty: &'a Type,
        ) -> Result<(), ErrorEmitted> {
            match ty {
                Type::Custom {
                    path,
                    span: custom_ty_span,
                } => {
                    // Look-up the name to confirm it's a new-type.
                    if let Some((new_ty, new_ty_span)) =
                        new_types.iter().find_map(|NewTypeDecl { name, ty, span }| {
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
                            let res = inspect_type_names(handler, new_types, seen_names, new_ty);
                            seen_names.remove(path.as_str());
                            res
                        }
                    } else {
                        // Will be a path to an enum or variant.
                        Ok(())
                    }
                }

                Type::Array { ty, .. } => inspect_type_names(handler, new_types, seen_names, ty),

                Type::Tuple { fields, .. } => fields
                    .iter()
                    .try_for_each(|(_, ty)| inspect_type_names(handler, new_types, seen_names, ty)),

                Type::Alias { ty, .. } => inspect_type_names(handler, new_types, seen_names, ty),

                Type::Map { ty_from, ty_to, .. } => {
                    inspect_type_names(handler, new_types, seen_names, ty_from)?;
                    inspect_type_names(handler, new_types, seen_names, ty_to)
                }

                Type::Error(_) | Type::Unknown(_) | Type::Primitive { .. } => Ok(()),
            }
        }

        for NewTypeDecl { name, ty, span } in &self.new_types {
            let mut seen_names = FxHashMap::from_iter(std::iter::once((name.name.as_str(), span)));

            let _ = inspect_type_names(handler, &self.new_types, &mut seen_names, ty);
        }

        if handler.has_errors() {
            Err(handler.cancel())
        } else {
            Ok(())
        }
    }

    fn lower_newtypes_in_newtypes(&mut self, handler: &Handler) -> Result<(), ErrorEmitted> {
        // Search for a custom type with a specific name and return a mut ref to it.
        fn get_custom_type_mut_ref<'a>(
            custom_path: &str,
            ty: &'a mut Type,
        ) -> Option<&'a mut Type> {
            match ty {
                Type::Custom { path, .. } => (path == custom_path).then_some(ty),

                Type::Array { ty, .. } => get_custom_type_mut_ref(custom_path, ty),

                Type::Tuple { fields, .. } => fields
                    .iter_mut()
                    .find_map(|(_, fld_ty)| get_custom_type_mut_ref(custom_path, fld_ty)),

                Type::Alias { ty, .. } => get_custom_type_mut_ref(custom_path, ty),

                Type::Map { ty_from, ty_to, .. } => get_custom_type_mut_ref(custom_path, ty_from)
                    .or_else(|| get_custom_type_mut_ref(custom_path, ty_to)),

                Type::Error(_) | Type::Unknown(_) | Type::Primitive { .. } => None,
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
                            msg: "infinite loop in lower_newtypes_in_newtypes()",
                            span: empty_span(),
                        },
                    }));
                }
            }
        }

        Ok(())
    }

    fn lower_newtypes_in_contract(&mut self) {
        use std::borrow::BorrowMut;

        fn replace_custom_type(new_types: &[NewTypeDecl], ty: &mut Type) {
            match ty {
                Type::Custom { path, .. } => {
                    if let Some((new_ty, new_span)) =
                        new_types.iter().find_map(|NewTypeDecl { name, ty, span }| {
                            (&name.name == path).then_some((ty, span))
                        })
                    {
                        *ty = Type::Alias {
                            path: path.clone(),
                            ty: Box::new(new_ty.clone()),
                            span: new_span.clone(),
                        };
                    }
                }

                Type::Alias { ty, .. } => replace_custom_type(new_types, ty),

                Type::Array { ty, .. } => {
                    replace_custom_type(new_types, ty.borrow_mut());
                }

                Type::Tuple { fields, .. } => {
                    for (_, ty) in fields.iter_mut() {
                        replace_custom_type(new_types, ty);
                    }
                }

                Type::Map { ty_from, ty_to, .. } => {
                    replace_custom_type(new_types, ty_from);
                    replace_custom_type(new_types, ty_to);
                }

                Type::Error(_) | Type::Unknown(_) | Type::Primitive { .. } => {}
            }
        }

        // Then, loop for every known var or state type, or any `as` cast expr, or any storage
        // type, and replace any custom types which match with the type alias.
        self.vars
            .update_types(|_, ty| replace_custom_type(&self.new_types, ty));

        self.states
            .update_types(|_, ty| replace_custom_type(&self.new_types, ty));

        self.exprs.update_exprs(|_, expr| {
            if let Expr::Cast { ty, .. } = expr {
                replace_custom_type(&self.new_types, ty.borrow_mut());
            }
        });

        if let Some((storage_vars, _)) = &mut self.storage {
            for StorageVar { ty, .. } in storage_vars {
                replace_custom_type(&self.new_types, ty);
            }
        }
    }

    pub(super) fn check_undefined_types(&mut self, handler: &Handler) {
        let valid_custom_tys: FxHashSet<&String> = FxHashSet::from_iter(
            self.enums
                .iter()
                .map(|ed| &ed.name.name)
                .chain(self.new_types.iter().map(|ntd| &ntd.name.name)),
        );

        for (state_key, _) in self.states() {
            if let Type::Custom { path, span, .. } = state_key.get_ty(self) {
                if !valid_custom_tys.contains(path) {
                    handler.emit_err(Error::Compile {
                        error: CompileError::UndefinedType { span: span.clone() },
                    });
                }
            }
        }

        for (var_key, _) in self.vars() {
            if let Type::Custom { path, span, .. } = var_key.get_ty(self) {
                if !valid_custom_tys.contains(path) {
                    handler.emit_err(Error::Compile {
                        error: CompileError::UndefinedType { span: span.clone() },
                    });
                }
            }
        }

        for expr in self.exprs() {
            if let Type::Custom { path, span, .. } = expr.get_ty(self) {
                if !valid_custom_tys.contains(path) {
                    handler.emit_err(Error::Compile {
                        error: CompileError::UndefinedType { span: span.clone() },
                    });
                }
            }
        }

        for expr_key in self.exprs() {
            if let Some(Expr::Cast { ty, .. }) = expr_key.try_get(self) {
                if let Type::Custom { path, span, .. } = ty.as_ref() {
                    if !valid_custom_tys.contains(path) {
                        handler.emit_err(Error::Compile {
                            error: CompileError::UndefinedType { span: span.clone() },
                        });
                    }
                }
            }
        }
    }

    pub(super) fn check_constraint_types(&mut self, handler: &Handler) {
        // After all expression types are inferred, then all constraint expressions must be of type bool
        self.constraints.iter().for_each(|constraint_decl| {
            let expr_type = constraint_decl.expr.get_ty(self);
            if !expr_type.is_bool() {
                handler.emit_err(Error::Compile {
                    error: CompileError::ConstraintExpressionTypeError {
                        large_err: Box::new(LargeTypeError::ConstraintExpressionTypeError {
                            expected_ty: self
                                .with_pred(Type::Primitive {
                                    kind: PrimitiveKind::Bool,
                                    span: empty_span(),
                                })
                                .to_string(),
                            found_ty: self.with_pred(expr_type).to_string(),
                            span: constraint_decl.span.clone(),
                            expected_span: Some(constraint_decl.span.clone()),
                        }),
                    },
                });
            }
        })
    }

    pub(super) fn check_storage_types(&self, handler: &Handler) {
        if !self.is_root() {
            // Only self check when we're the root predicate.
            return;
        }

        if let Some((storage_vars, _)) = &self.storage {
            for StorageVar { ty, span, .. } in storage_vars {
                if !(ty.is_bool() || ty.is_int() || ty.is_b256() || ty.is_tuple() || ty.is_array())
                {
                    let ty = ty.is_alias().unwrap_or(ty);
                    if let Type::Map {
                        ref ty_from,
                        ref ty_to,
                        ..
                    } = ty
                    {
                        if !((ty_from.is_bool() || ty_from.is_int() || ty_from.is_b256())
                            && (ty_to.is_bool()
                                || ty_to.is_int()
                                || ty_to.is_b256()
                                || ty_to.is_map()
                                || ty_to.is_tuple()
                                || ty_to.is_array()))
                        {
                            // TODO: allow arbitrary types in storage maps
                            handler.emit_err(Error::Compile {
                                error: CompileError::Internal {
                                    msg: "currently in storage maps, keys must be int, bool, or b256 \
                                            and values must be int or bool",
                                    span: span.clone(),
                                },
                            });
                        }
                    } else {
                        // TODO: allow arbitrary types in storage blocks
                        handler.emit_err(Error::Compile {
                            error: CompileError::Internal {
                                msg: "only ints, bools, and maps are currently allowed in a \
                                        storage block",
                                span: span.clone(),
                            },
                        });
                    }
                }
            }
        }
    }

    pub(super) fn check_for_map_type_vars(&mut self, handler: &Handler) {
        // Ex. var x: ( int => int ); is disallowed
        self.vars().for_each(|(var_key, var)| {
            let ty = var_key.get_ty(self);
            if ty.is_map() {
                handler.emit_err(Error::Compile {
                    error: CompileError::VarTypeIsMap {
                        span: var.span.clone(),
                    },
                });
            }
        })
    }

    pub(super) fn type_check_all_exprs(
        &mut self,
        handler: &Handler,
        consts: &FxHashMap<String, Const>,
    ) {
        self.check_iface_inst_addrs(handler, consts);
        self.check_pred_inst_addrs(handler, consts);

        // Check all the 'root' exprs (constraints, state init exprs, and var init exprs) one at a
        // time, gathering errors as we go. Copying the keys out first to avoid borrowing conflict.
        let mut all_expr_keys = self
            .constraints
            .iter()
            .map(|ConstraintDecl { expr: key, .. }| *key)
            .chain(self.states().map(|(_, state)| state.expr))
            .chain(self.var_inits.iter().map(|(_, expr)| *expr))
            .collect::<Vec<_>>();

        // When we're checking the root predicate we check all the consts too.
        if self.is_root() {
            all_expr_keys.extend(consts.iter().map(|(_, Const { expr, .. })| *expr));
        }

        for expr_key in all_expr_keys {
            if let Err(err) = self.type_check_single_expr(consts, expr_key) {
                handler.emit_err(err);
            }
        }

        // Now check all if declarations
        self.if_decls
            .clone()
            .iter()
            .for_each(|if_decl| self.type_check_if_decl(consts, if_decl, handler));

        // Confirm now that all decision variables are typed.
        let mut var_key_to_new_type = FxHashMap::default();
        for (var_key, var) in self.vars() {
            let ty = var_key.get_ty(self);
            if ty.is_unknown() {
                if let Some(init_expr_key) = self.var_inits.get(var_key) {
                    let ty = init_expr_key.get_ty(self);
                    if !ty.is_unknown() {
                        if var.is_pub
                            && !(ty.is_bool()
                                || ty.is_b256()
                                || ty.is_int()
                                || ty.is_tuple()
                                || ty.is_array())
                        {
                            handler.emit_err(Error::Compile {
                                error: CompileError::Internal {
                                    msg: "only `bool`, b256`, `int`, tuple, and array pub vars \
                                          are currently supported",
                                    span: var.span.clone(),
                                },
                            });
                        } else {
                            var_key_to_new_type.insert(var_key, ty.clone());
                        }
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
            } else if var.is_pub
                && !(ty.is_bool() || ty.is_b256() || ty.is_int() || ty.is_tuple() || ty.is_array())
            {
                handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "only `bool`, b256`, `int`, tuple, and array pub vars \
                            are currently supported",
                        span: var.span.clone(),
                    },
                });
            }
        }

        self.vars.update_types(|var_key, ty| {
            if let Some(new_ty) = var_key_to_new_type.get(&var_key) {
                *ty = new_ty.clone()
            }
        });

        // Confirm now that all state variables are typed.
        let mut state_key_to_new_type = FxHashMap::default();
        for (state_key, state) in self.states() {
            let state_ty = state_key.get_ty(self);
            if !state_ty.is_unknown() {
                let expr_ty = state.expr.get_ty(self);
                if !expr_ty.is_unknown() {
                    if !state_ty.eq(self, expr_ty) {
                        handler.emit_err(Error::Compile {
                            error: CompileError::StateVarInitTypeError {
                                large_err: Box::new(LargeTypeError::StateVarInitTypeError {
                                    expected_ty: self.with_pred(state_ty).to_string(),
                                    found_ty: self.with_pred(expr_ty).to_string(),
                                    span: self.expr_key_to_span(state.expr),
                                    expected_span: Some(state_ty.span().clone()),
                                }),
                            },
                        });
                    }
                    // State variables of type `Map` are not allowed
                    if state_ty.is_map() {
                        handler.emit_err(Error::Compile {
                            error: CompileError::StateVarTypeIsMap {
                                span: state.span.clone(),
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
                    // State variables of type `Map` are not allowed
                    if expr_ty.is_map() {
                        handler.emit_err(Error::Compile {
                            error: CompileError::StateVarTypeIsMap {
                                span: state.span.clone(),
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
            }
        }
        self.states.update_types(|state_key, ty| {
            if let Some(new_ty) = state_key_to_new_type.get(&state_key) {
                *ty = new_ty.clone()
            }
        });

        // Last thing we have to do is to type check all the range expressions in array types and
        // make sure they are integers or enums
        let mut checked_range_exprs = FxHashSet::default();
        for range_expr in self
            .vars()
            .filter_map(|(var_key, _)| var_key.get_ty(self).get_array_range_expr())
            .chain(
                self.states()
                    .filter_map(|(state_key, _)| state_key.get_ty(self).get_array_range_expr()),
            )
            .chain(
                self.exprs()
                    .filter_map(|expr_key| expr_key.get_ty(self).get_array_range_expr()),
            )
            .collect::<Vec<_>>()
            .iter()
        {
            if let Err(err) = self.type_check_single_expr(consts, *range_expr) {
                handler.emit_err(err);
            } else if !(range_expr.get_ty(self).is_int()
                || range_expr.get_ty(self).is_enum(self)
                || checked_range_exprs.contains(range_expr))
            {
                handler.emit_err(Error::Compile {
                    error: CompileError::InvalidArrayRangeType {
                        found_ty: self.with_pred(range_expr.get_ty(self)).to_string(),
                        span: self.expr_key_to_span(*range_expr),
                    },
                });
                // Make sure to not collect too many duplicate errors
                checked_range_exprs.insert(range_expr);
            }
        }
    }

    fn check_iface_inst_addrs(&mut self, handler: &Handler, consts: &FxHashMap<String, Const>) {
        // Type check all interface instance declarations.
        let mut addr_keys = Vec::default();
        for InterfaceInstance {
            interface,
            address,
            span,
            ..
        } in &self.interface_instances
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

        self.check_instance_addresses(handler, consts, &addr_keys);
    }

    fn check_pred_inst_addrs(&mut self, handler: &Handler, consts: &FxHashMap<String, Const>) {
        // Type check all predicate instance declarations.
        let mut addr_keys = Vec::default();
        for PredicateInstance {
            interface_instance,
            predicate,
            address,
            span,
            ..
        } in &self.predicate_instances
        {
            // Make sure that an appropriate interface instance exists and an appropriate
            // predicate interface exists.
            if let Some(interface_instance) = self
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
                        addr_keys.push(*address);
                    } else {
                        handler.emit_err(Error::Compile {
                            error: CompileError::MissingPredicateInterface {
                                pred_name: predicate.name.to_string(),
                                interface_name: interface.name.to_string(),
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
        }

        self.check_instance_addresses(handler, consts, &addr_keys);
    }

    fn check_instance_addresses(
        &mut self,
        handler: &Handler,
        consts: &FxHashMap<String, Const>,
        addr_keys: &[ExprKey],
    ) {
        for address in addr_keys {
            match self.type_check_single_expr(consts, *address) {
                Ok(()) => {
                    let ty = address.get_ty(self);
                    if !ty.is_b256() {
                        handler.emit_err(Error::Compile {
                            error: CompileError::AddressExpressionTypeError {
                                large_err: Box::new(LargeTypeError::AddressExpressionTypeError {
                                    expected_ty: self
                                        .with_pred(Type::Primitive {
                                            kind: PrimitiveKind::B256,
                                            span: empty_span(),
                                        })
                                        .to_string(),
                                    found_ty: self.with_pred(ty).to_string(),
                                    span: self.expr_key_to_span(*address),
                                    expected_span: Some(self.expr_key_to_span(*address)),
                                }),
                            },
                        });
                    }
                }

                Err(err) => {
                    handler.emit_err(err);
                }
            }
        }
    }

    // Type check an if statement and all of its sub-statements including other ifs. This is a
    // recursive function.
    fn type_check_if_decl(
        &mut self,
        consts: &FxHashMap<String, Const>,
        if_decl: &IfDecl,
        handler: &Handler,
    ) {
        let IfDecl {
            condition,
            then_block,
            else_block,
            ..
        } = if_decl;

        // Make sure the condition is a `bool`
        if let Err(err) = self.type_check_single_expr(consts, *condition) {
            handler.emit_err(err);
        } else {
            let cond_ty = condition.get_ty(self);
            if !cond_ty.is_bool() {
                handler.emit_err(Error::Compile {
                    error: CompileError::NonBoolConditional {
                        ty: self.with_pred(cond_ty).to_string(),
                        conditional: "`if` statement".to_string(),
                        span: self.expr_key_to_span(*condition),
                    },
                });
            }
        }

        // Type check each block statement in the "then" block
        then_block.iter().for_each(|block_statement| {
            self.type_check_block_statement(consts, block_statement, handler);
        });

        // Type check each block statement in the "else" block, if available
        else_block.iter().flatten().for_each(|block_statement| {
            self.type_check_block_statement(consts, block_statement, handler);
        });
    }

    fn type_check_block_statement(
        &mut self,
        consts: &FxHashMap<String, Const>,
        block_statement: &BlockStatement,
        handler: &Handler,
    ) {
        match block_statement {
            BlockStatement::Constraint(ConstraintDecl { expr, .. }) => {
                if let Err(err) = self.type_check_single_expr(consts, *expr) {
                    handler.emit_err(err);
                }
            }
            BlockStatement::If(if_decl) => self.type_check_if_decl(consts, if_decl, handler),
        }
    }

    fn type_check_single_expr(
        &mut self,
        consts: &FxHashMap<String, Const>,
        expr_key: ExprKey,
    ) -> Result<(), Error> {
        // Attempt to infer all the types of each expr.
        let mut queue = Vec::new();

        // Utility to check for recursion in the queue.  A macro to avoid borrowing self.
        macro_rules! push_to_queue {
            ($dependant_key: ident, $dependency_key: ident) => {
                if queue.contains(&$dependency_key) {
                    Err(Error::Compile {
                        error: CompileError::ExprRecursion {
                            dependant_span: self.expr_key_to_span($dependant_key),
                            dependency_span: self.expr_key_to_span($dependency_key),
                        },
                    })
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
                match self.infer_expr_key_type(consts, next_key)? {
                    // Successfully inferred its type.  Save it and pop it from the queue.
                    Inference::Type(ty) => {
                        next_key.set_ty(ty, self);
                        queue.pop();
                    }

                    // Some dependencies need to be inferred before we can get back to this
                    // expr.  When pushing dependencies we need to check if they're already
                    // queued, in which case we have a recursive dependency and an error.
                    Inference::Dependant(dep_expr_key) => push_to_queue!(next_key, dep_expr_key)?,
                    Inference::Dependencies(mut dep_expr_keys) => dep_expr_keys
                        .drain(..)
                        .try_for_each(|dep_expr_key| push_to_queue!(next_key, dep_expr_key))?,

                    // Some expressions (e.g., macro calls) just aren't valid any longer and
                    // are best ignored.
                    Inference::Ignored => {
                        queue.pop();
                    }
                }
            }
        }

        Ok(())
    }

    fn infer_expr_key_type(
        &self,
        consts: &FxHashMap<String, Const>,
        expr_key: ExprKey,
    ) -> Result<Inference, Error> {
        let expr: &Expr = expr_key.try_get(self).ok_or_else(|| Error::Compile {
            error: CompileError::Internal {
                msg: "orphaned expr key when type checking",
                span: empty_span(),
            },
        })?;

        match expr {
            Expr::Error(span) => Err(Error::Compile {
                error: CompileError::Internal {
                    msg: "unable to type check from error expression",
                    span: span.clone(),
                },
            }),

            Expr::Immediate { value, span } => self.infer_immediate(value, span),

            Expr::Array {
                elements,
                range_expr,
                span,
            } => self.infer_array_expr(*range_expr, elements, span),

            Expr::Tuple { fields, span } => self.infer_tuple_expr(fields, span),

            Expr::PathByKey(var_key, span) => self.infer_path_by_key(*var_key, span),

            Expr::PathByName(path, span) => self.infer_path_by_name(consts, path, span),

            Expr::StorageAccess(name, span) => self.infer_storage_access(name, span),

            Expr::ExternalStorageAccess {
                interface_instance,
                name,
                span,
                ..
            } => self.infer_external_storage_access(interface_instance, name, span),

            Expr::UnaryOp {
                op,
                expr: op_expr_key,
                span,
            } => self.infer_unary_op(*op, *op_expr_key, span),

            Expr::BinaryOp { op, lhs, rhs, span } => self.infer_binary_op(*op, *lhs, *rhs, span),

            Expr::MacroCall { .. } => Ok(Inference::Ignored),

            Expr::IntrinsicCall { name, args, span } => {
                self.infer_intrinsic_call_expr(name, args, span)
            }

            Expr::Select {
                condition,
                then_expr,
                else_expr,
                span,
            } => self.infer_select_expr(*condition, *then_expr, *else_expr, span),

            Expr::Index { expr, index, span } => self.infer_index_expr(*expr, *index, span),

            Expr::TupleFieldAccess { tuple, field, span } => {
                self.infer_tuple_access_expr(*tuple, field, span)
            }

            Expr::Cast { value, ty, span } => self.infer_cast_expr(*value, ty, span),

            Expr::In {
                value,
                collection,
                span,
            } => self.infer_in_expr(*value, *collection, span),

            Expr::Range { lb, ub, span } => self.infer_range_expr(*lb, *ub, span),

            Expr::Generator {
                kind,
                gen_ranges,
                conditions,
                body,
                span,
            } => self.infer_generator_expr(kind, gen_ranges, conditions, *body, span),
        }
    }

    pub(super) fn infer_immediate(&self, imm: &Immediate, span: &Span) -> Result<Inference, Error> {
        if let Immediate::Array(el_imms) = imm {
            // Immediate::get_ty() assumes the array is well formed.  We need to
            // confirm here.
            if el_imms.is_empty() {
                return Err(Error::Compile {
                    error: CompileError::EmptyArrayExpression { span: span.clone() },
                });
            }

            // Get the assumed type.
            let ary_ty = imm.get_ty(Some(span));
            let Type::Array { ty: el0_ty, .. } = &ary_ty else {
                return Err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "array immediate does NOT have an array type?",
                        span: span.clone(),
                    },
                });
            };

            el_imms.iter().try_for_each(|el_imm| {
                let el_ty = el_imm.get_ty(None);
                if !el_ty.eq(self, el0_ty.as_ref()) {
                    Err(Error::Compile {
                        error: CompileError::NonHomogeneousArrayElement {
                            expected_ty: self.with_pred(el0_ty.as_ref()).to_string(),
                            ty: self.with_pred(el_ty).to_string(),
                            span: span.clone(),
                        },
                    })
                } else {
                    Ok(())
                }
            })?;

            Ok(Inference::Type(ary_ty))
        } else {
            Ok(Inference::Type(imm.get_ty(Some(span))))
        }
    }

    fn infer_path_by_key(&self, var_key: VarKey, span: &Span) -> Result<Inference, Error> {
        let ty = var_key.get_ty(self);
        if !ty.is_unknown() {
            Ok(Inference::Type(ty.clone()))
        } else if let Some(init_expr_key) = self.var_inits.get(var_key) {
            let init_expr_ty = init_expr_key.get_ty(self);
            if !init_expr_ty.is_unknown() {
                Ok(Inference::Type(init_expr_ty.clone()))
            } else {
                // We have a variable with an initialiser but don't know the initialiser type
                // yet.
                Ok(Inference::Dependant(*init_expr_key))
            }
        } else {
            Err(Error::Compile {
                error: CompileError::Internal {
                    msg: "untyped variable doesn't have initialiser",
                    span: span.clone(),
                },
            })
        }
    }

    fn infer_path_by_name(
        &self,
        consts: &FxHashMap<String, Const>,
        path: &Path,
        span: &Span,
    ) -> Result<Inference, Error> {
        if let Some(var_key) = self
            .vars()
            .find_map(|(var_key, var)| (&var.name == path).then_some(var_key))
        {
            // It's a var.
            self.infer_path_by_key(var_key, span)
        } else if let Some((state_key, state)) =
            self.states().find(|(_, state)| (&state.name == path))
        {
            // It's state.
            let state_expr_ty = state.expr.get_ty(self);
            let state_type = state_key.get_ty(self);
            Ok(if !state_type.is_unknown() {
                Inference::Type(state_type.clone())
            } else if !state_expr_ty.is_unknown() {
                Inference::Type(state_expr_ty.clone())
            } else {
                Inference::Dependant(state.expr)
            })
        } else if let Some(Const { decl_ty, .. }) = consts.get(path) {
            if !decl_ty.is_unknown() {
                Ok(Inference::Type(decl_ty.clone()))
            } else {
                Err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "const decl has unknown type *after* evaluation",
                        span: span.clone(),
                    },
                })
            }
        } else if let Some(EphemeralDecl { ty, .. }) = self
            .ephemerals
            .iter()
            .find(|eph_decl| &eph_decl.name == path)
        {
            // It's an ephemeral value.
            Ok(Inference::Type(ty.clone()))
        } else if let Some(ty) = self
            .new_types
            .iter()
            .find_map(|NewTypeDecl { name, ty, .. }| (&name.name == path).then_some(ty))
        {
            // It's a fully matched newtype.
            Ok(Inference::Type(ty.clone()))
        } else if let Some(ty) = self.infer_extern_var(path) {
            // It's an external var
            Ok(ty)
        } else {
            // None of the above. That leaves enums.
            self.infer_enum_variant_by_name(path, span)
        }
    }

    fn infer_storage_access(&self, name: &String, span: &Span) -> Result<Inference, Error> {
        match self.storage.as_ref() {
            Some(storage) => match storage.0.iter().find(|s_var| s_var.name.name == *name) {
                Some(s_var) => Ok(Inference::Type(s_var.ty.clone())),
                None => Err(Error::Compile {
                    error: CompileError::StorageSymbolNotFound {
                        name: name.clone(),
                        span: span.clone(),
                    },
                }),
            },
            None => Err(Error::Compile {
                error: CompileError::MissingStorageBlock {
                    name: name.clone(),
                    span: span.clone(),
                },
            }),
        }
    }

    fn infer_external_storage_access(
        &self,
        interface_instance: &Path,
        name: &String,
        span: &Span,
    ) -> Result<Inference, Error> {
        // Find the interface instance or emit an error
        let Some(interface_instance) = self
            .interface_instances
            .iter()
            .find(|e| e.name.to_string() == *interface_instance)
        else {
            return Err(Error::Compile {
                error: CompileError::MissingInterfaceInstance {
                    name: interface_instance.clone(),
                    span: span.clone(),
                },
            });
        };

        // Find the interface declaration corresponding to the interface instance
        let Some(interface) = self
            .interfaces
            .iter()
            .find(|e| e.name.to_string() == *interface_instance.interface)
        else {
            // No need to emit an error here because a `MissingInterface` error should have already
            // been emitted earlier when all interface instances were type checked. Instead, we
            // just return an `Unknown` type knowing that the compilation will fail anyways.
            return Ok(Inference::Type(Type::Unknown(empty_span())));
        };

        // Then, look for the storage variable that this access refers to
        match interface.storage.as_ref() {
            Some(storage) => match storage.0.iter().find(|s_var| s_var.name.name == *name) {
                Some(s_var) => Ok(Inference::Type(s_var.ty.clone())),
                None => Err(Error::Compile {
                    error: CompileError::StorageSymbolNotFound {
                        name: name.clone(),
                        span: span.clone(),
                    },
                }),
            },
            None => Err(Error::Compile {
                error: CompileError::MissingStorageBlock {
                    name: name.clone(),
                    span: span.clone(),
                },
            }),
        }
    }

    fn infer_extern_var(&self, path: &Path) -> Option<Inference> {
        // Look through all available predicate instances and their corresponding interfaces for a var
        // with the same path as `path`
        for PredicateInstance {
            name,
            interface_instance,
            predicate,
            ..
        } in &self.predicate_instances
        {
            if let Some(interface_instance) = self
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
        }

        // No extern var that matches `path`.
        None
    }

    pub(super) fn infer_enum_variant_by_name(
        &self,
        path: &Path,
        span: &Span,
    ) -> Result<Inference, Error> {
        // Check first if the path prefix matches a new type.
        for NewTypeDecl { name, ty, .. } in &self.new_types {
            if let Type::Custom {
                path: enum_path,
                span,
            } = ty
            {
                // This new type is to an enum.  Does the new type path match the passed path?
                if path.starts_with(&name.name) {
                    // Yep, we might have an enum wrapped in a new type.
                    let new_type_len = name.name.len();
                    if path.chars().nth(new_type_len) == Some(':') {
                        // Definitely worth trying.  Recurse.
                        let new_path = enum_path.clone() + &path[new_type_len..];
                        if let ty @ Ok(_) = self.infer_enum_variant_by_name(&new_path, span) {
                            // We found an enum variant.
                            return ty;
                        }
                    }
                }
            }
        }

        // If we're searching for an enum variant and it appears to be unqualified then we can
        // report some hints.
        let mut err_potential_enums = Vec::new();

        if let Some(ty) = self.enums.iter().find_map(
            |EnumDecl {
                 name: enum_name,
                 variants,
                 span,
             }| {
                // The path might be to the enum itself, or to its variants, in which case the
                // returned type is the enum.
                (&enum_name.name == path
                    || variants.iter().any(|variant| {
                        if path[2..] == variant.name {
                            err_potential_enums.push(enum_name.name.clone());
                        }

                        let mut full_variant = enum_name.name.clone();
                        full_variant.push_str("::");
                        full_variant.push_str(&variant.name);

                        &full_variant == path
                    }))
                .then(|| Type::Custom {
                    path: enum_name.name.clone(),
                    span: span.clone(),
                })
            },
        ) {
            Ok(Inference::Type(ty.clone()))
        } else {
            Err(Error::Compile {
                error: CompileError::SymbolNotFound {
                    name: path.clone(),
                    span: span.clone(),
                    enum_names: err_potential_enums,
                },
            })
        }
    }

    fn infer_unary_op(
        &self,
        op: UnaryOp,
        rhs_expr_key: ExprKey,
        span: &Span,
    ) -> Result<Inference, Error> {
        fn drill_down_to_path(
            pred: &Predicate,
            expr_key: &ExprKey,
            span: &Span,
        ) -> Result<(), Error> {
            match expr_key.try_get(pred) {
                Some(Expr::PathByName(name, span)) => {
                    if !pred.states().any(|(_, state)| state.name == *name) {
                        Err(Error::Compile {
                            error: CompileError::InvalidNextStateAccess { span: span.clone() },
                        })
                    } else {
                        Ok(())
                    }
                }

                Some(Expr::PathByKey(var_key, span)) => {
                    if !pred
                        .states()
                        .any(|(_, state)| state.name == var_key.get(pred).name)
                    {
                        Err(Error::Compile {
                            error: CompileError::InvalidNextStateAccess { span: span.clone() },
                        })
                    } else {
                        Ok(())
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
                    drill_down_to_path(pred, expr, span)
                }

                _ => Err(Error::Compile {
                    error: CompileError::InvalidNextStateAccess { span: span.clone() },
                }),
            }
        }

        match op {
            UnaryOp::Error => Err(Error::Compile {
                error: CompileError::Internal {
                    msg: "unable to type check unary op error",
                    span: span.clone(),
                },
            }),

            UnaryOp::NextState => {
                // Next state access must be a path that resolves to a state variable.  It _may_ be
                // via array indices or tuple fields or even other prime ops.
                drill_down_to_path(self, &rhs_expr_key, span)?;

                let ty = rhs_expr_key.get_ty(self);
                Ok(if !ty.is_unknown() {
                    Inference::Type(ty.clone())
                } else {
                    Inference::Dependant(rhs_expr_key)
                })
            }

            UnaryOp::Neg => {
                // RHS must be an int or real.
                let ty = rhs_expr_key.get_ty(self);
                if !ty.is_unknown() {
                    if ty.is_num() {
                        Ok(Inference::Type(ty.clone()))
                    } else {
                        Err(Error::Compile {
                            error: CompileError::OperatorTypeError {
                                arity: "unary",
                                large_err: Box::new(LargeTypeError::OperatorTypeError {
                                    op: "-",
                                    expected_ty: "numeric".to_string(),
                                    found_ty: self.with_pred(ty).to_string(),
                                    span: span.clone(),
                                    expected_span: None,
                                }),
                            },
                        })
                    }
                } else {
                    Ok(Inference::Dependant(rhs_expr_key))
                }
            }

            UnaryOp::Not => {
                // RHS must be a bool.
                let ty = rhs_expr_key.get_ty(self);
                if !ty.is_unknown() {
                    if ty.is_bool() {
                        Ok(Inference::Type(ty.clone()))
                    } else {
                        Err(Error::Compile {
                            error: CompileError::OperatorTypeError {
                                arity: "unary",
                                large_err: Box::new(LargeTypeError::OperatorTypeError {
                                    op: "!",
                                    expected_ty: "bool".to_string(),
                                    found_ty: self.with_pred(ty).to_string(),
                                    span: span.clone(),
                                    expected_span: None,
                                }),
                            },
                        })
                    }
                } else {
                    Ok(Inference::Dependant(rhs_expr_key))
                }
            }
        }
    }

    fn infer_binary_op(
        &self,
        op: BinaryOp,
        lhs_expr_key: ExprKey,
        rhs_expr_key: ExprKey,
        span: &Span,
    ) -> Result<Inference, Error> {
        let check_numeric_args = |lhs_ty: &Type, rhs_ty: &Type, ty_str: &str| {
            if !lhs_ty.is_num() {
                Err(Error::Compile {
                    error: CompileError::OperatorTypeError {
                        arity: "binary",
                        large_err: Box::new(LargeTypeError::OperatorTypeError {
                            op: op.as_str(),
                            expected_ty: ty_str.to_string(),
                            found_ty: self.with_pred(lhs_ty).to_string(),
                            span: self.expr_key_to_span(lhs_expr_key),
                            expected_span: None,
                        }),
                    },
                })
            } else if !rhs_ty.is_num() {
                Err(Error::Compile {
                    error: CompileError::OperatorTypeError {
                        arity: "binary",
                        large_err: Box::new(LargeTypeError::OperatorTypeError {
                            op: op.as_str(),
                            expected_ty: ty_str.to_string(),
                            found_ty: self.with_pred(rhs_ty).to_string(),
                            span: self.expr_key_to_span(rhs_expr_key),
                            expected_span: None,
                        }),
                    },
                })
            } else if !lhs_ty.eq(self, rhs_ty) {
                // Here we assume the LHS is the 'correct' type.
                Err(Error::Compile {
                    error: CompileError::OperatorTypeError {
                        arity: "binary",
                        large_err: Box::new(LargeTypeError::OperatorTypeError {
                            op: op.as_str(),
                            expected_ty: self.with_pred(lhs_ty).to_string(),
                            found_ty: self.with_pred(rhs_ty).to_string(),
                            span: self.expr_key_to_span(rhs_expr_key),
                            expected_span: Some(self.expr_key_to_span(lhs_expr_key)),
                        }),
                    },
                })
            } else {
                Ok(())
            }
        };

        // Checks if a given `ExprKey` is a path to a state var or a "next state" expression. If
        // not, emit an error.
        let check_state_var_arg = |arg: ExprKey| match arg.try_get(self) {
            Some(Expr::PathByName(name, _))
                if self.states().any(|(_, state)| state.name == *name) =>
            {
                Ok(())
            }
            Some(Expr::PathByKey(var_key, _))
                if self
                    .states()
                    .any(|(_, state)| state.name == var_key.get(self).name) =>
            {
                Ok(())
            }
            Some(Expr::UnaryOp {
                op: UnaryOp::NextState,
                ..
            }) => Ok(()),
            _ => Err(Error::Compile {
                error: CompileError::CompareToNilError {
                    op: op.as_str(),
                    span: self.expr_key_to_span(arg),
                },
            }),
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
                        check_numeric_args(&lhs_ty, rhs_ty, "numeric")
                            .map(|_| Inference::Type(lhs_ty))
                    }

                    BinaryOp::Equal | BinaryOp::NotEqual => {
                        // If either operands is `nil`, then ensure that the other is a state var
                        // or a "next state" expression
                        if lhs_ty.is_nil() && !rhs_ty.is_nil() {
                            check_state_var_arg(rhs_expr_key)?;
                        } else if !lhs_ty.is_nil() && rhs_ty.is_nil() {
                            check_state_var_arg(lhs_expr_key)?;
                        }

                        // We can special case implicit constraints which are injected by variable
                        // initialiser handling.  Each `var a = b` gets a magic `constraint a == b`
                        // which we check for type mismatches elsewhere, and emit a much better
                        // error then.
                        let mut is_init_constraint = false;
                        if !lhs_ty.eq(self, rhs_ty)
                            && op == BinaryOp::Equal
                            && self
                                .var_inits
                                .values()
                                .any(|init_key| *init_key == rhs_expr_key)
                        {
                            is_init_constraint = true;
                        }

                        // Both args must be equatable, which at this stage is any type; binary op
                        // type is bool.
                        if !lhs_ty.eq(self, rhs_ty)
                            && !lhs_ty.is_nil()
                            && !rhs_ty.is_nil()
                            && !is_init_constraint
                        {
                            Err(Error::Compile {
                                error: CompileError::OperatorTypeError {
                                    arity: "binary",
                                    large_err: Box::new(LargeTypeError::OperatorTypeError {
                                        op: op.as_str(),
                                        expected_ty: self.with_pred(lhs_ty).to_string(),
                                        found_ty: self.with_pred(rhs_ty).to_string(),
                                        span: self.expr_key_to_span(rhs_expr_key),
                                        expected_span: Some(self.expr_key_to_span(lhs_expr_key)),
                                    }),
                                },
                            })
                        } else {
                            Ok(Inference::Type(Type::Primitive {
                                kind: PrimitiveKind::Bool,
                                span: span.clone(),
                            }))
                        }
                    }

                    BinaryOp::LessThanOrEqual
                    | BinaryOp::LessThan
                    | BinaryOp::GreaterThanOrEqual
                    | BinaryOp::GreaterThan => {
                        // Both args must be ordinal, i.e., ints, reals; binary op type is bool.
                        check_numeric_args(&lhs_ty, rhs_ty, "numeric").map(|_| {
                            Inference::Type(Type::Primitive {
                                kind: PrimitiveKind::Bool,
                                span: span.clone(),
                            })
                        })
                    }

                    BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                        // Both arg types and binary op type are all bool.
                        if !lhs_ty.is_bool() {
                            Err(Error::Compile {
                                error: CompileError::OperatorTypeError {
                                    arity: "binary",
                                    large_err: Box::new(LargeTypeError::OperatorTypeError {
                                        op: op.as_str(),
                                        expected_ty: "bool".to_string(),
                                        found_ty: self.with_pred(lhs_ty).to_string(),
                                        span: self.expr_key_to_span(lhs_expr_key),
                                        expected_span: Some(span.clone()),
                                    }),
                                },
                            })
                        } else if !rhs_ty.is_bool() {
                            Err(Error::Compile {
                                error: CompileError::OperatorTypeError {
                                    arity: "binary",
                                    large_err: Box::new(LargeTypeError::OperatorTypeError {
                                        op: op.as_str(),
                                        expected_ty: "bool".to_string(),
                                        found_ty: self.with_pred(rhs_ty).to_string(),
                                        span: self.expr_key_to_span(rhs_expr_key),
                                        expected_span: Some(span.clone()),
                                    }),
                                },
                            })
                        } else {
                            Ok(Inference::Type(lhs_ty.clone()))
                        }
                    }
                }
            } else {
                Ok(Inference::Dependant(rhs_expr_key))
            }
        } else {
            Ok(Inference::Dependant(lhs_expr_key))
        }
    }

    fn infer_select_expr(
        &self,
        cond_expr_key: ExprKey,
        then_expr_key: ExprKey,
        else_expr_key: ExprKey,
        span: &Span,
    ) -> Result<Inference, Error> {
        let cond_ty = cond_expr_key.get_ty(self);
        let then_ty = then_expr_key.get_ty(self);
        let else_ty = else_expr_key.get_ty(self);
        if !cond_ty.is_unknown() {
            if !cond_ty.is_bool() {
                Err(Error::Compile {
                    error: CompileError::NonBoolConditional {
                        ty: self.with_pred(cond_ty).to_string(),
                        conditional: "select expression".to_string(),
                        span: self.expr_key_to_span(cond_expr_key),
                    },
                })
            } else if !then_ty.is_unknown() {
                if !else_ty.is_unknown() {
                    if then_ty.eq(self, else_ty) {
                        Ok(Inference::Type(then_ty.clone()))
                    } else {
                        Err(Error::Compile {
                            error: CompileError::SelectBranchesTypeMismatch {
                                large_err: Box::new(LargeTypeError::SelectBranchesTypeMismatch {
                                    then_type: self.with_pred(then_ty).to_string(),
                                    then_span: self.expr_key_to_span(then_expr_key),
                                    else_type: self.with_pred(else_ty).to_string(),
                                    else_span: self.expr_key_to_span(else_expr_key),
                                    span: span.clone(),
                                }),
                            },
                        })
                    }
                } else {
                    Ok(Inference::Dependant(else_expr_key))
                }
            } else {
                Ok(Inference::Dependant(then_expr_key))
            }
        } else {
            Ok(Inference::Dependant(cond_expr_key))
        }
    }

    fn infer_range_expr(
        &self,
        lower_bound_key: ExprKey,
        upper_bound_key: ExprKey,
        span: &Span,
    ) -> Result<Inference, Error> {
        let lb_ty = lower_bound_key.get_ty(self);
        let ub_ty = upper_bound_key.get_ty(self);
        if !lb_ty.is_unknown() {
            if !ub_ty.is_unknown() {
                if !lb_ty.eq(self, ub_ty) {
                    Err(Error::Compile {
                        error: CompileError::RangeTypesMismatch {
                            lb_ty: self.with_pred(lb_ty).to_string(),
                            ub_ty: self.with_pred(ub_ty).to_string(),
                            span: ub_ty.span().clone(),
                        },
                    })
                } else if !lb_ty.is_num() {
                    Err(Error::Compile {
                        error: CompileError::RangeTypesNonNumeric {
                            ty: self.with_pred(lb_ty).to_string(),
                            span: span.clone(),
                        },
                    })
                } else {
                    Ok(Inference::Type(lb_ty.clone()))
                }
            } else {
                Ok(Inference::Dependant(upper_bound_key))
            }
        } else {
            Ok(Inference::Dependant(lower_bound_key))
        }
    }

    fn infer_cast_expr(
        &self,
        value_key: ExprKey,
        to_ty: &Type,
        span: &Span,
    ) -> Result<Inference, Error> {
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
                Err(Error::Compile {
                    error: CompileError::BadCastTo {
                        ty: self.with_pred(to_ty).to_string(),
                        span: span.clone(),
                    },
                })
            } else if (to_ty.is_int()
                && !from_ty.is_int()
                && !from_ty.is_enum(self)
                && !from_ty.is_bool())
                || (to_ty.is_real() && !from_ty.is_int() && !from_ty.is_real())
            {
                // We can only cast to ints from ints, enums or bools and to reals from ints or reals.
                Err(Error::Compile {
                    error: CompileError::BadCastFrom {
                        ty: self.with_pred(from_ty).to_string(),
                        span: span.clone(),
                    },
                })
            } else {
                Ok(Inference::Type(to_ty.clone()))
            }
        } else {
            Ok(Inference::Dependant(value_key))
        }
    }

    fn infer_in_expr(
        &self,
        value_key: ExprKey,
        collection_key: ExprKey,
        span: &Span,
    ) -> Result<Inference, Error> {
        // If the collection is a range, then it must be between ints or reals and the value must
        // match.  If it's an array it can be any type but still the value must match the array
        // element type.
        let value_ty = value_key.get_ty(self);
        let collection_ty = collection_key.get_ty(self);
        if !value_ty.is_unknown() {
            if !collection_ty.is_unknown() {
                if collection_ty.is_num() {
                    if !value_ty.eq(self, collection_ty) {
                        Err(Error::Compile {
                            error: CompileError::InExprTypesMismatch {
                                val_ty: self.with_pred(value_ty).to_string(),
                                range_ty: self.with_pred(collection_ty).to_string(),
                                span: collection_ty.span().clone(),
                            },
                        })
                    } else {
                        Ok(Inference::Type(Type::Primitive {
                            kind: PrimitiveKind::Bool,
                            span: span.clone(),
                        }))
                    }
                } else if let Some(el_ty) = collection_ty.get_array_el_type() {
                    if !value_ty.eq(self, el_ty) {
                        Err(Error::Compile {
                            error: CompileError::InExprTypesArrayMismatch {
                                val_ty: self.with_pred(value_ty).to_string(),
                                el_ty: self.with_pred(el_ty).to_string(),
                                span: el_ty.span().clone(),
                            },
                        })
                    } else {
                        Ok(Inference::Type(Type::Primitive {
                            kind: PrimitiveKind::Bool,
                            span: span.clone(),
                        }))
                    }
                } else {
                    Err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "range ty is not numeric or array?",
                            span: span.clone(),
                        },
                    })
                }
            } else {
                Ok(Inference::Dependant(collection_key))
            }
        } else {
            Ok(Inference::Dependant(value_key))
        }
    }

    fn infer_array_expr(
        &self,
        range_expr_key: ExprKey,
        element_exprs: &[ExprKey],
        span: &Span,
    ) -> Result<Inference, Error> {
        if element_exprs.is_empty() {
            return Err(Error::Compile {
                error: CompileError::EmptyArrayExpression { span: span.clone() },
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
                    if !el_ty.eq(self, el0_ty) {
                        return Err(Error::Compile {
                            error: CompileError::NonHomogeneousArrayElement {
                                expected_ty: self.with_pred(&el0_ty).to_string(),
                                ty: self.with_pred(el_ty).to_string(),
                                span: self.expr_key_to_span(*el_key),
                            },
                        });
                    }
                } else {
                    deps.push(*el_key);
                }
            }

            // Must also type check the range_expr
            if range_expr_key.get_ty(self).is_unknown() {
                deps.push(range_expr_key);
            }

            Ok(if deps.is_empty() {
                Inference::Type(Type::Array {
                    ty: Box::new(el0_ty.clone()),
                    range: Some(range_expr_key),
                    size: Some(element_exprs.len() as i64),
                    span: span.clone(),
                })
            } else {
                Inference::Dependencies(deps)
            })
        } else {
            Ok(Inference::Dependant(*el0))
        }
    }

    fn infer_index_expr(
        &self,
        array_expr_key: ExprKey,
        index_expr_key: ExprKey,
        span: &Span,
    ) -> Result<Inference, Error> {
        let index_ty = index_expr_key.get_ty(self);
        if index_ty.is_unknown() {
            return Ok(Inference::Dependant(index_expr_key));
        }

        let ary_ty = array_expr_key.get_ty(self);
        if ary_ty.is_unknown() {
            return Ok(Inference::Dependant(array_expr_key));
        }

        if let Some(range_expr_key) = ary_ty.get_array_range_expr() {
            // Is this an array?
            let range_ty = range_expr_key.get_ty(self);
            if range_ty.is_unknown() {
                return Ok(Inference::Dependant(range_expr_key));
            }

            if (!index_ty.is_int() && !index_ty.is_enum(self)) || !index_ty.eq(self, range_ty) {
                Err(Error::Compile {
                    error: CompileError::ArrayAccessWithWrongType {
                        found_ty: self.with_pred(index_ty).to_string(),
                        expected_ty: self.with_pred(range_ty).to_string(),
                        span: self.expr_key_to_span(index_expr_key),
                    },
                })
            } else if let Some(ty) = ary_ty.get_array_el_type() {
                Ok(Inference::Type(ty.clone()))
            } else {
                Err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "failed to get array element type \
                          in infer_index_expr()",
                        span: span.clone(),
                    },
                })
            }
        } else if let Some(ty) = ary_ty.get_array_el_type() {
            // Is it an array with an unknown range (probably a const immediate)?
            Ok(Inference::Type(ty.clone()))
        } else if let Some(from_ty) = ary_ty.get_map_ty_from() {
            // Is this a storage map?
            if !from_ty.eq(self, index_ty) {
                Err(Error::Compile {
                    error: CompileError::StorageMapAccessWithWrongType {
                        found_ty: self.with_pred(index_ty).to_string(),
                        expected_ty: self.with_pred(from_ty).to_string(),
                        span: self.expr_key_to_span(index_expr_key),
                    },
                })
            } else if let Some(ty) = ary_ty.get_map_ty_to() {
                Ok(Inference::Type(ty.clone()))
            } else {
                Err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "failed to get array element type \
                          in infer_index_expr()",
                        span: span.clone(),
                    },
                })
            }
        } else {
            Err(Error::Compile {
                error: CompileError::IndexExprNonIndexable {
                    non_indexable_type: self.with_pred(ary_ty).to_string(),
                    span: span.clone(),
                },
            })
        }
    }

    fn infer_tuple_expr(
        &self,
        fields: &[(Option<Ident>, ExprKey)],
        span: &Span,
    ) -> Result<Inference, Error> {
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

        Ok(if deps.is_empty() {
            Inference::Type(Type::Tuple {
                fields: field_tys,
                span: span.clone(),
            })
        } else {
            Inference::Dependencies(deps)
        })
    }

    fn infer_tuple_access_expr(
        &self,
        tuple_expr_key: ExprKey,
        field: &TupleAccess,
        span: &Span,
    ) -> Result<Inference, Error> {
        let tuple_ty = tuple_expr_key.get_ty(self);
        if !tuple_ty.is_unknown() {
            if tuple_ty.is_tuple() {
                match field {
                    TupleAccess::Error => Err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "unable to type check tuple field access error",
                            span: span.clone(),
                        },
                    }),

                    TupleAccess::Index(idx) => {
                        if let Some(field_ty) = tuple_ty.get_tuple_field_type_by_idx(*idx) {
                            Ok(Inference::Type(field_ty.clone()))
                        } else {
                            Err(Error::Compile {
                                error: CompileError::InvalidTupleAccessor {
                                    accessor: idx.to_string(),
                                    tuple_type: self.with_pred(tuple_ty).to_string(),
                                    span: span.clone(),
                                },
                            })
                        }
                    }

                    TupleAccess::Name(name) => {
                        if let Some(field_ty) = tuple_ty.get_tuple_field_type_by_name(name) {
                            Ok(Inference::Type(field_ty.clone()))
                        } else {
                            Err(Error::Compile {
                                error: CompileError::InvalidTupleAccessor {
                                    accessor: name.name.clone(),
                                    tuple_type: self.with_pred(&tuple_ty).to_string(),
                                    span: span.clone(),
                                },
                            })
                        }
                    }
                }
            } else {
                Err(Error::Compile {
                    error: CompileError::TupleAccessNonTuple {
                        non_tuple_type: self.with_pred(tuple_ty).to_string(),
                        span: span.clone(),
                    },
                })
            }
        } else {
            Ok(Inference::Dependant(tuple_expr_key))
        }
    }

    fn infer_generator_expr(
        &self,
        kind: &GeneratorKind,
        ranges: &[(Ident, ExprKey)],
        conditions: &[ExprKey],
        body_expr_key: ExprKey,
        span: &Span,
    ) -> Result<Inference, Error> {
        let mut deps = Vec::new();

        for (_, range_expr_key) in ranges {
            let range_ty = range_expr_key.get_ty(self);
            if !range_ty.is_unknown() {
                if !range_ty.is_int() {
                    return Err(Error::Compile {
                        error: CompileError::NonIntGeneratorRange {
                            ty: self.with_pred(range_ty).to_string(),
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
                    return Err(Error::Compile {
                        error: CompileError::NonBoolGeneratorCondition {
                            ty: self.with_pred(cond_ty).to_string(),
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
                return Err(Error::Compile {
                    error: CompileError::NonBoolGeneratorBody {
                        ty: self.with_pred(body_ty).to_string(),
                        gen_kind: kind.to_string(),
                        span: self.expr_key_to_span(body_expr_key),
                    },
                });
            }
        } else {
            deps.push(body_expr_key);
        }

        Ok(if deps.is_empty() {
            Inference::Type(Type::Primitive {
                kind: PrimitiveKind::Bool,
                span: span.clone(),
            })
        } else {
            Inference::Dependencies(deps)
        })
    }

    // Confirm that all var init exprs and const init exprs match their declared type, if they have
    // one.
    pub(super) fn check_inits(&mut self, handler: &Handler, consts: &FxHashMap<String, Const>) {
        // Confirm types for all the variable initialisers first.
        for (var_key, init_expr_key) in &self.var_inits {
            let var_decl_ty = var_key.get_ty(self);

            // Reporting an error that we're expecting 'Unknown' is not useful.
            if !var_decl_ty.is_unknown() {
                let init_ty = init_expr_key.get_ty(self);

                if !var_decl_ty.eq(self, init_ty) {
                    handler.emit_err(Error::Compile {
                        error: CompileError::InitTypeError {
                            init_kind: "variable",
                            large_err: Box::new(LargeTypeError::InitTypeError {
                                init_kind: "variable",
                                expected_ty: self.with_pred(var_decl_ty).to_string(),
                                found_ty: self.with_pred(init_ty).to_string(),
                                expected_ty_span: var_decl_ty.span().clone(),
                                init_span: self.expr_key_to_span(*init_expr_key),
                            }),
                        },
                    });
                }
            }
        }

        // Now confirm that every const initialiser type matches the const decl type.
        if self.is_root() {
            for Const {
                expr: init_expr_key,
                decl_ty,
                ..
            } in consts.values()
            {
                let init_ty = init_expr_key.get_ty(self);

                // Special case for enum variants -- they'll have an init_ty of `int`.  So we have
                // an error if the types mismatch and they're _not_ an enum/int combo exception.
                if !(init_ty.eq(self, decl_ty) || decl_ty.is_enum(self) && init_ty.is_int()) {
                    handler.emit_err(Error::Compile {
                        error: CompileError::InitTypeError {
                            init_kind: "const",
                            large_err: Box::new(LargeTypeError::InitTypeError {
                                init_kind: "const",
                                expected_ty: self.with_pred(decl_ty).to_string(),
                                found_ty: self.with_pred(init_ty).to_string(),
                                expected_ty_span: decl_ty.span().clone(),
                                init_span: self.expr_key_to_span(*init_expr_key),
                            }),
                        },
                    });
                }
            }
        }
    }

    pub(super) fn expr_key_to_span(&self, expr_key: ExprKey) -> Span {
        expr_key
            .try_get(self)
            .map(|expr| expr.span().clone())
            .unwrap_or_else(empty_span)
    }
}
