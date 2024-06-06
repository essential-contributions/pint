mod intrinsics;

use super::{Expr, ExprKey, Ident, IntermediateIntent, Program, VarKey};
use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler, LargeTypeError},
    expr::{BinaryOp, GeneratorKind, Immediate, TupleAccess, UnaryOp},
    intermediate::{BlockStatement, ConstraintDecl, IfDecl, IntentInstance, InterfaceInstance},
    span::{empty_span, Span, Spanned},
    types::{EnumDecl, EphemeralDecl, NewTypeDecl, Path, PrimitiveKind, Type},
};
use fxhash::FxHashMap;
use std::collections::HashSet;

enum Inference {
    Ignored,
    Type(Type),
    Dependant(ExprKey),
    Dependencies(Vec<ExprKey>),
}

impl Program {
    pub fn type_check(mut self, handler: &Handler) -> Result<Self, ErrorEmitted> {
        self = handler.scope(|handler| self.check_program_kind(handler))?;

        for ii in self.iis.values_mut() {
            for expr_key in ii.exprs() {
                if let Some(span) = ii.removed_macro_calls.get(expr_key) {
                    // This expression was actually a macro call which expanded to just declarations,
                    // not another expression. We can set a specific error in this case.
                    handler.emit_err(Error::Compile {
                        error: CompileError::MacroCallWasNotExpression { span: span.clone() },
                    });
                }
            }

            if handler.has_errors() {
                return Err(handler.cancel());
            }

            ii.check_undefined_types(handler);
            ii.lower_newtypes();
            ii.type_check_all_exprs(handler);
            ii.check_constraint_types(handler);
        }

        if handler.has_errors() {
            Err(handler.cancel())
        } else {
            Ok(self)
        }
    }
}

impl IntermediateIntent {
    fn lower_newtypes(&mut self) {
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

                Type::Error(_) | Type::Unknown(_) | Type::Primitive { .. } | Type::Alias { .. } => {
                }
            }
        }

        // Loop for every known var or state type, or any `as` cast expr, and replace any custom
        // types which match with the type alias.
        self.vars
            .update_types(|_, ty| replace_custom_type(&self.new_types, ty));

        self.states
            .update_types(|_, ty| replace_custom_type(&self.new_types, ty));

        self.exprs.update_exprs(|_, expr| {
            if let Expr::Cast { ty, .. } = expr {
                replace_custom_type(&self.new_types, ty.borrow_mut());
            }
        });
    }

    fn check_undefined_types(&mut self, handler: &Handler) {
        let valid_custom_tys: HashSet<&String> = HashSet::from_iter(
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

    fn check_constraint_types(&mut self, handler: &Handler) {
        // After all expression types are inferred, then all constraint expressions must be of type bool
        self.constraints.iter().for_each(|constraint_decl| {
            let expr_type = constraint_decl.expr.get_ty(self);
            if !expr_type.is_bool() {
                handler.emit_err(Error::Compile {
                    error: CompileError::ConstraintExpressionTypeError {
                        large_err: Box::new(LargeTypeError::ConstraintExpressionTypeError {
                            expected_ty: self
                                .with_ii(Type::Primitive {
                                    kind: PrimitiveKind::Bool,
                                    span: empty_span(),
                                })
                                .to_string(),
                            found_ty: self.with_ii(expr_type).to_string(),
                            span: constraint_decl.span.clone(),
                            expected_span: Some(constraint_decl.span.clone()),
                        }),
                    },
                });
            }
        })
    }

    fn type_check_all_exprs(&mut self, handler: &Handler) {
        // Type check all interface instance declarations
        self.interface_instances.clone().into_iter().for_each(
            |InterfaceInstance {
                 interface,
                 address,
                 span,
                 ..
             }| {
                if !self
                    .interfaces
                    .iter()
                    .any(|e| e.name.to_string() == *interface)
                {
                    handler.emit_err(Error::Compile {
                        error: CompileError::MissingInterface {
                            name: interface.clone(),
                            span: span.clone(),
                        },
                    });
                }

                match self.type_check_next_expr(address) {
                    Ok(()) => {
                        let ty = address.get_ty(self);
                        if !ty.is_b256() {
                            handler.emit_err(Error::Compile {
                                error: CompileError::AddressExpressionTypeError {
                                    large_err: Box::new(
                                        LargeTypeError::AddressExpressionTypeError {
                                            expected_ty: self
                                                .with_ii(Type::Primitive {
                                                    kind: PrimitiveKind::B256,
                                                    span: empty_span(),
                                                })
                                                .to_string(),
                                            found_ty: self.with_ii(ty).to_string(),
                                            span: self.expr_key_to_span(address),
                                            expected_span: Some(self.expr_key_to_span(address)),
                                        },
                                    ),
                                },
                            });
                        }
                    }
                    Err(err) => {
                        handler.emit_err(err);
                    }
                }
            },
        );

        // Type check all interface instance declarations
        self.intent_instances.clone().into_iter().for_each(
            |IntentInstance {
                 interface_instance,
                 intent,
                 address,
                 span,
                 ..
             }| {
                // Make sure that an appropriate interface instance exists and an appropriate
                // intent interface exists
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
                        if !interface
                            .intent_interfaces
                            .iter()
                            .any(|e| e.name.to_string() == *intent.to_string())
                        {
                            handler.emit_err(Error::Compile {
                                error: CompileError::MissingIntentInterface {
                                    intent_name: intent.name.to_string(),
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
                };

                // Type check the address field
                match self.type_check_next_expr(address) {
                    Ok(()) => {
                        let ty = address.get_ty(self);
                        if !ty.is_b256() {
                            handler.emit_err(Error::Compile {
                                error: CompileError::AddressExpressionTypeError {
                                    large_err: Box::new(
                                        LargeTypeError::AddressExpressionTypeError {
                                            expected_ty: self
                                                .with_ii(Type::Primitive {
                                                    kind: PrimitiveKind::B256,
                                                    span: empty_span(),
                                                })
                                                .to_string(),
                                            found_ty: self.with_ii(ty).to_string(),
                                            span: self.expr_key_to_span(address),
                                            expected_span: Some(self.expr_key_to_span(address)),
                                        },
                                    ),
                                },
                            });
                        }
                    }
                    Err(err) => {
                        handler.emit_err(err);
                    }
                }
            },
        );

        // Check all the 'root' exprs (constraints, state initi exprs, and directives) one at a
        // time, gathering errors as we go. Copying the keys out first to avoid borrowing
        // conflict.
        self.constraints
            .iter()
            .map(|ConstraintDecl { expr: key, .. }| *key)
            .chain(self.states().map(|(_, state)| state.expr))
            .chain(
                self.directives
                    .iter()
                    .filter_map(|(solve_func, _)| solve_func.get_expr().cloned()),
            )
            .collect::<Vec<_>>()
            .into_iter()
            .for_each(|expr_key| {
                if let Err(err) = self.type_check_next_expr(expr_key) {
                    handler.emit_err(err);
                }
            });

        // Now check all if declarations
        self.if_decls
            .clone()
            .iter()
            .for_each(|if_decl| self.type_check_if_decl(if_decl, handler));

        // Confirm now that all decision variables are typed.
        let mut var_key_to_new_type = FxHashMap::default();
        for (var_key, var) in self.vars() {
            let ty = var_key.get_ty(self);
            if ty.is_unknown() {
                if let Some(init_expr_key) = self.var_inits.get(var_key) {
                    let ty = init_expr_key.get_ty(self);
                    if !ty.is_unknown() {
                        if var.is_pub && !(ty.is_bool() || ty.is_b256() || ty.is_int()) {
                            handler.emit_err(Error::Compile {
                                error: CompileError::Internal {
                                    msg: "only `bool`, b256`, and `int` pub vars are currently supported",
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
            } else if var.is_pub && !(ty.is_bool() || ty.is_b256() || ty.is_int()) {
                handler.emit_err(Error::Compile {
                    error: CompileError::Internal {
                        msg: "only `bool`, b256`, and `int` pub vars are currently supported",
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
                    if state_ty != expr_ty {
                        handler.emit_err(Error::Compile {
                            error: CompileError::StateVarInitTypeError {
                                large_err: Box::new(LargeTypeError::StateVarInitTypeError {
                                    expected_ty: self.with_ii(state_ty).to_string(),
                                    found_ty: self.with_ii(expr_ty).to_string(),
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
        let mut checked_range_exprs = HashSet::new();
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
            if let Err(err) = self.type_check_next_expr(*range_expr) {
                handler.emit_err(err);
            } else if !(range_expr.get_ty(self).is_int()
                || range_expr.get_ty(self).is_enum()
                || checked_range_exprs.contains(range_expr))
            {
                handler.emit_err(Error::Compile {
                    error: CompileError::InvalidArrayRangeType {
                        found_ty: self.with_ii(range_expr.get_ty(self)).to_string(),
                        span: self.expr_key_to_span(*range_expr),
                    },
                });
                // Make sure to not collect too many duplicate errors
                checked_range_exprs.insert(range_expr);
            }
        }
    }

    // Type check an if statement and all of its sub-statements including other ifs. This is a
    // recursive function.
    fn type_check_if_decl(&mut self, if_decl: &IfDecl, handler: &Handler) {
        let IfDecl {
            condition,
            then_block,
            else_block,
            ..
        } = if_decl;

        // Make sure the condition is a `bool`
        if let Err(err) = self.type_check_next_expr(*condition) {
            handler.emit_err(err);
        } else {
            let cond_ty = condition.get_ty(self);
            if !cond_ty.is_bool() {
                handler.emit_err(Error::Compile {
                    error: CompileError::NonBoolConditional {
                        ty: self.with_ii(cond_ty).to_string(),
                        conditional: "`if` statement".to_string(),
                        span: self.expr_key_to_span(*condition),
                    },
                });
            }
        }

        // Type check each block statement in the "then" block
        then_block.iter().for_each(|block_statement| {
            self.type_check_block_statement(block_statement, handler);
        });

        // Type check each block statement in the "else" block, if available
        else_block.iter().flatten().for_each(|block_statement| {
            self.type_check_block_statement(block_statement, handler);
        });
    }

    fn type_check_block_statement(&mut self, block_statement: &BlockStatement, handler: &Handler) {
        match block_statement {
            BlockStatement::Constraint(ConstraintDecl { expr, .. }) => {
                if let Err(err) = self.type_check_next_expr(*expr) {
                    handler.emit_err(err);
                }
            }
            BlockStatement::If(if_decl) => self.type_check_if_decl(if_decl, handler),
        }
    }

    fn type_check_next_expr(&mut self, expr_key: ExprKey) -> Result<(), Error> {
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
                match self.infer_expr_key_type(next_key)? {
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

    fn infer_expr_key_type(&self, expr_key: ExprKey) -> Result<Inference, Error> {
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

            Expr::Immediate { value, span } => Ok(Inference::Type(Self::get_immediate_type(
                value,
                span.clone(),
            ))),

            Expr::PathByKey(var_key, span) => self.infer_path_by_key(*var_key, span),

            Expr::PathByName(path, span) => self.infer_path_by_name(path, span),

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

            Expr::Array {
                elements,
                range_expr,
                span,
            } => self.infer_array_expr(*range_expr, elements, span),

            Expr::Index { expr, index, span } => self.infer_index_expr(*expr, *index, span),

            Expr::Tuple { fields, span } => self.infer_tuple_expr(fields, span),

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

    fn get_immediate_type(imm: &Immediate, span: Span) -> Type {
        match imm {
            Immediate::Real(_) => Type::Primitive {
                kind: PrimitiveKind::Real,
                span: span.clone(),
            },
            Immediate::Int(_) => Type::Primitive {
                kind: PrimitiveKind::Int,
                span: span.clone(),
            },
            Immediate::Bool(_) => Type::Primitive {
                kind: PrimitiveKind::Bool,
                span: span.clone(),
            },
            Immediate::String(_) => Type::Primitive {
                kind: PrimitiveKind::String,
                span: span.clone(),
            },
            Immediate::B256(_) => Type::Primitive {
                kind: PrimitiveKind::B256,
                span: span.clone(),
            },
            Immediate::Error => Type::Error(span.clone()),
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

    fn infer_path_by_name(&self, path: &Path, span: &Span) -> Result<Inference, Error> {
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
            Some(storage) => match storage.0.iter().find(|s_var| s_var.name == *name) {
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
            Some(storage) => match storage.0.iter().find(|s_var| s_var.name == *name) {
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
        // Look through all available intent instances and their corresponding interfaces for a var
        // with the same path as `path`
        for IntentInstance {
            name,
            interface_instance,
            intent,
            ..
        } in &self.intent_instances
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
                    if let Some(intent) = interface
                        .intent_interfaces
                        .iter()
                        .find(|e| e.name.to_string() == *intent.to_string())
                    {
                        for var in &intent.vars {
                            if name.to_string() + "::" + &var.name == *path {
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

    fn infer_enum_variant_by_name(&self, path: &Path, span: &Span) -> Result<Inference, Error> {
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
        match op {
            UnaryOp::Error => Err(Error::Compile {
                error: CompileError::Internal {
                    msg: "unable to type check unary op error",
                    span: span.clone(),
                },
            }),

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
                                    found_ty: self.with_ii(ty).to_string(),
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
                                    found_ty: self.with_ii(ty).to_string(),
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

            UnaryOp::NextState => {
                // Next state access must be a path that resolves to a state variable.
                match rhs_expr_key.try_get(self) {
                    Some(Expr::PathByName(name, span)) => {
                        if !self.states().any(|(_, state)| state.name == *name) {
                            Err(Error::Compile {
                                error: CompileError::InvalidNextStateAccess { span: span.clone() },
                            })?
                        }
                    }
                    Some(Expr::PathByKey(var_key, span)) => {
                        if !self
                            .states()
                            .any(|(_, state)| state.name == var_key.get(self).name)
                        {
                            Err(Error::Compile {
                                error: CompileError::InvalidNextStateAccess { span: span.clone() },
                            })?
                        }
                    }
                    _ => Err(Error::Compile {
                        error: CompileError::InvalidNextStateAccess { span: span.clone() },
                    })?,
                }

                let ty = rhs_expr_key.get_ty(self);
                if !ty.is_unknown() {
                    Ok(Inference::Type(ty.clone()))
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
                            found_ty: self.with_ii(lhs_ty).to_string(),
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
                            found_ty: self.with_ii(rhs_ty).to_string(),
                            span: self.expr_key_to_span(rhs_expr_key),
                            expected_span: None,
                        }),
                    },
                })
            } else if lhs_ty != rhs_ty {
                // Here we assume the LHS is the 'correct' type.
                Err(Error::Compile {
                    error: CompileError::OperatorTypeError {
                        arity: "binary",
                        large_err: Box::new(LargeTypeError::OperatorTypeError {
                            op: op.as_str(),
                            expected_ty: self.with_ii(lhs_ty).to_string(),
                            found_ty: self.with_ii(rhs_ty).to_string(),
                            span: self.expr_key_to_span(rhs_expr_key),
                            expected_span: Some(self.expr_key_to_span(lhs_expr_key)),
                        }),
                    },
                })
            } else {
                Ok(())
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
                        check_numeric_args(&lhs_ty, rhs_ty, "numeric")
                            .map(|_| Inference::Type(lhs_ty))
                    }

                    BinaryOp::Equal | BinaryOp::NotEqual => {
                        // Both args must be equatable, which at this stage is any type; binary op
                        // type is bool.
                        if &lhs_ty != rhs_ty {
                            Err(Error::Compile {
                                error: CompileError::OperatorTypeError {
                                    arity: "binary",
                                    large_err: Box::new(LargeTypeError::OperatorTypeError {
                                        op: op.as_str(),
                                        expected_ty: self.with_ii(lhs_ty).to_string(),
                                        found_ty: self.with_ii(rhs_ty).to_string(),
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
                                        found_ty: self.with_ii(lhs_ty).to_string(),
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
                                        found_ty: self.with_ii(rhs_ty).to_string(),
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
                        ty: self.with_ii(cond_ty).to_string(),
                        conditional: "select expression".to_string(),
                        span: self.expr_key_to_span(cond_expr_key),
                    },
                })
            } else if !then_ty.is_unknown() {
                if !else_ty.is_unknown() {
                    if then_ty == else_ty {
                        Ok(Inference::Type(then_ty.clone()))
                    } else {
                        Err(Error::Compile {
                            error: CompileError::SelectBranchesTypeMismatch {
                                large_err: Box::new(LargeTypeError::SelectBranchesTypeMismatch {
                                    then_type: self.with_ii(then_ty).to_string(),
                                    then_span: self.expr_key_to_span(then_expr_key),
                                    else_type: self.with_ii(else_ty).to_string(),
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
                if lb_ty != ub_ty {
                    Err(Error::Compile {
                        error: CompileError::RangeTypesMismatch {
                            lb_ty: self.with_ii(lb_ty).to_string(),
                            ub_ty: self.with_ii(ub_ty).to_string(),
                            span: ub_ty.span().clone(),
                        },
                    })
                } else if !lb_ty.is_num() {
                    Err(Error::Compile {
                        error: CompileError::RangeTypesNonNumeric {
                            ty: self.with_ii(lb_ty).to_string(),
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
                        ty: self.with_ii(to_ty).to_string(),
                        span: span.clone(),
                    },
                })
            } else if (to_ty.is_int()
                && !from_ty.is_int()
                && !from_ty.is_enum()
                && !from_ty.is_bool())
                || (to_ty.is_real() && !from_ty.is_int() && !from_ty.is_real())
            {
                // We can only cast to ints from ints, enums or bools and to reals from ints or reals.
                Err(Error::Compile {
                    error: CompileError::BadCastFrom {
                        ty: self.with_ii(from_ty).to_string(),
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
                    if value_ty != collection_ty {
                        Err(Error::Compile {
                            error: CompileError::InExprTypesMismatch {
                                val_ty: self.with_ii(value_ty).to_string(),
                                range_ty: self.with_ii(collection_ty).to_string(),
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
                    if value_ty != el_ty {
                        Err(Error::Compile {
                            error: CompileError::InExprTypesArrayMismatch {
                                val_ty: self.with_ii(value_ty).to_string(),
                                el_ty: self.with_ii(el_ty).to_string(),
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
                    if el_ty != el0_ty {
                        return Err(Error::Compile {
                            error: CompileError::NonHomogeneousArrayElement {
                                expected_ty: self.with_ii(&el0_ty).to_string(),
                                ty: self.with_ii(el_ty).to_string(),
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
                    range: range_expr_key,
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

            if (!index_ty.is_int() && !index_ty.is_enum()) || index_ty != range_ty {
                Err(Error::Compile {
                    error: CompileError::ArrayAccessWithWrongType {
                        found_ty: self.with_ii(index_ty).to_string(),
                        expected_ty: self.with_ii(range_ty).to_string(),
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
        } else if let Some(from_ty) = ary_ty.get_map_ty_from() {
            // Is this a storage map?
            if from_ty != index_ty {
                Err(Error::Compile {
                    error: CompileError::StorageMapAccessWithWrongType {
                        found_ty: self.with_ii(index_ty).to_string(),
                        expected_ty: self.with_ii(from_ty).to_string(),
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
                    non_indexable_type: self.with_ii(ary_ty).to_string(),
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
                                    tuple_type: self.with_ii(tuple_ty).to_string(),
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
                                    tuple_type: self.with_ii(&tuple_ty).to_string(),
                                    span: span.clone(),
                                },
                            })
                        }
                    }
                }
            } else {
                Err(Error::Compile {
                    error: CompileError::TupleAccessNonTuple {
                        non_tuple_type: self.with_ii(tuple_ty).to_string(),
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
                            ty: self.with_ii(range_ty).to_string(),
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
                            ty: self.with_ii(cond_ty).to_string(),
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
                        ty: self.with_ii(body_ty).to_string(),
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

    fn expr_key_to_span(&self, expr_key: ExprKey) -> Span {
        expr_key
            .try_get(self)
            .map(|expr| expr.span().clone())
            .unwrap_or_else(empty_span)
    }
}
