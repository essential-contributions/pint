use super::{Expr, ExprKey, Ident, IntermediateIntent, Program, VarKey};
use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler, LargeTypeError},
    expr::{BinaryOp, GeneratorKind, Immediate, TupleAccess, UnaryOp},
    span::{empty_span, Span, Spanned},
    types::{EnumDecl, EphemeralDecl, NewTypeDecl, Path, PrimitiveKind, Type},
};

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
            ii.check_undefined_types(handler);
            ii.lower_newtypes();
            ii.type_check_all_exprs(handler);
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

                Type::Error(_) | Type::Primitive { .. } | Type::Alias { .. } => {}
            }
        }

        // Loop for every known var or state type, or any `as` cast expr, and replace any custom
        // types which match with the type alias.
        for (_, ty) in self.var_types.iter_mut() {
            replace_custom_type(&self.new_types, ty);
        }

        for (_, ty) in self.state_types.iter_mut() {
            replace_custom_type(&self.new_types, ty);
        }

        for (_, expr) in &mut self.exprs {
            if let Expr::Cast { ty, .. } = expr {
                replace_custom_type(&self.new_types, ty.borrow_mut());
            }
        }
    }

    fn check_undefined_types(&mut self, handler: &Handler) {
        let valid_custom_tys: HashSet<&String> = HashSet::from_iter(
            self.enums
                .iter()
                .map(|ed| &ed.name.name)
                .chain(self.new_types.iter().map(|ntd| &ntd.name.name)),
        );

        self.var_types
            .values()
            .chain(self.state_types.values())
            .chain(self.expr_types.values())
            .for_each(|ty| {
                if let Type::Custom { path, span, .. } = ty {
                    if !valid_custom_tys.contains(path) {
                        handler.emit_err(Error::Compile {
                            error: CompileError::UndefinedType { span: span.clone() },
                        });
                    }
                }
            });

        for expr_key in self.exprs() {
            if let Some(Expr::Cast { ty, .. }) = self.exprs.get(expr_key) {
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

    fn type_check_all_exprs(&mut self, handler: &Handler) {
        // Check all the 'root' exprs (constraints and directives) one at a time, gathering errors
        // as we go.  Copying the keys out first to avoid borrowing conflict.
        self.constraints
            .iter()
            .map(|(key, _)| *key)
            .chain(self.states.iter().map(|(_, state)| state.expr))
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

        // Confirm now that all decision variables are typed.
        for (var_key, var) in &self.vars {
            if self.var_types.get(var_key).is_none() {
                if let Some(init_expr_key) = self.var_inits.get(var_key) {
                    if let Some(ty) = self.expr_types.get(*init_expr_key) {
                        self.var_types.insert(var_key, ty.clone());
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

        // Confirm now that all state variables are typed.
        for (state_key, state) in &self.states {
            if let Some(state_ty) = self.state_types.get(state_key) {
                self.expr_types
                    .get(state.expr)
                    .map(|expr_ty| {
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
                    })
                    .unwrap_or_else(|| {
                        handler.emit_err(Error::Compile {
                            error: CompileError::UnknownType {
                                span: state.span.clone(),
                            },
                        });
                    });
            } else {
                self.expr_types
                    .get(state.expr)
                    .map(|expr_ty| {
                        self.state_types.insert(state_key, expr_ty.clone());
                        // State variables of type `Map` are not allowed
                        if expr_ty.is_map() {
                            handler.emit_err(Error::Compile {
                                error: CompileError::StateVarTypeIsMap {
                                    span: state.span.clone(),
                                },
                            });
                        }
                    })
                    .unwrap_or_else(|| {
                        handler.emit_err(Error::Compile {
                            error: CompileError::UnknownType {
                                span: state.span.clone(),
                            },
                        });
                    });
            }
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
            if self.expr_types.contains_key(next_key) {
                queue.pop();
            } else {
                match self.infer_expr_key_type(next_key)? {
                    // Successfully inferred its type.  Save it and pop it from the queue.
                    Inference::Type(ty) => {
                        self.expr_types.insert(next_key, ty);
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
        let expr: &Expr = self.exprs.get(expr_key).ok_or_else(|| {
            if let Some(span) = self.removed_macro_calls.get(expr_key) {
                // This dependant expression was actually a macro call which expanded to just
                // declarations, not another expression.  We can set a specific error in this case.
                Error::Compile {
                    error: CompileError::MacroCallWasNotExpression { span: span.clone() },
                }
            } else {
                Error::Compile {
                    error: CompileError::Internal {
                        msg: "orphaned expr key when type checking",
                        span: empty_span(),
                    },
                }
            }
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
                extern_path,
                name,
                span,
                ..
            } => self.infer_external_storage_access(extern_path, name, span),

            Expr::UnaryOp {
                op,
                expr: op_expr_key,
                span,
            } => self.infer_unary_op(*op, *op_expr_key, span),

            Expr::BinaryOp { op, lhs, rhs, span } => self.infer_binary_op(*op, *lhs, *rhs, span),

            Expr::MacroCall { .. } => Ok(Inference::Ignored),

            Expr::FnCall { name, args, span } => {
                let mut deps = Vec::new();

                args.iter()
                    .filter(|arg_key| self.expr_types.get(**arg_key).is_none())
                    .for_each(|arg_key| deps.push(*arg_key));

                if deps.is_empty() {
                    // For now, this very special case is all we support.
                    if name.as_str().ends_with("::storage_lib::get")
                        || name.as_str().ends_with("::storage_lib::get_extern")
                    {
                        Ok(Inference::Type(Type::Primitive {
                            kind: PrimitiveKind::Int,
                            span: span.clone(),
                        }))
                    } else if name.as_str().ends_with("::context::sender") {
                        Ok(Inference::Type(Type::Primitive {
                            kind: PrimitiveKind::B256,
                            span: span.clone(),
                        }))
                    } else {
                        Err(Error::Compile {
                            error: CompileError::Internal {
                                msg: "unable to check type of FnCall",
                                span: span.clone(),
                            },
                        })
                    }
                } else {
                    Ok(Inference::Dependencies(deps))
                }
            }

            Expr::If {
                condition,
                then_block,
                else_block,
                span,
            } => self.infer_if_expr(*condition, *then_block, *else_block, span),

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
        if let Some(ty) = self.var_types.get(var_key) {
            Ok(Inference::Type(ty.clone()))
        } else if let Some(init_expr_key) = self.var_inits.get(var_key) {
            if let Some(init_expr_ty) = self.expr_types.get(*init_expr_key) {
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
            .vars
            .iter()
            .find_map(|(var_key, var)| (&var.name == path).then_some(var_key))
        {
            // It's a var.
            self.infer_path_by_key(var_key, span)
        } else if let Some((state_key, state)) =
            self.states.iter().find(|(_, state)| (&state.name == path))
        {
            // It's state.
            Ok(if let Some(ty) = self.state_types.get(state_key) {
                Inference::Type(ty.clone())
            } else if let Some(init_expr_ty) = self.expr_types.get(state.expr) {
                Inference::Type(init_expr_ty.clone())
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
        } else {
            // None of the above.  That leaves enums.
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
        extern_path: &Path,
        name: &String,
        span: &Span,
    ) -> Result<Inference, Error> {
        // First, get the `extern` declaration that this access refers to
        let Some(r#extern) = self
            .externs
            .iter()
            .find(|e| e.name.to_string() == *extern_path)
        else {
            return Err(Error::Compile {
                error: CompileError::MissingExtern {
                    name: extern_path.clone(),
                    span: span.clone(),
                },
            });
        };

        // Then, look for the storage variable that this access refers to
        let Some(s_var) = r#extern
            .storage_vars
            .iter()
            .find(|s_var| s_var.name == *name)
        else {
            return Err(Error::Compile {
                error: CompileError::StorageSymbolNotFound {
                    name: name.clone(),
                    span: span.clone(),
                },
            });
        };

        Ok(Inference::Type(s_var.ty.clone()))
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
                if let Some(ty) = self.expr_types.get(rhs_expr_key) {
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
                if let Some(ty) = self.expr_types.get(rhs_expr_key) {
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

            UnaryOp::NextState => Ok(if let Some(ty) = self.expr_types.get(rhs_expr_key) {
                Inference::Type(ty.clone())
            } else {
                Inference::Dependant(rhs_expr_key)
            }),
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

        if let Some(lhs_ty) = self.expr_types.get(lhs_expr_key).cloned() {
            if let Some(rhs_ty) = self.expr_types.get(rhs_expr_key) {
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

    fn infer_if_expr(
        &self,
        cond_expr_key: ExprKey,
        then_expr_key: ExprKey,
        else_expr_key: ExprKey,
        span: &Span,
    ) -> Result<Inference, Error> {
        if let Some(cond_ty) = self.expr_types.get(cond_expr_key) {
            if !cond_ty.is_bool() {
                Err(Error::Compile {
                    error: CompileError::IfCondTypeNotBool(self.expr_key_to_span(cond_expr_key)),
                })
            } else if let Some(then_ty) = self.expr_types.get(then_expr_key) {
                if let Some(else_ty) = self.expr_types.get(else_expr_key) {
                    if then_ty == else_ty {
                        Ok(Inference::Type(then_ty.clone()))
                    } else {
                        Err(Error::Compile {
                            error: CompileError::IfBranchesTypeMismatch {
                                large_err: Box::new(LargeTypeError::IfBranchesTypeMismatch {
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
        if let Some(lb_ty) = self.expr_types.get(lower_bound_key) {
            if let Some(ub_ty) = self.expr_types.get(upper_bound_key) {
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

        if let Some(from_ty) = self.expr_types.get(value_key) {
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
        if let Some(value_ty) = self.expr_types.get(value_key) {
            if let Some(collection_ty) = self.expr_types.get(collection_key) {
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
        if let Some(el0_ty) = self.expr_types.get(*el0) {
            for el_key in elements {
                if let Some(el_ty) = self.expr_types.get(*el_key) {
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
        let index_ty = match self.expr_types.get(index_expr_key) {
            Some(ty) => ty,
            None => return Ok(Inference::Dependant(index_expr_key)),
        };

        let ary_ty = match self.expr_types.get(array_expr_key) {
            Some(ty) => ty,
            None => return Ok(Inference::Dependant(array_expr_key)),
        };

        if let Some(range_expr_key) = ary_ty.get_array_range_expr() {
            // Is this an array?
            let range_ty = match self.expr_types.get(range_expr_key) {
                Some(ty) => ty,
                None => return Ok(Inference::Dependant(range_expr_key)),
            };

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
            if let Some(field_ty) = self.expr_types.get(*field_expr_key) {
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
        if let Some(tuple_ty) = self.expr_types.get(tuple_expr_key) {
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
            if let Some(range_ty) = self.expr_types.get(*range_expr_key) {
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
            if let Some(cond_ty) = self.expr_types.get(*cond_expr_key) {
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

        if let Some(body_ty) = self.expr_types.get(body_expr_key) {
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
        self.exprs
            .get(expr_key)
            .map(|expr| expr.span().clone())
            .unwrap_or_else(empty_span)
    }
}
