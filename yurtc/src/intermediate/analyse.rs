use super::{Expr, ExprKey, Ident, IntermediateIntent, VarKey};

use crate::{
    error::{CompileError, LargeTypeError},
    expr::{BinaryOp, Immediate, TupleAccess, UnaryOp},
    span::{empty_span, Span, Spanned},
    types::{EnumDecl, EphemeralDecl, NewTypeDecl, Path, PrimitiveKind, Type},
};

enum Inference {
    Ignored,
    Type(Type),
    Dependant(ExprKey),
    Dependencies(Vec<ExprKey>),
}

impl IntermediateIntent {
    pub fn type_check(mut self) -> super::Result<Self> {
        self.lower_newtypes()?;
        self.type_check_all_exprs()?;
        Ok(self)
    }

    fn lower_newtypes(&mut self) -> super::Result<()> {
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

        Ok(())
    }

    fn type_check_all_exprs(&mut self) -> super::Result<()> {
        // Attempt to infer all the types of each expr.
        let mut queue = Vec::new();

        // Utility to check for recursion in the queue.  A macro to avoid borrowing self.
        macro_rules! push_to_queue {
            ($dependant_key: ident, $dependency_key: ident) => {
                if queue.contains(&$dependency_key) {
                    Err(CompileError::ExprRecursion {
                        dependant_span: self.expr_key_to_span($dependant_key),
                        dependency_span: self.expr_key_to_span($dependency_key),
                    })
                } else {
                    queue.push($dependency_key);
                    Ok(())
                }
            };
        }

        for expr_key in self.exprs.keys() {
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
                        Inference::Dependant(dep_expr_key) => {
                            push_to_queue!(next_key, dep_expr_key)?
                        }
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
        }

        // Confirm now that all the variables are typed.
        for (var_key, var) in &self.vars {
            if self.var_types.get(var_key).is_none() {
                if let Some(init_expr_key) = self.var_inits.get(var_key) {
                    match self.expr_types.get(*init_expr_key) {
                        Some(ty) => {
                            self.var_types.insert(var_key, ty.clone());
                        }
                        None => {
                            return Err(CompileError::UnknownType {
                                span: var.span.clone(),
                            });
                        }
                    }
                } else {
                    return Err(CompileError::Internal {
                        msg: "untyped variable has no initialiser",
                        span: var.span.clone(),
                    });
                }
            }
        }

        Ok(())
    }

    fn infer_expr_key_type(&self, expr_key: ExprKey) -> super::Result<Inference> {
        let expr: &Expr = self
            .exprs
            .get(expr_key)
            .ok_or_else(|| CompileError::Internal {
                msg: "orphaned expr key when type checking",
                span: empty_span(),
            })?;

        match expr {
            Expr::Error(span) => Err(CompileError::Internal {
                msg: "unable to type check from error expression",
                span: span.clone(),
            }),

            Expr::Immediate { value, span } => Ok(Inference::Type(Self::get_immediate_type(
                value,
                span.clone(),
            ))),

            Expr::PathByKey(var_key, span) => self.infer_path_by_key(*var_key, span),

            Expr::PathByName(path, span) => self.infer_path_by_name(path, span),

            Expr::UnaryOp {
                op,
                expr: op_expr_key,
                span,
            } => self.infer_unary_op(*op, *op_expr_key, span),

            Expr::BinaryOp { op, lhs, rhs, span } => self.infer_binary_op(*op, *lhs, *rhs, span),

            Expr::MacroCall { .. } => Ok(Inference::Ignored),

            Expr::FnCall { name, span, .. } => {
                // For now, this very special case is all we support.
                if name.as_str() == "::storage::get" {
                    Ok(Inference::Type(Type::Primitive {
                        kind: PrimitiveKind::Int,
                        span: span.clone(),
                    }))
                } else {
                    Err(CompileError::Internal {
                        msg: "unable to check type of FnCall",
                        span: span.clone(),
                    })
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

            Expr::ArrayElementAccess { array, index, span } => {
                self.infer_array_access_expr(*array, *index, span)
            }

            Expr::Tuple { fields, span } => self.infer_tuple_expr(fields, span),

            Expr::TupleFieldAccess { tuple, field, span } => {
                self.infer_tuple_access_expr(*tuple, field, span)
            }

            Expr::Cast { ty, .. } => Ok(Inference::Type(*ty.clone())),

            Expr::In { span, .. } => Ok(Inference::Type(Type::Primitive {
                kind: PrimitiveKind::Bool,
                span: span.clone(),
            })),

            Expr::Range { span, .. } => Ok(Inference::Type(Type::Primitive {
                kind: PrimitiveKind::Int,
                span: span.clone(),
            })),

            Expr::ForAll { span, .. } => Ok(Inference::Type(Type::Primitive {
                kind: PrimitiveKind::Bool,
                span: span.clone(),
            })),
        }
    }

    fn get_immediate_type(imm: &Immediate, span: Span) -> Type {
        match imm {
            Immediate::Real(_) => Type::Primitive {
                kind: PrimitiveKind::Real,
                span: span.clone(),
            },
            Immediate::Int(_) | Immediate::BigInt(_) => Type::Primitive {
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
        }
    }

    fn infer_path_by_key(&self, var_key: VarKey, span: &Span) -> super::Result<Inference> {
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
            Err(CompileError::Internal {
                msg: "untyped variable doesn't have initialiser",
                span: span.clone(),
            })
        }
    }

    fn infer_path_by_name(&self, path: &Path, span: &Span) -> super::Result<Inference> {
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

    fn infer_enum_variant_by_name(&self, path: &Path, span: &Span) -> super::Result<Inference> {
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
                        match self.infer_enum_variant_by_name(&new_path, span) {
                            ty @ Ok(_) => {
                                // We found an enum variant.
                                return ty;
                            }
                            Err(_) => {}
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
                variants
                    .iter()
                    .any(|variant| {
                        if path[2..] == variant.name {
                            err_potential_enums.push(enum_name.name.clone());
                        }

                        let mut full_variant = enum_name.name.clone();
                        full_variant.push_str("::");
                        full_variant.push_str(&variant.name);

                        &full_variant == path
                    })
                    .then(|| Type::Custom {
                        path: enum_name.name.clone(),
                        span: span.clone(),
                    })
            },
        ) {
            Ok(Inference::Type(ty.clone()))
        } else {
            Err(CompileError::SymbolNotFound {
                name: path.clone(),
                span: span.clone(),
                enum_names: err_potential_enums,
            })
        }
    }

    fn infer_unary_op(
        &self,
        op: UnaryOp,
        rhs_expr_key: ExprKey,
        span: &Span,
    ) -> super::Result<Inference> {
        match op {
            UnaryOp::Error => Err(CompileError::Internal {
                msg: "unable to type check unary op error",
                span: span.clone(),
            }),

            UnaryOp::Neg => {
                // RHS must be an int or real.
                if let Some(ty) = self.expr_types.get(rhs_expr_key) {
                    if ty.is_num() {
                        Ok(Inference::Type(ty.clone()))
                    } else {
                        Err(CompileError::OperatorTypeError {
                            arity: "unary",
                            large_err: Box::new(LargeTypeError::OperatorTypeError {
                                op: "-",
                                expected_ty: "numeric".to_string(),
                                found_ty: self.with_ii(ty).to_string(),
                                span: span.clone(),
                                expected_span: None,
                            }),
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
                        Err(CompileError::OperatorTypeError {
                            arity: "unary",
                            large_err: Box::new(LargeTypeError::OperatorTypeError {
                                op: "!",
                                expected_ty: "bool".to_string(),
                                found_ty: self.with_ii(ty).to_string(),
                                span: span.clone(),
                                expected_span: None,
                            }),
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
    ) -> super::Result<Inference> {
        let check_numeric_args = |lhs_ty: &Type, rhs_ty: &Type, ty_str: &str| {
            if !lhs_ty.is_num() {
                Err(CompileError::OperatorTypeError {
                    arity: "binary",
                    large_err: Box::new(LargeTypeError::OperatorTypeError {
                        op: op.as_str(),
                        expected_ty: ty_str.to_string(),
                        found_ty: self.with_ii(lhs_ty).to_string(),
                        span: self.expr_key_to_span(lhs_expr_key),
                        expected_span: None,
                    }),
                })
            } else if !rhs_ty.is_num() {
                Err(CompileError::OperatorTypeError {
                    arity: "binary",
                    large_err: Box::new(LargeTypeError::OperatorTypeError {
                        op: op.as_str(),
                        expected_ty: ty_str.to_string(),
                        found_ty: self.with_ii(rhs_ty).to_string(),
                        span: self.expr_key_to_span(rhs_expr_key),
                        expected_span: None,
                    }),
                })
            } else if lhs_ty != rhs_ty {
                // Here we assume the LHS is the 'correct' type.
                Err(CompileError::OperatorTypeError {
                    arity: "binary",
                    large_err: Box::new(LargeTypeError::OperatorTypeError {
                        op: op.as_str(),
                        expected_ty: self.with_ii(lhs_ty).to_string(),
                        found_ty: self.with_ii(rhs_ty).to_string(),
                        span: self.expr_key_to_span(rhs_expr_key),
                        expected_span: Some(self.expr_key_to_span(lhs_expr_key)),
                    }),
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
                            Err(CompileError::OperatorTypeError {
                                arity: "binary",
                                large_err: Box::new(LargeTypeError::OperatorTypeError {
                                    op: op.as_str(),
                                    expected_ty: self.with_ii(lhs_ty).to_string(),
                                    found_ty: self.with_ii(rhs_ty).to_string(),
                                    span: self.expr_key_to_span(rhs_expr_key),
                                    expected_span: Some(self.expr_key_to_span(lhs_expr_key)),
                                }),
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
                            Err(CompileError::OperatorTypeError {
                                arity: "binary",
                                large_err: Box::new(LargeTypeError::OperatorTypeError {
                                    op: op.as_str(),
                                    expected_ty: "bool".to_string(),
                                    found_ty: self.with_ii(lhs_ty).to_string(),
                                    span: self.expr_key_to_span(lhs_expr_key),
                                    expected_span: Some(span.clone()),
                                }),
                            })
                        } else if !rhs_ty.is_bool() {
                            Err(CompileError::OperatorTypeError {
                                arity: "binary",
                                large_err: Box::new(LargeTypeError::OperatorTypeError {
                                    op: op.as_str(),
                                    expected_ty: "bool".to_string(),
                                    found_ty: self.with_ii(rhs_ty).to_string(),
                                    span: self.expr_key_to_span(rhs_expr_key),
                                    expected_span: Some(span.clone()),
                                }),
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
    ) -> super::Result<Inference> {
        if let Some(cond_ty) = self.expr_types.get(cond_expr_key) {
            if !cond_ty.is_bool() {
                Err(CompileError::IfCondTypeNotBool(
                    self.expr_key_to_span(cond_expr_key),
                ))
            } else if let Some(then_ty) = self.expr_types.get(then_expr_key) {
                if let Some(else_ty) = self.expr_types.get(else_expr_key) {
                    if then_ty == else_ty {
                        Ok(Inference::Type(then_ty.clone()))
                    } else {
                        Err(CompileError::IfBranchesTypeMismatch {
                            large_err: Box::new(LargeTypeError::IfBranchesTypeMismatch {
                                then_type: self.with_ii(then_ty).to_string(),
                                then_span: self.expr_key_to_span(then_expr_key),
                                else_type: self.with_ii(else_ty).to_string(),
                                else_span: self.expr_key_to_span(else_expr_key),
                                span: span.clone(),
                            }),
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

    fn infer_array_expr(
        &self,
        range_expr_key: ExprKey,
        element_exprs: &[ExprKey],
        span: &Span,
    ) -> super::Result<Inference> {
        if element_exprs.is_empty() {
            return Err(CompileError::EmptyArrayExpression { span: span.clone() });
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
                        return Err(CompileError::NonHomogeneousArrayElement {
                            expected_ty: self.with_ii(&el0_ty).to_string(),
                            ty: self.with_ii(el_ty).to_string(),
                            span: self.expr_key_to_span(*el_key),
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
                    span: span.clone(),
                })
            } else {
                Inference::Dependencies(deps)
            })
        } else {
            Ok(Inference::Dependant(*el0))
        }
    }

    fn infer_array_access_expr(
        &self,
        array_expr_key: ExprKey,
        index_expr_key: ExprKey,
        span: &Span,
    ) -> super::Result<Inference> {
        if let Some(index_ty) = self.expr_types.get(index_expr_key) {
            if !index_ty.is_int() {
                Err(CompileError::ArrayAccessWithNonInt {
                    found_ty: self.with_ii(index_ty).to_string(),
                    span: self.expr_key_to_span(index_expr_key),
                })
            } else if let Some(ary_ty) = self.expr_types.get(array_expr_key) {
                if let Some(ty) = ary_ty.get_array_el_type() {
                    Ok(Inference::Type(ty.clone()))
                } else {
                    Err(CompileError::ArrayAccessNonArray {
                        non_array_type: self.with_ii(ary_ty).to_string(),
                        span: span.clone(),
                    })
                }
            } else {
                Ok(Inference::Dependant(array_expr_key))
            }
        } else {
            Ok(Inference::Dependant(index_expr_key))
        }
    }

    fn infer_tuple_expr(
        &self,
        fields: &[(Option<Ident>, ExprKey)],
        span: &Span,
    ) -> super::Result<Inference> {
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
    ) -> super::Result<Inference> {
        if let Some(tuple_ty) = self.expr_types.get(tuple_expr_key) {
            if tuple_ty.is_tuple() {
                match field {
                    TupleAccess::Error => Err(CompileError::Internal {
                        msg: "unable to type check tuple field access error",
                        span: span.clone(),
                    }),

                    TupleAccess::Index(idx) => {
                        if let Some(field_ty) = tuple_ty.get_tuple_field_type_by_idx(*idx) {
                            Ok(Inference::Type(field_ty.clone()))
                        } else {
                            Err(CompileError::InvalidTupleAccessor {
                                accessor: idx.to_string(),
                                tuple_type: self.with_ii(tuple_ty).to_string(),
                                span: span.clone(),
                            })
                        }
                    }

                    TupleAccess::Name(name) => {
                        if let Some(field_ty) = tuple_ty.get_tuple_field_type_by_name(name) {
                            Ok(Inference::Type(field_ty.clone()))
                        } else {
                            Err(CompileError::InvalidTupleAccessor {
                                accessor: name.name.clone(),
                                tuple_type: self.with_ii(&tuple_ty).to_string(),
                                span: span.clone(),
                            })
                        }
                    }
                }
            } else {
                Err(CompileError::TupleAccessNonTuple {
                    non_tuple_type: self.with_ii(tuple_ty).to_string(),
                    span: span.clone(),
                })
            }
        } else {
            Ok(Inference::Dependant(tuple_expr_key))
        }
    }

    fn expr_key_to_span(&self, expr_key: ExprKey) -> Span {
        self.exprs
            .get(expr_key)
            .map(|expr| expr.span().clone())
            .unwrap_or_else(empty_span)
    }
}
