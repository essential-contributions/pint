use super::Inference;
use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler, LargeTypeError},
    expr::{
        BinaryOp, ExternalIntrinsic, GeneratorKind, Immediate, IntrinsicKind, MatchBranch,
        MatchElse, TupleAccess, UnaryOp,
    },
    predicate::{Contract, Expr, ExprKey, Ident, PredKey, Predicate},
    span::{empty_span, Span, Spanned},
    types::{b256, r#bool, PrimitiveKind, Type, UnionVariant},
    warning::Warning,
};
use fxhash::FxHashSet;

impl Contract {
    pub(super) fn type_check_single_expr(
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
            handler.emit_internal_err(
                "orphaned expr key when type checking".to_string(),
                empty_span(),
            )
        })?;

        match expr {
            Expr::Error(span) => Err(handler.emit_internal_err(
                "unable to type check from error expression".to_string(),
                span.clone(),
            )),

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

            Expr::LocalStorageAccess { name, span, .. } => {
                Ok(self.infer_local_storage_access(handler, name, span))
            }

            Expr::ExternalStorageAccess {
                interface,
                address,
                name,
                span,
                ..
            } => Ok(self.infer_external_storage_access(handler, interface, *address, name, span)),

            Expr::UnaryOp {
                op,
                expr: op_expr_key,
                span,
            } => Ok(self.infer_unary_op(handler, pred, *op, *op_expr_key, span)),

            Expr::BinaryOp { op, lhs, rhs, span } => {
                Ok(self.infer_binary_op(handler, *op, *lhs, *rhs, span))
            }

            Expr::MacroCall { .. } => Ok(Inference::Ignored),

            Expr::IntrinsicCall { kind, args, span } => {
                self.infer_intrinsic_call_expr(handler, pred, kind, args, span)
            }

            Expr::LocalPredicateCall {
                predicate: called_predicate,
                args,
                span,
            } => self.infer_local_predicate_call(handler, pred, called_predicate, args, span),

            Expr::ExternalPredicateCall {
                interface,
                c_addr,
                predicate: called_predicate,
                p_addr,
                args,
                span,
            } => self.infer_external_predicate_call(
                handler,
                interface,
                *c_addr,
                called_predicate,
                *p_addr,
                args,
                span,
            ),

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

            Expr::UnionTag { .. } | Expr::UnionValue { .. } => Err(handler.emit_internal_err(
                "union utility expressions should not exist during type checking".to_string(),
                empty_span(),
            )),
        }

        // TODO return err if handler is non-empty?
    }

    pub(in crate::predicate::analyse) fn infer_immediate(
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
                handler.emit_internal_err(
                    "array immediate does NOT have an array type?".to_string(),
                    span.clone(),
                );
                return Inference::Type(Type::Error(span.clone()));
            };

            let _ = el_imms.iter().try_for_each(|el_imm| {
                let el_ty = el_imm.get_ty(None);
                if !el_ty.eq(self, el0_ty.as_ref()) {
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

    fn infer_local_storage_access(
        &self,
        handler: &Handler,
        name: &String,
        span: &Span,
    ) -> Inference {
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
        interface: &String,
        address: ExprKey,
        name: &String,
        span: &Span,
    ) -> Inference {
        if address.get_ty(self).is_unknown() {
            Inference::Dependant(address)
        } else {
            // Find the interface declaration corresponding to the interface instance
            let Some(interface) = self
                .interfaces
                .iter()
                .find(|e| e.name.to_string() == *interface)
            else {
                handler.emit_err(Error::Compile {
                    error: CompileError::MissingInterface {
                        name: interface.clone(),
                        span: span.clone(),
                    },
                });
                return Inference::Type(Type::Error(empty_span()));
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
        }
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
                        .map(|pred| pred.variables().any(|(_, variable)| variable.name == *name))
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
                handler.emit_internal_err(
                    "unable to type check unary op error".to_string(),
                    span.clone(),
                );
                Inference::Type(Type::Error(span.clone()))
            }

            UnaryOp::NextState => {
                // Next state access must be a path that resolves to a variable.  It _may_ be
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
        op: BinaryOp,
        lhs_expr_key: ExprKey,
        rhs_expr_key: ExprKey,
        span: &Span,
    ) -> Inference {
        let check_numeric_args = |lhs_ty: &Type, rhs_ty: &Type| {
            if !lhs_ty.is_num() {
                if !lhs_ty.is_error() {
                    handler.emit_err(Error::Compile {
                        error: CompileError::OperatorInvalidType {
                            op: op.as_str(),
                            ty_kind: "non-numeric",
                            bad_ty: self.with_ctrct(lhs_ty).to_string(),
                            span: span.clone(),
                        },
                    });
                }
            } else if !rhs_ty.is_num() {
                if !rhs_ty.is_error() {
                    handler.emit_err(Error::Compile {
                        error: CompileError::OperatorInvalidType {
                            op: op.as_str(),
                            ty_kind: "non-numeric",
                            bad_ty: self.with_ctrct(rhs_ty).to_string(),
                            span: span.clone(),
                        },
                    });
                }
            } else if !lhs_ty.eq(self, rhs_ty) {
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
                        check_numeric_args(&lhs_ty, rhs_ty);
                        Inference::Type(lhs_ty)
                    }
                    BinaryOp::Equal | BinaryOp::NotEqual => {
                        // Both args must be equatable, which at this stage is any type *except*
                        // unions; binary op type is bool.
                        if !lhs_ty.eq(self, rhs_ty) {
                            // Only emit an error if neither side is nil nor error, nor an
                            // initialiser constraint as per above.
                            if !lhs_ty.is_nil()
                                && !rhs_ty.is_nil()
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
                                            expected_span: Some(
                                                self.expr_key_to_span(lhs_expr_key),
                                            ),
                                        }),
                                    },
                                });
                            }
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
                        check_numeric_args(&lhs_ty, rhs_ty);
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
                if !expected.eq(self, found) {
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

    pub(super) fn infer_local_predicate_call(
        &self,
        handler: &Handler,
        pred: Option<&Predicate>,
        called_predicate: &String,
        args: &[ExprKey],
        span: &Span,
    ) -> Result<Inference, ErrorEmitted> {
        let deps = args
            .iter()
            .filter(|arg_key| arg_key.get_ty(self).is_unknown())
            .cloned()
            .collect::<Vec<_>>();

        if deps.is_empty() {
            if let Some(pred) = pred {
                if *called_predicate == pred.name {
                    handler.emit_err(Error::Compile {
                        error: CompileError::SelfReferencialPredicate {
                            pred_name: called_predicate.to_string(),
                            span: span.clone(),
                        },
                    });

                    return Ok(Inference::Type(r#bool()));
                }
            }

            // Find the called predicate the `self.preds`
            let Some((_, called_predicate)) = self
                .preds
                .iter()
                .find(|(_, pred)| pred.name == *called_predicate)
            else {
                handler.emit_err(Error::Compile {
                    error: CompileError::MissingPredicate {
                        pred_name: called_predicate.to_string(),
                        interface_name: None,
                        span: span.clone(),
                    },
                });

                return Ok(Inference::Type(r#bool()));
            };

            // Check all the arg types against the predicate found above
            for (expected, found) in called_predicate.params.iter().zip(args.iter()) {
                let found_ty = found.get_ty(self);
                let expected_ty = &expected.ty;
                if !expected_ty.eq(self, found_ty) {
                    handler.emit_err(Error::Compile {
                        error: CompileError::MismatchedPredicateArgType {
                            expected: format!("{}", self.with_ctrct(expected_ty)),
                            found: format!("{}", self.with_ctrct(found_ty)),
                            span: span.clone(),
                            arg_span: found.get(self).span().clone(),
                        },
                    });
                }
            }

            // Also, ensure that the number of arguments is correct
            if called_predicate.params.len() != args.len() {
                handler.emit_err(Error::Compile {
                    error: CompileError::UnexpectedPredicateArgCount {
                        expected: called_predicate.params.len(),
                        found: args.len(),
                        span: span.clone(),
                    },
                });
            }

            Ok(Inference::Type(r#bool()))
        } else {
            Ok(Inference::Dependencies(deps))
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn infer_external_predicate_call(
        &self,
        handler: &Handler,
        interface: &String,
        c_addr: ExprKey,
        called_predicate: &String,
        p_addr: ExprKey,
        args: &[ExprKey],
        span: &Span,
    ) -> Result<Inference, ErrorEmitted> {
        let mut deps = Vec::new();

        // Check the contract address
        let c_addr_ty = c_addr.get_ty(self);
        if c_addr_ty.is_unknown() {
            deps.push(c_addr);
        } else if !c_addr_ty.is_b256() {
            handler.emit_err(Error::Compile {
                error: CompileError::AddressExpressionTypeError {
                    large_err: Box::new(LargeTypeError::AddressExpressionTypeError {
                        expected_ty: self.with_ctrct(b256()).to_string(),
                        found_ty: self.with_ctrct(c_addr_ty).to_string(),
                        span: self.expr_key_to_span(c_addr),
                        expected_span: Some(self.expr_key_to_span(c_addr)),
                    }),
                },
            });
        }

        // Check the predicate address
        let p_addr_ty = p_addr.get_ty(self);
        if p_addr_ty.is_unknown() {
            deps.push(p_addr);
        } else if !p_addr_ty.is_b256() {
            handler.emit_err(Error::Compile {
                error: CompileError::AddressExpressionTypeError {
                    large_err: Box::new(LargeTypeError::AddressExpressionTypeError {
                        expected_ty: self.with_ctrct(b256()).to_string(),
                        found_ty: self.with_ctrct(p_addr_ty).to_string(),
                        span: self.expr_key_to_span(p_addr),
                        expected_span: Some(self.expr_key_to_span(p_addr)),
                    }),
                },
            });
        }

        args.iter()
            .filter(|arg_key| arg_key.get_ty(self).is_unknown())
            .for_each(|arg_key| deps.push(*arg_key));

        if deps.is_empty() {
            // Search for the contract interface
            let Some(interface) = self
                .interfaces
                .iter()
                .find(|e| e.name.to_string() == *interface)
            else {
                handler.emit_err(Error::Compile {
                    error: CompileError::MissingInterface {
                        name: interface.clone(),
                        span: span.clone(),
                    },
                });
                return Ok(Inference::Type(r#bool()));
            };

            // Search for the predicate interface
            let Some(predicate_interface) = interface
                .predicate_interfaces
                .iter()
                .find(|e| e.name.to_string() == *called_predicate.to_string())
            else {
                handler.emit_err(Error::Compile {
                    error: CompileError::MissingPredicate {
                        pred_name: called_predicate.to_string(),
                        interface_name: Some(interface.name.to_string()),
                        span: span.clone(),
                    },
                });
                return Ok(Inference::Type(r#bool()));
            };

            // Check all the arg types against the predicate found above
            for (expected, arg) in predicate_interface.params.iter().zip(args.iter()) {
                let found = arg.get_ty(self);
                if !expected.ty.eq(self, found) {
                    handler.emit_err(Error::Compile {
                        error: CompileError::MismatchedPredicateArgType {
                            expected: format!("{}", self.with_ctrct(expected.ty.clone())),
                            found: format!("{}", self.with_ctrct(found)),
                            span: span.clone(),
                            arg_span: arg.get(self).span().clone(),
                        },
                    });
                }
            }

            // Also, ensure that the number of arguments is correct
            if predicate_interface.params.len() != args.len() {
                handler.emit_err(Error::Compile {
                    error: CompileError::UnexpectedPredicateArgCount {
                        expected: predicate_interface.params.len(),
                        found: args.len(),
                        span: span.clone(),
                    },
                });
            }

            Ok(Inference::Type(r#bool()))
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
                    if !then_ty.eq(self, else_ty) {
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
        } else if !union_ty.is_union() {
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
                if let Some(union_variant_count) = union_ty.get_union_variant_count(self) {
                    if variant_count < union_variant_count && else_branch.is_none() {
                        // We don't have all variants covered.
                        let mut missing_variants = union_ty.get_union_variant_names(self);
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
                    if !branch_ty.eq(self, match_ty) {
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
                if !lb_ty.eq(self, ub_ty) {
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
        // FROM               TO    ACTION
        //
        // bool               int   Boolean to integer cast
        // int                int   No-op
        // enumeration union  int   Produce the tag
        //
        // int                real  Produce the closest possible real
        // enumeration union  real  Produce the tag and then produce the closest possible real
        // real               real  No-op

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
            } else if (to_ty.is_int() && !from_ty.is_bool())
                && !from_ty.is_int()
                && !from_ty.is_enumeration_union(self)
                || (to_ty.is_real()
                    && !from_ty.is_int()
                    && !from_ty.is_enumeration_union(self)
                    && !from_ty.is_real())
            {
                // We can only cast
                // - To ints from bools, ints, or enumeration unions.
                // - To reals from ints, enumeration unions, or reals.
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
                    if !value_ty.eq(self, collection_ty) {
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
                    if !value_ty.eq(self, el_ty) {
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
                    handler.emit_internal_err(
                        "range ty is not numeric or array?".to_string(),
                        span.clone(),
                    );
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
                    if !el_ty.eq(self, el0_ty) {
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

            if (!index_ty.is_int() && !index_ty.is_enumeration_union(self))
                || !index_ty.eq(self, range_ty)
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
                handler.emit_internal_err(
                    "failed to get array element type in infer_index_expr()".to_string(),
                    span.clone(),
                );

                Inference::Type(Type::Error(span.clone()))
            }
        } else if let Some(el_ty) = ary_ty.get_array_el_type() {
            // Is it an array with an unknown range (probably a const immediate)?
            Inference::Type(el_ty.clone())
        } else if let Some(from_ty) = ary_ty.get_map_ty_from() {
            // Is this a storage map?
            if !from_ty.eq(self, index_ty) {
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
                handler.emit_internal_err(
                    "failed to get array element type \
                          in infer_index_expr()"
                        .to_string(),
                    span.clone(),
                );

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
                        handler.emit_internal_err(
                            "unable to type check tuple field access error".to_string(),
                            span.clone(),
                        );
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
            if let Some((decl, union_decl)) = self
                .unions
                .iter()
                .find(|(_, ud)| ud.name.name == union_name)
            {
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
                                decl,
                                span: union_decl.span.clone(),
                            })
                        }

                        (Some(value), Some(ud_var_ty)) => {
                            // Confirm variant type is value type.
                            let value_ty = value.get_ty(self);
                            if value_ty.is_unknown() {
                                Inference::Dependant(value)
                            } else if !value_ty.eq(self, ud_var_ty) {
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
                                    decl,
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
}
