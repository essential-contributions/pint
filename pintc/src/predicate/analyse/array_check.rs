use super::{Contract, Evaluator, ExprKey};
use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler, LargeTypeError},
    expr::{BinaryOp, Expr, Immediate},
    predicate::PredKey,
    span::Spanned,
    types::Type,
};

fn get_array_size_from_type(
    contract: &Contract,
    handler: &Handler,
    array_ty: &Type,
) -> Result<i64, ErrorEmitted> {
    if let Some(n) = array_ty.get_array_size() {
        Ok(n)
    } else if let Some(range_expr_key) = array_ty.get_array_range_expr() {
        Type::get_array_size_from_range_expr(handler, range_expr_key, contract)
    } else {
        unreachable!("array_ty MUST be an array type for this call")
    }
}

impl Contract {
    pub(crate) fn check_array_lengths(&self, handler: &Handler, pred_key: PredKey) {
        fn check_array_type(
            contract: &Contract,
            handler: &Handler,
            ty: &Type,
            init: Option<ExprKey>,
        ) {
            if ty.is_array() {
                if let Ok(n) = get_array_size_from_type(contract, handler, ty) {
                    if n < 1 {
                        handler.emit_err(Error::Compile {
                            error: CompileError::InvalidConstArrayLength {
                                span: ty.span().clone(),
                            },
                        });
                    }

                    // We know we have an array expression.  If we have an initialiser then the
                    // type checker must agree that it's an array, but currently it doesn't check
                    // the lengths are correct.
                    if let Some(init) = init {
                        let init_ty = init.get_ty(contract);
                        if let Some(init_len) = init_ty.get_array_size() {
                            if init_len != n {
                                handler.emit_err(Error::Compile {
                                    error: CompileError::InitTypeError {
                                        init_kind: "const",
                                        large_err: Box::new(LargeTypeError::InitTypeError {
                                            init_kind: "const",
                                            expected_ty: contract.with_ctrct(ty).to_string(),
                                            found_ty: contract.with_ctrct(init_ty).to_string(),
                                            expected_ty_span: ty.span().clone(),
                                            init_span: contract.expr_key_to_span(init),
                                        }),
                                    },
                                });
                            }
                        }
                    }
                }
            }
        }

        let pred = &self.preds[pred_key];

        for param in &pred.params {
            check_array_type(self, handler, &param.ty, None);
        }

        for expr_key in self.exprs(pred_key) {
            check_array_type(self, handler, expr_key.get_ty(self), None);
        }

        for (_name, cnst) in self.consts.iter() {
            check_array_type(self, handler, &cnst.decl_ty, Some(cnst.expr));
        }
    }

    pub(crate) fn check_array_indexing(&self, handler: &Handler, pred_key: PredKey) {
        // Gather all accesses into *arrays*.  `Expr::Index` is also used for maps.
        let accesses: Vec<(Type, ExprKey)> = self
            .exprs(pred_key)
            .filter_map(|expr_key| {
                expr_key.try_get(self).and_then(|expr| {
                    if let Expr::Index { expr, index, .. } = expr {
                        let indexed_ty = expr.get_ty(self);
                        indexed_ty.is_array().then(|| (indexed_ty.clone(), *index))
                    } else {
                        None
                    }
                })
            })
            .collect();

        let evaluator = Evaluator::new(self);
        for (array_ty, index_key) in accesses {
            // First, try evaluating the index value, since it must be an immediate int (or union
            // variant, which evaluates to int).
            let index_span = index_key.get(self).span().clone();
            if let Ok(index_value) = evaluator.evaluate(index_key, handler, self).map_err(|_| {
                handler.emit_err(Error::Compile {
                    error: CompileError::NonConstArrayIndex {
                        span: index_span.clone(),
                    },
                })
            }) {
                // Get the size of the accessed array.
                if let Ok(array_size) = get_array_size_from_type(self, handler, &array_ty) {
                    // Check for OOB.
                    match index_value {
                        Immediate::Int(imm_val) => {
                            if imm_val < 0 || imm_val >= array_size {
                                handler.emit_err(Error::Compile {
                                    error: CompileError::ArrayIndexOutOfBounds { span: index_span },
                                });
                            }
                        }

                        Immediate::UnionVariant { tag_num, decl, .. }
                            if self.unions[decl].is_enumeration_union() =>
                        {
                            if tag_num < 0 || tag_num >= array_size {
                                handler.emit_err(Error::Compile {
                                    error: CompileError::ArrayIndexOutOfBounds { span: index_span },
                                });
                            }
                        }

                        _ => {
                            handler.emit_err(Error::Compile {
                                error: CompileError::InvalidConstArrayIndex { span: index_span },
                            });
                        }
                    }
                }
            }
        }
    }

    pub(crate) fn check_array_compares(&self, handler: &Handler, pred_key: PredKey) {
        // Analyse all comparisons between arrays.  The only valid operators are Eq and NotEq.
        for expr_key in self.exprs(pred_key) {
            if let Expr::BinaryOp { op, lhs, rhs, span } = expr_key.get(self) {
                if *op == BinaryOp::Equal || *op == BinaryOp::NotEqual {
                    let lhs_ty = lhs.get_ty(self);
                    let rhs_ty = rhs.get_ty(self);

                    if lhs_ty.is_array() && rhs_ty.is_array() {
                        // We're comparing arrays.  Now compare their sizes.
                        let lhs_size = get_array_size_from_type(self, handler, lhs_ty);
                        let rhs_size = get_array_size_from_type(self, handler, rhs_ty);

                        if let (Ok(lhs_size), Ok(rhs_size)) = (lhs_size, rhs_size) {
                            if lhs_size != rhs_size {
                                handler.emit_err(Error::Compile {
                                    error: CompileError::MismatchedArrayComparisonSizes {
                                        op: op.to_string(),
                                        lhs_size,
                                        rhs_size,
                                        span: span.clone(),
                                    },
                                });
                            }
                        }
                    }
                }
            }
        }
    }
}
