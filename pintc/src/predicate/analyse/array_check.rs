use super::{Contract, Evaluator, ExprKey, Predicate};
use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::{BinaryOp, Expr, Immediate},
    span::Spanned,
    types::Type,
};

fn get_array_size_from_type(
    contract: &Contract,
    pred: &Predicate,
    handler: &Handler,
    array_ty: &Type,
) -> Result<i64, ErrorEmitted> {
    if let Some(n) = array_ty.get_array_size() {
        Ok(n)
    } else if let Some(range_expr_key) = array_ty.get_array_range_expr() {
        Type::get_array_size_from_range_expr(handler, range_expr_key.get(contract), contract, pred)
    } else {
        unreachable!("array_ty MUST be an array type for this call")
    }
}

impl Predicate {
    pub(crate) fn check_array_lengths(&self, contract: &Contract, handler: &Handler) {
        fn check_array_type(contract: &Contract, pred: &Predicate, handler: &Handler, ty: &Type) {
            if ty.is_array() {
                if let Ok(n) = get_array_size_from_type(contract, pred, handler, ty) {
                    if n < 1 {
                        handler.emit_err(Error::Compile {
                            error: CompileError::InvalidConstArrayLength {
                                span: ty.span().clone(),
                            },
                        });
                    }
                }
            }
        }

        for var_key in self.vars().map(|(k, _)| k) {
            check_array_type(contract, self, handler, var_key.get_ty(self));
        }

        for expr_key in contract.exprs(self) {
            check_array_type(contract, self, handler, expr_key.get_ty(contract));
        }
    }

    pub(crate) fn check_array_indexing(&self, contract: &Contract, handler: &Handler) {
        // Gather all accesses into *arrays*.  `Expr::Index` is also used for maps.
        let accesses: Vec<(Type, ExprKey)> = contract
            .exprs(self)
            .filter_map(|expr_key| {
                expr_key.try_get(contract).and_then(|expr| {
                    if let Expr::Index { expr, index, .. } = expr {
                        let indexed_ty = expr.get_ty(contract);
                        indexed_ty.is_array().then(|| (indexed_ty.clone(), *index))
                    } else {
                        None
                    }
                })
            })
            .collect();

        let evaluator = Evaluator::new(self);
        for (array_ty, index_key) in accesses {
            // First, try evaluating the index value, since it must be an immediate int (or enum
            // variant, which evaluates to int).
            let index_expr = index_key.get(contract);
            let index_span = index_expr.span().clone();
            if let Ok(index_value) = evaluator
                .evaluate(index_expr, handler, contract, &self.name)
                .map_err(|_| {
                    handler.emit_err(Error::Compile {
                        error: CompileError::NonConstArrayIndex {
                            span: index_span.clone(),
                        },
                    })
                })
            {
                // Get the size of the accessed array.
                if let Ok(array_size) = get_array_size_from_type(contract, self, handler, &array_ty)
                {
                    // Check for OOB.
                    if let Immediate::Int(imm_val) = index_value {
                        if imm_val < 0 || imm_val >= array_size {
                            handler.emit_err(Error::Compile {
                                error: CompileError::ArrayIndexOutOfBounds { span: index_span },
                            });
                        }
                    } else {
                        handler.emit_err(Error::Compile {
                            error: CompileError::InvalidConstArrayIndex { span: index_span },
                        });
                    }
                }
            }
        }
    }

    pub(crate) fn check_array_compares(&self, contract: &Contract, handler: &Handler) {
        // Analyse all comparisons between arrays.  The only valid operators are Eq and NotEq.
        for expr_key in contract.exprs(self) {
            if let Expr::BinaryOp { op, lhs, rhs, span } = expr_key.get(contract) {
                if *op == BinaryOp::Equal || *op == BinaryOp::NotEqual {
                    let lhs_ty = lhs.get_ty(contract);
                    let rhs_ty = rhs.get_ty(contract);

                    if lhs_ty.is_array() && rhs_ty.is_array() {
                        // We're comparing arrays.  Now compare their sizes.
                        let lhs_size = get_array_size_from_type(contract, self, handler, lhs_ty);
                        let rhs_size = get_array_size_from_type(contract, self, handler, rhs_ty);

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
