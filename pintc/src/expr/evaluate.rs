use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::{BinaryOp, Expr, Immediate, UnaryOp},
    intermediate::{ExprKey, IntermediateIntent},
    span::empty_span,
    types::Path,
};
use std::collections::HashMap;

impl Expr {
    /// Given an expression `self`, an `IntermediateIntent`, and a map between symbols and their
    /// values as `Immediate`s, evaluate `self` into an `Immediate`
    ///
    /// This is pretty basic for now and will fail for anything other than `Immediate`,
    /// `PathByName`, `UnaryOp`, `BinaryOp`.
    pub(crate) fn evaluate(
        &self,
        handler: &Handler,
        ii: &IntermediateIntent,
        values_map: &HashMap<Path, Immediate>,
    ) -> Result<Immediate, ErrorEmitted> {
        use BinaryOp::*;
        use Immediate::{Bool, Int, Real};
        use UnaryOp::{Neg, Not};
        match self {
            Expr::Immediate { value: imm, .. } => Ok(imm.clone()),
            Expr::PathByName(path, span) => values_map.get(path).cloned().ok_or_else(|| {
                handler.emit_err(Error::Compile {
                    error: CompileError::SymbolNotFound {
                        name: path.to_string(),
                        span: span.clone(),
                        enum_names: Vec::new(),
                    },
                })
            }),
            Expr::UnaryOp { op, expr, .. } => {
                let expr = ii
                    .exprs
                    .get(*expr)
                    .expect("guaranteed by parser")
                    .evaluate(handler, ii, values_map)?;

                match (expr, op) {
                    (Real(expr), Neg) => Ok(Real(-expr)),
                    (Int(expr), Neg) => Ok(Int(-expr)),
                    (Bool(expr), Not) => Ok(Bool(!expr)),
                    _ => Err(handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "type error: invalid unary op for expression",
                            span: empty_span(),
                        },
                    })),
                }
            }
            Expr::BinaryOp { op, lhs, rhs, .. } => {
                let lhs = ii
                    .exprs
                    .get(*lhs)
                    .expect("guaranteed by parser")
                    .evaluate(handler, ii, values_map)?;

                let rhs = ii
                    .exprs
                    .get(*rhs)
                    .expect("guaranteed by parser")
                    .evaluate(handler, ii, values_map)?;

                match (lhs, rhs) {
                    (Real(lhs), Real(rhs)) => match op {
                        // Arithmetic
                        Add => Ok(Real(lhs + rhs)),
                        Sub => Ok(Real(lhs - rhs)),
                        Mul => Ok(Real(lhs * rhs)),
                        Div => Ok(Real(lhs / rhs)),

                        // Comparison
                        Equal => Ok(Bool(lhs == rhs)),
                        NotEqual => Ok(Bool(lhs != rhs)),
                        LessThan => Ok(Bool(lhs < rhs)),
                        LessThanOrEqual => Ok(Bool(lhs <= rhs)),
                        GreaterThan => Ok(Bool(lhs > rhs)),
                        GreaterThanOrEqual => Ok(Bool(lhs >= rhs)),

                        _ => Err(handler.emit_err(Error::Compile {
                            error: CompileError::Internal {
                                msg: "type error: invalid binary op for reals",
                                span: empty_span(),
                            },
                        })),
                    },
                    (Int(lhs), Int(rhs)) => match op {
                        // Arithmetic
                        Add => Ok(Int(lhs + rhs)),
                        Sub => Ok(Int(lhs - rhs)),
                        Mul => Ok(Int(lhs * rhs)),
                        Div => Ok(Int(lhs / rhs)),
                        Mod => Ok(Int(lhs % rhs)),

                        // Comparison
                        Equal => Ok(Bool(lhs == rhs)),
                        NotEqual => Ok(Bool(lhs != rhs)),
                        LessThan => Ok(Bool(lhs < rhs)),
                        LessThanOrEqual => Ok(Bool(lhs <= rhs)),
                        GreaterThan => Ok(Bool(lhs > rhs)),
                        GreaterThanOrEqual => Ok(Bool(lhs >= rhs)),

                        _ => Err(handler.emit_err(Error::Compile {
                            error: CompileError::Internal {
                                msg: "type error: invalid binary op for ints",
                                span: empty_span(),
                            },
                        })),
                    },
                    (Bool(lhs), Bool(rhs)) => match op {
                        // Comparison
                        Equal => Ok(Bool(lhs == rhs)),
                        NotEqual => Ok(Bool(lhs != rhs)),
                        LessThan => Ok(Bool(!lhs && rhs)),
                        LessThanOrEqual => Ok(Bool(lhs <= rhs)),
                        GreaterThan => Ok(Bool(lhs && !rhs)),
                        GreaterThanOrEqual => Ok(Bool(lhs >= rhs)),

                        // Logical
                        LogicalAnd => Ok(Bool(lhs && rhs)),
                        LogicalOr => Ok(Bool(lhs || rhs)),

                        _ => Err(handler.emit_err(Error::Compile {
                            error: CompileError::Internal {
                                msg: "type error: invalid binary op for bools",
                                span: empty_span(),
                            },
                        })),
                    },
                    _ => Err(handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "compile-time evaluation for \"big ints\" and \"strings\" \
                              not currently supported",
                            span: empty_span(),
                        },
                    })),
                }
            }
            _ => Err(handler.emit_err(Error::Compile {
                error: CompileError::Internal {
                    msg: "unexpected expression during compile-time evaluation",
                    span: empty_span(),
                },
            })),
        }
    }
}

impl ExprKey {
    /// Given an `ExprKey` `self`, an `IntermediateIntent`, and a map between some symbols and
    /// their values as `Immediate`s, create a deep clone of `self` (i.e. clone the `Expr` it
    /// points to and all of its sub-expressions) while replacing each symbol by its immediate
    /// value as per `values_map`.
    ///
    /// Note: not every symbol needs to be replaced.
    ///
    /// For example, if `self` points to the following expression:
    ///
    ///
    /// ```pint
    /// i + j > k
    /// ```
    ///
    /// and if `values_map` looks like: `i -> 5, j -> 9`, then `plug_in` creates the following new
    /// expression:
    ///
    /// ```pint
    /// 5 + 9 > k
    /// ```
    ///
    /// and inserts it (and its sub-expressions) into `ii.exprs`.
    pub(crate) fn plug_in(
        self,
        ii: &mut IntermediateIntent,
        values_map: &HashMap<Path, Immediate>,
    ) -> ExprKey {
        let expr = ii
            .exprs
            .get(self)
            .expect("expr key must belong to ii.expr")
            .clone();

        let plugged = match expr {
            Expr::Immediate { .. }
            | Expr::StorageAccess(..)
            | Expr::ExternalStorageAccess { .. }
            | Expr::MacroCall { .. }
            | Expr::Error(_) => expr,
            Expr::PathByName(ref path, ref span) => {
                let span = span.clone();
                values_map.get(path).map_or(expr, |value| Expr::Immediate {
                    value: value.clone(),
                    span,
                })
            }
            Expr::PathByKey(key, ref span) => {
                let span = span.clone();
                values_map
                    .get(&ii.vars[key].name)
                    .map_or(expr, |value| Expr::Immediate {
                        value: value.clone(),
                        span,
                    })
            }
            Expr::UnaryOp { op, expr, span } => {
                let expr = expr.plug_in(ii, values_map);

                Expr::UnaryOp { op, expr, span }
            }
            Expr::BinaryOp { op, lhs, rhs, span } => {
                let lhs = lhs.plug_in(ii, values_map);
                let rhs = rhs.plug_in(ii, values_map);

                Expr::BinaryOp { op, lhs, rhs, span }
            }
            Expr::FnCall { name, args, span } => {
                let args = args
                    .iter()
                    .map(|arg| arg.plug_in(ii, values_map))
                    .collect::<Vec<_>>();

                Expr::FnCall { name, args, span }
            }
            Expr::If {
                condition,
                then_block,
                else_block,
                span,
            } => {
                let condition = condition.plug_in(ii, values_map);
                let then_block = then_block.plug_in(ii, values_map);
                let else_block = else_block.plug_in(ii, values_map);

                Expr::If {
                    condition,
                    then_block,
                    else_block,
                    span,
                }
            }
            Expr::Array {
                elements,
                range_expr,
                span,
            } => {
                let elements = elements
                    .iter()
                    .map(|element| element.plug_in(ii, values_map))
                    .collect::<Vec<_>>();
                let range_expr = range_expr.plug_in(ii, values_map);

                Expr::Array {
                    elements,
                    range_expr,
                    span,
                }
            }
            Expr::Index { expr, index, span } => {
                let expr = expr.plug_in(ii, values_map);
                let index = index.plug_in(ii, values_map);

                Expr::Index { expr, index, span }
            }
            Expr::Tuple { fields, span } => {
                let fields = fields
                    .iter()
                    .map(|(name, value)| (name.clone(), value.plug_in(ii, values_map)))
                    .collect::<Vec<_>>();

                Expr::Tuple { fields, span }
            }
            Expr::TupleFieldAccess { tuple, field, span } => {
                let tuple = tuple.plug_in(ii, values_map);

                Expr::TupleFieldAccess { tuple, field, span }
            }
            Expr::Cast { value, ty, span } => {
                let value = value.plug_in(ii, values_map);

                Expr::Cast { value, ty, span }
            }
            Expr::In {
                value,
                collection,
                span,
            } => {
                let value = value.plug_in(ii, values_map);
                let collection = collection.plug_in(ii, values_map);

                Expr::In {
                    value,
                    collection,
                    span,
                }
            }
            Expr::Range { lb, ub, span } => {
                let lb = lb.plug_in(ii, values_map);
                let ub = ub.plug_in(ii, values_map);

                Expr::Range { lb, ub, span }
            }
            Expr::Generator {
                kind,
                gen_ranges,
                conditions,
                body,
                span,
            } => {
                let gen_ranges = gen_ranges
                    .iter()
                    .map(|(index, range)| (index.clone(), range.plug_in(ii, values_map)))
                    .collect::<Vec<_>>();
                let conditions = conditions
                    .iter()
                    .map(|condition| condition.plug_in(ii, values_map))
                    .collect::<Vec<_>>();
                let body = body.plug_in(ii, values_map);

                Expr::Generator {
                    kind,
                    gen_ranges,
                    conditions,
                    body,
                    span,
                }
            }
        };

        // Insert the new plugged expression and its type.
        let plugged_key = ii.exprs.insert(plugged);
        if let Some(expr_ty) = ii.expr_types.get(self) {
            ii.expr_types.insert(plugged_key, expr_ty.clone());
        }

        plugged_key
    }
}
