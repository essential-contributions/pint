use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::{
        BinaryOp as BinOp, Expr, Immediate as Imm, MatchBranch, MatchElse, TupleAccess, UnaryOp,
    },
    predicate::{Contract, ExprKey},
    span::{empty_span, Spanned},
    types::Type,
};
use fxhash::FxHashMap;

#[derive(Default)]
pub(crate) struct Evaluator {
    enumeration_union_values: FxHashMap<String, Imm>,
    scope_values: FxHashMap<String, Imm>,
}

impl Evaluator {
    pub(crate) fn new(contract: &Contract) -> Evaluator {
        Evaluator {
            enumeration_union_values: Self::create_enumeration_union_map(contract),
            scope_values: FxHashMap::default(),
        }
    }

    pub(crate) fn from_values(
        contract: &Contract,
        scope_values: FxHashMap<String, Imm>,
    ) -> Evaluator {
        Evaluator {
            enumeration_union_values: Self::create_enumeration_union_map(contract),
            scope_values,
        }
    }

    pub(crate) fn contains_path(&self, path: &String) -> bool {
        self.scope_values.contains_key(path)
    }

    pub(crate) fn insert_value(&mut self, path: String, imm: Imm) -> Option<Imm> {
        self.scope_values.insert(path, imm)
    }

    pub(crate) fn into_values(self) -> FxHashMap<String, Imm> {
        self.scope_values
    }

    /// Given a list of `UnionDecl`s, create an `FxHashMap` from union variant paths to
    /// corresponding `Imm::UnionVariant`s. Only consider unions that are enumerations, i.e.,
    /// unions with only valueless variants.
    fn create_enumeration_union_map(contract: &Contract) -> FxHashMap<String, Imm> {
        FxHashMap::from_iter(
            contract
                .unions
                .iter()
                .filter(|(_, union)| union.is_enumeration_union())
                .flat_map(|(key, union)| {
                    union.variants.iter().enumerate().map(move |(idx, v)| {
                        (
                            format!("{}::{}", union.name.name, v.variant_name.name),
                            Imm::UnionVariant {
                                tag_num: idx as i64,
                                value_size: 0,
                                value: None,
                                decl: key,
                            },
                        )
                    })
                }),
        )
    }

    pub(crate) fn evaluate(
        &self,
        expr: ExprKey,
        handler: &Handler,
        contract: &Contract,
    ) -> Result<Imm, ErrorEmitted> {
        match expr.get(contract) {
            Expr::Immediate { value, .. } => Ok(value.clone()),

            Expr::Array { elements, .. } => {
                let imm_elements = elements
                    .iter()
                    .map(|el_key| self.evaluate(*el_key, handler, contract))
                    .collect::<Result<_, _>>()?;

                Ok(Imm::Array(imm_elements))
            }

            Expr::Tuple { fields, .. } => {
                let imm_fields = fields
                    .iter()
                    .map(|(name, fld_key)| {
                        self.evaluate(*fld_key, handler, contract)
                            .map(|fld_imm| (name.clone(), fld_imm))
                    })
                    .collect::<Result<_, _>>()?;

                Ok(Imm::Tuple(imm_fields))
            }

            Expr::Path(path, span) => self
                .scope_values
                .get(path)
                .or_else(|| self.enumeration_union_values.get(path))
                .cloned()
                .ok_or_else(|| {
                    handler.emit_err(Error::Compile {
                        error: CompileError::SymbolNotFound {
                            name: path.to_string(),
                            span: span.clone(),
                            union_names: Vec::new(),
                        },
                    })
                }),

            Expr::UnaryOp { op, expr, .. } => {
                let expr = self.evaluate(*expr, handler, contract)?;

                match (expr, op) {
                    (Imm::Real(expr), UnaryOp::Neg) => Ok(Imm::Real(-expr)),
                    (Imm::Int(expr), UnaryOp::Neg) => Ok(Imm::Int(-expr)),
                    (Imm::Bool(expr), UnaryOp::Not) => Ok(Imm::Bool(!expr)),
                    _ => Err(handler.emit_internal_err(
                        "type error: invalid unary op for expression",
                        empty_span(),
                    )),
                }
            }

            Expr::BinaryOp { op, lhs, rhs, .. } => {
                let lhs = self.evaluate(*lhs, handler, contract)?;
                let rhs = self.evaluate(*rhs, handler, contract)?;

                match (lhs, rhs) {
                    (Imm::Real(lhs), Imm::Real(rhs)) => match op {
                        // Arithmetic
                        BinOp::Add => Ok(Imm::Real(lhs + rhs)),
                        BinOp::Sub => Ok(Imm::Real(lhs - rhs)),
                        BinOp::Mul => Ok(Imm::Real(lhs * rhs)),
                        BinOp::Div => Ok(Imm::Real(lhs / rhs)),

                        // Comparison
                        BinOp::Equal => Ok(Imm::Bool(lhs == rhs)),
                        BinOp::NotEqual => Ok(Imm::Bool(lhs != rhs)),
                        BinOp::LessThan => Ok(Imm::Bool(lhs < rhs)),
                        BinOp::LessThanOrEqual => Ok(Imm::Bool(lhs <= rhs)),
                        BinOp::GreaterThan => Ok(Imm::Bool(lhs > rhs)),
                        BinOp::GreaterThanOrEqual => Ok(Imm::Bool(lhs >= rhs)),

                        _ => Err(handler.emit_internal_err(
                            "type error: invalid binary op for reals",
                            empty_span(),
                        )),
                    },

                    (Imm::Int(lhs), Imm::Int(rhs)) => match op {
                        // Arithmetic
                        BinOp::Add => Ok(Imm::Int(lhs + rhs)),
                        BinOp::Sub => Ok(Imm::Int(lhs - rhs)),
                        BinOp::Mul => Ok(Imm::Int(lhs * rhs)),
                        BinOp::Div => Ok(Imm::Int(lhs / rhs)),
                        BinOp::Mod => Ok(Imm::Int(lhs % rhs)),

                        // Comparison
                        BinOp::Equal => Ok(Imm::Bool(lhs == rhs)),
                        BinOp::NotEqual => Ok(Imm::Bool(lhs != rhs)),
                        BinOp::LessThan => Ok(Imm::Bool(lhs < rhs)),
                        BinOp::LessThanOrEqual => Ok(Imm::Bool(lhs <= rhs)),
                        BinOp::GreaterThan => Ok(Imm::Bool(lhs > rhs)),
                        BinOp::GreaterThanOrEqual => Ok(Imm::Bool(lhs >= rhs)),

                        _ => Err(handler.emit_internal_err(
                            "type error: invalid binary op for ints",
                            empty_span(),
                        )),
                    },

                    (Imm::Bool(lhs), Imm::Bool(rhs)) => match op {
                        // Comparison
                        BinOp::Equal => Ok(Imm::Bool(lhs == rhs)),
                        BinOp::NotEqual => Ok(Imm::Bool(lhs != rhs)),
                        BinOp::LessThan => Ok(Imm::Bool(!lhs && rhs)),
                        BinOp::LessThanOrEqual => Ok(Imm::Bool(lhs <= rhs)),
                        BinOp::GreaterThan => Ok(Imm::Bool(lhs && !rhs)),
                        BinOp::GreaterThanOrEqual => Ok(Imm::Bool(lhs >= rhs)),

                        // Logical
                        BinOp::LogicalAnd => Ok(Imm::Bool(lhs && rhs)),
                        BinOp::LogicalOr => Ok(Imm::Bool(lhs || rhs)),

                        _ => Err(handler.emit_internal_err(
                            "type error: invalid binary op for bools",
                            empty_span(),
                        )),
                    },

                    (Imm::B256(lhs), Imm::B256(rhs)) => match op {
                        // Equivalence.
                        BinOp::Equal => Ok(Imm::Bool(lhs == rhs)),
                        BinOp::NotEqual => Ok(Imm::Bool(lhs != rhs)),

                        _ => Err(handler.emit_internal_err(
                            "type error: invalid binary op for B256",
                            empty_span(),
                        )),
                    },

                    _ => Err(handler.emit_internal_err(
                        "compile-time evaluation binary op between some types \
                              not currently supported",
                        empty_span(),
                    )),
                }
            }

            Expr::Index { expr, index, span } => {
                // If the expr is an array...
                let ary = self.evaluate(*expr, handler, contract)?;
                if let Imm::Array(elements) = ary {
                    // And the index is an int...
                    match self.evaluate(*index, handler, contract)? {
                        Imm::Int(n) => elements.get(n as usize).cloned().ok_or_else(|| {
                            handler.emit_err(Error::Compile {
                                error: CompileError::ArrayIndexOutOfBounds { span: span.clone() },
                            })
                        }),

                        Imm::UnionVariant { tag_num, decl, .. }
                            if contract.unions[decl].is_enumeration_union() =>
                        {
                            elements.get(tag_num as usize).cloned().ok_or_else(|| {
                                handler.emit_err(Error::Compile {
                                    error: CompileError::ArrayIndexOutOfBounds {
                                        span: span.clone(),
                                    },
                                })
                            })
                        }

                        _ => Err(handler.emit_err(Error::Compile {
                            error: CompileError::InvalidConstArrayIndex { span: span.clone() },
                        })),
                    }
                } else {
                    Err(handler.emit_err(Error::Compile {
                        error: CompileError::CannotIndexIntoValue {
                            span: expr.get(contract).span().clone(),
                            index_span: span.clone(),
                        },
                    }))
                }
            }

            Expr::TupleFieldAccess { tuple, field, span } => {
                // If the expr is a tuple...
                let tup = self.evaluate(*tuple, handler, contract)?;
                if let Imm::Tuple(fields) = tup {
                    // And the field can be found...
                    match field {
                        TupleAccess::Index(n) => fields.get(*n).map(|pair| &pair.1),

                        TupleAccess::Name(id) => fields.iter().find_map(|(fld, e)| {
                            fld.as_ref()
                                .and_then(|fld| (fld.name == id.name).then_some(e))
                        }),

                        TupleAccess::Error => None,
                    }
                    .cloned()
                    .ok_or_else(|| {
                        let mut tuple_ty = tuple.get_ty(contract).clone();
                        if tuple_ty.is_unknown() {
                            tuple_ty = Imm::Tuple(fields).get_ty(Some(span));
                        }
                        handler.emit_err(Error::Compile {
                            error: CompileError::InvalidTupleAccessor {
                                accessor: field.to_string(),
                                tuple_type: contract.with_ctrct(tuple_ty).to_string(),
                                span: span.clone(),
                            },
                        })
                    })
                } else {
                    Err(handler.emit_err(Error::Compile {
                        error: CompileError::TupleAccessNonTuple {
                            non_tuple_type: contract.with_ctrct(tuple.get_ty(contract)).to_string(),
                            span: span.clone(),
                        },
                    }))
                }
            }

            Expr::Select {
                condition,
                then_expr,
                else_expr,
                span,
            } => {
                let cond = self.evaluate(*condition, handler, contract)?;
                if let Imm::Bool(b) = cond {
                    self.evaluate(if b { *then_expr } else { *else_expr }, handler, contract)
                } else {
                    let mut cond_ty = condition.get_ty(contract).clone();
                    if cond_ty.is_unknown() {
                        if let Expr::Immediate { value, .. } = condition.get(contract) {
                            cond_ty = value.get_ty(Some(span));
                        }
                    }

                    Err(handler.emit_err(Error::Compile {
                        error: CompileError::NonBoolConditional {
                            ty: contract.with_ctrct(cond_ty).to_string(),
                            conditional: "select expression".to_owned(),
                            span: span.clone(),
                        },
                    }))
                }
            }

            Expr::Cast { value, ty, span } => {
                let cast_error = |imm: Imm| -> Result<Imm, ErrorEmitted> {
                    let mut value_ty = value.get_ty(contract).clone();
                    if value_ty.is_unknown() {
                        value_ty = imm.get_ty(Some(span));
                    }

                    Err(handler.emit_err(Error::Compile {
                        error: CompileError::BadCastFrom {
                            ty: contract.with_ctrct(value_ty).to_string(),
                            span: span.clone(),
                        },
                    }))
                };

                // All casts are either redundant (e.g., bool as bool) or are to ints, except int
                // as real. They'll be rejected by the type checker if not.
                let imm = self.evaluate(*value, handler, contract)?;
                match imm {
                    Imm::Real(_) => {
                        if ty.is_real() {
                            Ok(imm)
                        } else {
                            cast_error(imm)
                        }
                    }

                    Imm::Int(i) => {
                        if ty.is_int() {
                            Ok(imm)
                        } else if ty.is_real() {
                            Ok(Imm::Real(i as f64))
                        } else {
                            cast_error(imm)
                        }
                    }

                    Imm::Bool(b) => {
                        if ty.is_bool() {
                            Ok(imm)
                        } else if ty.is_int() {
                            Ok(Imm::Int(if b { 1 } else { 0 }))
                        } else {
                            cast_error(imm)
                        }
                    }

                    // Union variants can be cast to `int` or `real` but only if the union is an
                    // enumeration
                    Imm::UnionVariant { tag_num, decl, .. } => {
                        let is_enumeration_union = contract.unions[decl].is_enumeration_union();

                        if is_enumeration_union && ty.is_int() {
                            Ok(Imm::Int(tag_num))
                        } else if is_enumeration_union && ty.is_real() {
                            Ok(Imm::Real(tag_num as f64))
                        } else {
                            cast_error(imm)
                        }
                    }

                    // Invalid to cast from these values.
                    Imm::String(_)
                    | Imm::B256(_)
                    | Imm::Array { .. }
                    | Imm::Tuple(_)
                    | Imm::Error => cast_error(imm),
                }
            }

            Expr::UnionVariant {
                path,
                path_span,
                value,
                ..
            } => {
                // Ugh, this isn't a great way to be getting the union type. :( Strings and all the
                // values/types which use them need to be refactored.  Or we get the type passed in
                // from evaluate().  This whole method is pretty sucky -- e.g, getting the tag
                // num and union size.
                let mut path_segs = path.split("::").collect::<Vec<_>>();
                path_segs.pop();
                let ty_path = path_segs.join("::");

                let (decl, _) = contract
                    .unions
                    .iter()
                    .find(|(_, union)| union.name.name == ty_path)
                    .ok_or_else(|| {
                        handler.emit_internal_err(
                            "Union decl for variant not found",
                            path_span.clone(),
                        )
                    })?;

                let union_ty = Type::Union {
                    decl,
                    span: empty_span(),
                };

                let tag_num = union_ty
                    .get_union_variant_names(contract)
                    .into_iter()
                    .enumerate()
                    .find_map(|(idx, variant_name)| (variant_name == path[2..]).then_some(idx))
                    .ok_or_else(|| {
                        handler.emit_internal_err("Union tag not found in union decl", empty_span())
                    })? as i64;

                let value = value
                    .as_ref()
                    .map(|value_key| self.evaluate(*value_key, handler, contract))
                    .transpose()?
                    .map(Box::new);

                Ok(Imm::UnionVariant {
                    tag_num,
                    value_size: union_ty.size(handler, contract)? - 1,
                    value,
                    decl,
                })
            }

            Expr::UnionTag { union_expr, span } => self
                .evaluate(*union_expr, handler, contract)
                .and_then(|imm| {
                    if let Imm::UnionVariant { tag_num, .. } = imm {
                        Ok(Imm::Int(tag_num))
                    } else {
                        Err(handler.emit_internal_err(
                            "unexpected expression during compile-time \
                                    evaluation of union expr (tag)",
                            span.clone(),
                        ))
                    }
                }),

            Expr::UnionValue {
                union_expr, span, ..
            } => self
                .evaluate(*union_expr, handler, contract)
                .and_then(|imm| {
                    if let Imm::UnionVariant {
                        value: Some(imm_val),
                        ..
                    } = imm
                    {
                        Ok(imm_val.as_ref().clone())
                    } else {
                        Err(handler.emit_internal_err(
                            "unexpected expression during compile-time \
                                    evaluation of union expr (value)",
                            span.clone(),
                        ))
                    }
                }),

            Expr::In {
                value, collection, ..
            } => {
                let value = self.evaluate(*value, handler, contract)?;

                if let Expr::Range { lb, ub, .. } = collection.get(contract) {
                    let lb = self.evaluate(*lb, handler, contract)?;
                    let ub = self.evaluate(*ub, handler, contract)?;

                    match (lb, ub, value) {
                        (Imm::Int(lb), Imm::Int(ub), Imm::Int(value)) => {
                            Ok(Imm::Bool(ub >= value && lb <= value))
                        }

                        (Imm::Real(lb), Imm::Real(ub), Imm::Real(value)) => {
                            Ok(Imm::Bool(ub >= value && lb <= value))
                        }

                        _ => Err(handler.emit_internal_err(
                            "unexpected expression during compile-time evaluation \
                            evaluation of in expr with range",
                            empty_span(),
                        )),
                    }
                } else {
                    let collection = self.evaluate(*collection, handler, contract)?;

                    match collection {
                        Imm::Array(collection) => Ok(Imm::Bool(collection.contains(&value))),

                        _ => Err(handler.emit_internal_err(
                            "unexpected expression during compile-time evaluation \
                            evaluation of in expr",
                            empty_span(),
                        )),
                    }
                }
            }

            Expr::ExternalStorageAccess { span, .. }
            | Expr::LocalStorageAccess { span, .. }
            | Expr::LocalPredicateCall { span, .. }
            | Expr::ExternalPredicateCall { span, .. }
            | Expr::Match { span, .. }
            | Expr::IntrinsicCall { span, .. }
            | Expr::Range { span, .. }
            | Expr::Generator { span, .. }
            | Expr::Map { span, .. }
            | Expr::KeyValue { span, .. }
            | Expr::Nil(span) => Err(handler.emit_err(Error::Compile {
                error: CompileError::InvalidConst { span: span.clone() },
            })),

            Expr::Error(_) | Expr::MacroCall { .. } | Expr::AsmBlock { .. } => Err(handler
                .emit_internal_err(
                    "unexpected expression during compile-time evaluation",
                    empty_span(),
                )),
        }
    }
}

impl ExprKey {
    /// Given an `ExprKey` `self`, a `Predicate`, and a map between some symbols and
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
    /// and inserts it (and its sub-expressions) into `pred.exprs`.
    pub(crate) fn plug_in(
        self,
        contract: &mut Contract,
        values_map: &FxHashMap<String, Imm>,
    ) -> ExprKey {
        let expr = self.get(contract).clone();

        let plugged = match expr {
            Expr::Immediate { .. } => expr,

            Expr::Array {
                elements,
                range_expr,
                span,
            } => {
                let elements = elements
                    .into_iter()
                    .map(|element| element.plug_in(contract, values_map))
                    .collect::<Vec<_>>();
                let range_expr = range_expr.plug_in(contract, values_map);

                Expr::Array {
                    elements,
                    range_expr,
                    span,
                }
            }

            Expr::Tuple { fields, span } => {
                let fields = fields
                    .into_iter()
                    .map(|(name, value)| (name, value.plug_in(contract, values_map)))
                    .collect::<Vec<_>>();

                Expr::Tuple { fields, span }
            }

            Expr::UnionVariant {
                path,
                path_span,
                value,
                span,
            } => {
                let value = value.map(|value| value.plug_in(contract, values_map));

                Expr::UnionVariant {
                    path,
                    path_span,
                    value,
                    span,
                }
            }

            Expr::KeyValue { lhs, rhs, span } => {
                let lhs = lhs.plug_in(contract, values_map);
                let rhs = rhs.plug_in(contract, values_map);

                Expr::KeyValue { lhs, rhs, span }
            }

            Expr::LocalStorageAccess { .. }
            | Expr::Nil(_)
            | Expr::MacroCall { .. }
            | Expr::Error(_) => expr,
            Expr::Path(ref path, ref span) => {
                let span = span.clone();
                values_map.get(path).map_or(expr, |value| Expr::Immediate {
                    value: value.clone(),
                    span,
                })
            }
            Expr::AsmBlock { args, ops, span } => {
                let args = args
                    .into_iter()
                    .map(|arg| arg.plug_in(contract, values_map))
                    .collect::<Vec<_>>();

                Expr::AsmBlock { args, ops, span }
            }
            Expr::ExternalStorageAccess {
                interface,
                address,
                name,
                span,
            } => {
                let address = address.plug_in(contract, values_map);

                Expr::ExternalStorageAccess {
                    interface,
                    address,
                    name,
                    span,
                }
            }
            Expr::UnaryOp { op, expr, span } => {
                let expr = expr.plug_in(contract, values_map);

                Expr::UnaryOp { op, expr, span }
            }
            Expr::BinaryOp { op, lhs, rhs, span } => {
                let lhs = lhs.plug_in(contract, values_map);
                let rhs = rhs.plug_in(contract, values_map);

                Expr::BinaryOp { op, lhs, rhs, span }
            }
            Expr::IntrinsicCall { kind, args, span } => {
                let args = args
                    .into_iter()
                    .map(|arg| arg.plug_in(contract, values_map))
                    .collect::<Vec<_>>();

                Expr::IntrinsicCall { kind, args, span }
            }
            Expr::ExternalPredicateCall {
                interface,
                c_addr,
                predicate,
                p_addr,
                args,
                span,
            } => {
                let c_addr = c_addr.plug_in(contract, values_map);
                let p_addr = p_addr.plug_in(contract, values_map);
                let args = args
                    .into_iter()
                    .map(|arg| arg.plug_in(contract, values_map))
                    .collect::<Vec<_>>();

                Expr::ExternalPredicateCall {
                    interface,
                    c_addr,
                    predicate,
                    p_addr,
                    args,
                    span,
                }
            }
            Expr::LocalPredicateCall {
                predicate,
                args,
                span,
            } => {
                let args = args
                    .into_iter()
                    .map(|arg| arg.plug_in(contract, values_map))
                    .collect::<Vec<_>>();

                Expr::LocalPredicateCall {
                    predicate,
                    args,
                    span,
                }
            }
            Expr::Select {
                condition,
                then_expr,
                else_expr,
                span,
            } => {
                let condition = condition.plug_in(contract, values_map);
                let then_expr = then_expr.plug_in(contract, values_map);
                let else_expr = else_expr.plug_in(contract, values_map);

                Expr::Select {
                    condition,
                    then_expr,
                    else_expr,
                    span,
                }
            }
            Expr::Index { expr, index, span } => {
                let expr = expr.plug_in(contract, values_map);
                let index = index.plug_in(contract, values_map);

                Expr::Index { expr, index, span }
            }
            Expr::TupleFieldAccess { tuple, field, span } => {
                let tuple = tuple.plug_in(contract, values_map);

                Expr::TupleFieldAccess { tuple, field, span }
            }
            Expr::Cast { value, ty, span } => {
                let value = value.plug_in(contract, values_map);

                Expr::Cast { value, ty, span }
            }
            Expr::In {
                value,
                collection,
                span,
            } => {
                let value = value.plug_in(contract, values_map);
                let collection = collection.plug_in(contract, values_map);

                Expr::In {
                    value,
                    collection,
                    span,
                }
            }
            Expr::Range { lb, ub, span } => {
                let lb = lb.plug_in(contract, values_map);
                let ub = ub.plug_in(contract, values_map);

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
                    .into_iter()
                    .map(|(index, range)| (index, range.plug_in(contract, values_map)))
                    .collect::<Vec<_>>();
                let conditions = conditions
                    .into_iter()
                    .map(|condition| condition.plug_in(contract, values_map))
                    .collect::<Vec<_>>();
                let body = body.plug_in(contract, values_map);

                Expr::Generator {
                    kind,
                    gen_ranges,
                    conditions,
                    body,
                    span,
                }
            }
            Expr::Map {
                param,
                range,
                body,
                span,
            } => {
                let range = range.plug_in(contract, values_map);
                let body = body.plug_in(contract, values_map);

                Expr::Map {
                    param,
                    range,
                    body,
                    span,
                }
            }
            Expr::Match {
                match_expr,
                match_branches,
                else_branch,
                span,
            } => {
                let match_expr = match_expr.plug_in(contract, values_map);
                let match_branches = match_branches
                    .into_iter()
                    .map(|match_branch| {
                        let constraints = match_branch
                            .constraints
                            .into_iter()
                            .map(|expr_key| expr_key.plug_in(contract, values_map))
                            .collect();
                        let expr = match_branch.expr.plug_in(contract, values_map);

                        MatchBranch {
                            constraints,
                            expr,
                            ..match_branch
                        }
                    })
                    .collect();
                let else_branch = else_branch.map(|else_branch| {
                    let constraints = else_branch
                        .constraints
                        .into_iter()
                        .map(|expr_key| expr_key.plug_in(contract, values_map))
                        .collect();
                    let expr = else_branch.expr.plug_in(contract, values_map);

                    MatchElse { constraints, expr }
                });

                Expr::Match {
                    match_expr,
                    match_branches,
                    else_branch,
                    span,
                }
            }

            Expr::UnionTag { union_expr, span } => {
                let union_expr = union_expr.plug_in(contract, values_map);

                Expr::UnionTag { union_expr, span }
            }
            Expr::UnionValue {
                union_expr,
                variant_ty,
                span,
            } => {
                let union_expr = union_expr.plug_in(contract, values_map);

                Expr::UnionValue {
                    union_expr,
                    variant_ty,
                    span,
                }
            }
        };

        // Insert the new plugged expression and its type.
        contract
            .exprs
            .insert(plugged, self.get_ty(contract).clone())
    }
}
