use super::{Contract, PredKey, Predicate};
use crate::{
    error::{ErrorEmitted, Handler},
    expr::{BinaryOp, Expr, InternalIntrinsic, IntrinsicKind, MatchBranch, MatchElse, UnaryOp},
    predicate::Immediate,
    span::{empty_span, Spanned},
    types::{PrimitiveKind, Type},
};
use fxhash::FxHashSet;
use std::collections::HashSet;

slotmap::new_key_type! { pub struct ExprKey; }

#[derive(Debug, Default, Clone)]
pub struct Exprs {
    exprs: slotmap::SlotMap<ExprKey, Expr>,
    expr_types: slotmap::SecondaryMap<ExprKey, Type>,
}

impl Exprs {
    /// Inserts an expression with its type
    pub fn insert(&mut self, expr: Expr, ty: Type) -> ExprKey {
        let key = self.exprs.insert(expr);
        self.expr_types.insert(key, ty);
        key
    }

    /// Removes an expression and its type given an `ExprKey`
    pub fn remove(&mut self, key: ExprKey) {
        self.exprs.remove(key);
        self.expr_types.remove(key);
    }

    /// Gets an `Expr` given its `ExprKey`
    pub fn get(&self, key: ExprKey) -> Option<&Expr> {
        self.exprs.get(key)
    }

    /// Apply function `f` on every expression
    /// Only the `Expr`s can be mutated, not the keys
    pub fn update_exprs(&mut self, f: impl FnOnce(ExprKey, &mut Expr) + std::marker::Copy) {
        for (key, expr) in self.exprs.iter_mut() {
            f(key, expr)
        }
    }

    /// Apply function `f` on every expression type
    /// Only the `Type`s can be mutated, not the keys
    pub fn update_types(&mut self, f: impl FnOnce(ExprKey, &mut Type) + std::marker::Copy) {
        for (key, ty) in self.expr_types.iter_mut() {
            f(key, ty)
        }
    }

    /// Inserts an integer expression with an empty span into the `exprs` map. Returns the
    /// `ExprKey` of the expression.
    pub fn insert_int(&mut self, i: i64) -> ExprKey {
        self.insert(
            Expr::Immediate {
                value: Immediate::Int(i),
                span: empty_span(),
            },
            Type::Primitive {
                kind: PrimitiveKind::Int,
                span: empty_span(),
            },
        )
    }

    /// Inserts a Boolean expression with an empty span into the `exprs` map. Returns the
    /// `ExprKey` of the expression.
    pub fn insert_bool(&mut self, b: bool) -> ExprKey {
        self.insert(
            Expr::Immediate {
                value: Immediate::Bool(b),
                span: empty_span(),
            },
            Type::Primitive {
                kind: PrimitiveKind::Bool,
                span: empty_span(),
            },
        )
    }
}

impl ExprKey {
    /// Returns an `Option` containing the `Expr` corresponding to key `self`. Returns `None` if
    /// the key can't be found in the `exprs` map.
    pub fn try_get<'a>(&self, contract: &'a Contract) -> Option<&'a Expr> {
        contract.exprs.exprs.get(*self)
    }

    /// Returns the `Expr` corresponding to key `self`. Panics if the key can't be found in the
    /// `exprs` map.
    pub fn get<'a>(&self, contract: &'a Contract) -> &'a Expr {
        contract.exprs.exprs.get(*self).unwrap()
    }

    /// Returns a mutable reference to the `Expr` corresponding to key `self`. Panics if the key
    /// can't be found in the `exprs` map.
    pub fn get_mut<'a>(&self, contract: &'a mut Contract) -> &'a mut Expr {
        contract.exprs.exprs.get_mut(*self).unwrap()
    }

    /// Returns the type of key `self` given a `Contract`. Panics if the type can't be
    /// found in the `expr_types` map.
    pub fn get_ty<'a>(&self, contract: &'a Contract) -> &'a Type {
        contract.exprs.expr_types.get(*self).unwrap()
    }

    /// Returns a mutable reference to the type of key `self` given a `Contract`. Panics if the
    /// type can't be found in the `expr_types` map.
    pub fn get_ty_mut<'a>(&self, contract: &'a mut Contract) -> &'a mut Type {
        contract.exprs.expr_types.get_mut(*self).unwrap()
    }

    /// Set the type of key `self` in a `Contract`. Panics if the type can't be found in the
    /// `expr_types` map.
    pub fn set_ty(&self, ty: Type, contract: &mut Contract) {
        contract.exprs.expr_types.insert(*self, ty);
    }

    /// Get the size of an expression. Usually, this is the accessesme as the size of the type but
    /// sometimes, the type doesn't have all the information we need.
    pub fn size(
        &self,
        handler: &Handler,
        contract: &Contract,
        pred: &Predicate,
    ) -> Result<usize, ErrorEmitted> {
        match self.get(contract) {
            Expr::KeyValue { lhs, rhs, .. } => {
                let key_size = if let Expr::IntrinsicCall {
                    kind: (IntrinsicKind::Internal(InternalIntrinsic::PreState), _),
                    args,
                    ..
                } = lhs.get(contract)
                {
                    args[0].size(handler, contract, pred)?
                } else {
                    return Err(handler.emit_internal_err(
                        "lhs for KeyValue has to be a __state intrinsic",
                        lhs.get(contract).span().clone(),
                    ));
                };

                let sizes = rhs.get_ty(contract).sizes(handler, contract)?;
                if rhs.get(contract).is_nil() {
                    Ok(1 + (1 + key_size) * sizes.len() + sizes.len())
                } else {
                    Ok(1 + (1 + key_size) * sizes.len()
                        + sizes.len()
                        + rhs.size(handler, contract, pred)?)
                }
            }

            Expr::Path(path, _) if self.get_ty(contract).is_key_value() => {
                let Some((_, var)) = pred
                    .variables()
                    .find(|(_, variable)| &variable.name == path)
                else {
                    return Err(handler.emit_internal_err(
                        "path expr of KeyValue type must refer to a local variable ",
                        self.get(contract).span().clone(),
                    ));
                };
                var.expr.size(handler, contract, pred)
            }

            Expr::BinaryOp { op, lhs, rhs, .. }
                if *op == BinaryOp::Concat
                    && lhs.get_ty(contract).is_key_value()
                    && rhs.get_ty(contract).is_key_value() =>
            {
                Ok(lhs.size(handler, contract, pred)? + rhs.size(handler, contract, pred)? - 1)
            }

            // All other cases can rely solely on the type
            _ => self.get_ty(contract).size(handler, contract),
        }
    }

    /// Return whether this expression can panic (typically related to storage).
    pub fn can_panic(&self, contract: &Contract, pred: &Predicate) -> bool {
        contract.exprs.get(*self).is_some_and(|expr| match expr {
            Expr::LocalStorageAccess { .. } | Expr::ExternalStorageAccess { .. } => true,

            Expr::Path(path, _) => pred.variables().any(|(_, variable)| &variable.name == path),

            Expr::AsmBlock { args, .. } => args.iter().any(|arg| arg.can_panic(contract, pred)),

            Expr::Error(_) | Expr::Immediate { .. } | Expr::MacroCall { .. } => false,

            Expr::Array {
                elements,
                range_expr,
                ..
            } => {
                elements.iter().any(|el| el.can_panic(contract, pred))
                    || range_expr.can_panic(contract, pred)
            }

            Expr::Tuple { fields, .. } => fields.iter().any(|fld| fld.1.can_panic(contract, pred)),

            Expr::UnionVariant { value, .. } => value
                .map(|value| value.can_panic(contract, pred))
                .unwrap_or(false),

            Expr::KeyValue { lhs, rhs, .. } => {
                lhs.can_panic(contract, pred) || rhs.can_panic(contract, pred)
            }

            Expr::Nil(_) => false,

            Expr::UnaryOp { expr, .. } => expr.can_panic(contract, pred),

            Expr::BinaryOp { lhs, rhs, .. } => {
                lhs.can_panic(contract, pred) || rhs.can_panic(contract, pred)
            }

            Expr::IntrinsicCall { args, .. } => {
                self.is_storage_access_intrinsic(contract)
                    || self.is_storage_vec_len(contract)
                    || args.iter().any(|arg| arg.can_panic(contract, pred))
            }

            Expr::LocalPredicateCall { args, .. } => {
                args.iter().any(|arg| arg.can_panic(contract, pred))
            }

            Expr::ExternalPredicateCall {
                c_addr,
                p_addr,
                args,
                ..
            } => {
                c_addr.can_panic(contract, pred)
                    || p_addr.can_panic(contract, pred)
                    || args.iter().any(|arg| arg.can_panic(contract, pred))
            }

            Expr::Select {
                condition,
                then_expr,
                else_expr,
                ..
            } => {
                condition.can_panic(contract, pred)
                    || then_expr.can_panic(contract, pred)
                    || else_expr.can_panic(contract, pred)
            }

            Expr::Match {
                match_expr,
                match_branches,
                else_branch,
                ..
            } => {
                match_expr.can_panic(contract, pred)
                    || match_branches.iter().any(
                        |MatchBranch {
                             constraints, expr, ..
                         }| {
                            constraints
                                .iter()
                                .any(|c_expr| c_expr.can_panic(contract, pred))
                                || expr.can_panic(contract, pred)
                        },
                    )
                    || else_branch
                        .as_ref()
                        .map(|MatchElse { constraints, expr }| {
                            constraints
                                .iter()
                                .any(|c_expr| c_expr.can_panic(contract, pred))
                                || expr.can_panic(contract, pred)
                        })
                        .unwrap_or(false)
            }

            Expr::Index { expr, index, .. } => {
                expr.can_panic(contract, pred) || index.can_panic(contract, pred)
            }

            Expr::TupleFieldAccess { tuple, .. } => tuple.can_panic(contract, pred),

            Expr::Cast { value, .. } => value.can_panic(contract, pred),

            Expr::In {
                value, collection, ..
            } => value.can_panic(contract, pred) || collection.can_panic(contract, pred),

            Expr::Range { lb, ub, .. } => {
                lb.can_panic(contract, pred) || ub.can_panic(contract, pred)
            }

            Expr::Generator {
                gen_ranges,
                conditions,
                body,
                ..
            } => {
                gen_ranges.iter().any(|rng| rng.1.can_panic(contract, pred))
                    || conditions.iter().any(|cond| cond.can_panic(contract, pred))
                    || body.can_panic(contract, pred)
            }

            Expr::Map { range, body, .. } => {
                range.can_panic(contract, pred) || body.can_panic(contract, pred)
            }

            Expr::UnionTag { union_expr, .. } | Expr::UnionValue { union_expr, .. } => {
                union_expr.can_panic(contract, pred)
            }
        })
    }

    /// Collect all storage accesses in an expression. For example, given the following expression:
    /// `{ storage::map[3].2, storage::y, [storage::z.2] }`, the method `collect_storage_accesses`
    /// returns the following storage accesses:
    ///
    /// - `storage::map[3].2`
    /// - `storage::y`
    /// - `storage::z.2`
    ///
    /// This function also returns the storage accesses on the LHS of `KeyValue` expressions.
    /// Those are "write-only" storage accesses. Note that the first set collected is a *superset*
    /// of the second set. That is, the first set also contains the "write-only" accesses.
    pub fn collect_storage_accesses(
        &self,
        contract: &Contract,
    ) -> (FxHashSet<ExprKey>, FxHashSet<ExprKey>) {
        let mut storage_accesses = FxHashSet::default();
        let mut write_only_storage_accesses = FxHashSet::default();

        if let Some(expr) = self.try_get(contract) {
            match expr {
                Expr::Array { elements, .. } => {
                    for e in elements {
                        let (accesses, write_only) = e.collect_storage_accesses(contract);
                        storage_accesses.extend(accesses);
                        write_only_storage_accesses.extend(write_only);
                    }
                }

                Expr::Tuple { fields, .. } => {
                    for (_, f) in fields {
                        let (accesses, write_only) = f.collect_storage_accesses(contract);
                        storage_accesses.extend(accesses);
                        write_only_storage_accesses.extend(write_only);
                    }
                }

                Expr::UnionVariant { value, .. } => {
                    if let Some(value) = value {
                        let (accesses, write_only) = value.collect_storage_accesses(contract);
                        storage_accesses.extend(accesses);
                        write_only_storage_accesses.extend(write_only);
                    }
                }

                Expr::KeyValue { lhs, rhs, .. } => {
                    write_only_storage_accesses.insert(*lhs);
                    let (lhs_accesses, lhs_write_only) = lhs.collect_storage_accesses(contract);
                    let (rhs_accesses, rhs_write_only) = rhs.collect_storage_accesses(contract);
                    storage_accesses.extend(lhs_accesses);
                    storage_accesses.extend(rhs_accesses);
                    write_only_storage_accesses.extend(lhs_write_only);
                    write_only_storage_accesses.extend(rhs_write_only);
                }

                Expr::LocalStorageAccess { .. } | Expr::ExternalStorageAccess { .. } => {
                    storage_accesses.insert(*self);
                }

                Expr::UnaryOp {
                    op: UnaryOp::NextState,
                    expr,
                    ..
                } => {
                    if expr.is_storage_access(contract) {
                        storage_accesses.insert(*self);
                    } else {
                        let (accesses, write_only) = expr.collect_storage_accesses(contract);
                        storage_accesses.extend(accesses);
                        write_only_storage_accesses.extend(write_only);
                    }
                }

                Expr::UnaryOp { expr, .. } => {
                    let (accesses, write_only) = expr.collect_storage_accesses(contract);
                    storage_accesses.extend(accesses);
                    write_only_storage_accesses.extend(write_only);
                }

                Expr::BinaryOp { lhs, rhs, .. } => {
                    let (lhs_accesses, lhs_write_only) = lhs.collect_storage_accesses(contract);
                    let (rhs_accesses, rhs_write_only) = rhs.collect_storage_accesses(contract);
                    storage_accesses.extend(lhs_accesses);
                    storage_accesses.extend(rhs_accesses);
                    write_only_storage_accesses.extend(lhs_write_only);
                    write_only_storage_accesses.extend(rhs_write_only);
                }

                Expr::IntrinsicCall { args, .. } => {
                    if self.is_storage_access_intrinsic(contract)
                        || self.is_storage_vec_len(contract)
                    {
                        storage_accesses.insert(*self);
                    } else {
                        for arg in args {
                            let (accesses, write_only) = arg.collect_storage_accesses(contract);
                            storage_accesses.extend(accesses);
                            write_only_storage_accesses.extend(write_only);
                        }
                    }
                }

                Expr::AsmBlock { args, .. } => {
                    for arg in args {
                        let (accesses, write_only) = arg.collect_storage_accesses(contract);
                        storage_accesses.extend(accesses);
                        write_only_storage_accesses.extend(write_only);
                    }
                }

                Expr::ExternalPredicateCall {
                    c_addr,
                    p_addr,
                    args,
                    ..
                } => {
                    let (c_accesses, c_write_only) = c_addr.collect_storage_accesses(contract);
                    let (p_accesses, p_write_only) = p_addr.collect_storage_accesses(contract);
                    storage_accesses.extend(c_accesses);
                    storage_accesses.extend(p_accesses);
                    write_only_storage_accesses.extend(c_write_only);
                    write_only_storage_accesses.extend(p_write_only);
                    for arg in args {
                        let (accesses, write_only) = arg.collect_storage_accesses(contract);
                        storage_accesses.extend(accesses);
                        write_only_storage_accesses.extend(write_only);
                    }
                }

                Expr::LocalPredicateCall { args, .. } => {
                    for arg in args {
                        let (accesses, write_only) = arg.collect_storage_accesses(contract);
                        storage_accesses.extend(accesses);
                        write_only_storage_accesses.extend(write_only);
                    }
                }

                Expr::Select {
                    condition,
                    then_expr,
                    else_expr,
                    ..
                } => {
                    let (cond_accesses, cond_write_only) =
                        condition.collect_storage_accesses(contract);
                    let (then_accesses, then_write_only) =
                        then_expr.collect_storage_accesses(contract);
                    let (else_accesses, else_write_only) =
                        else_expr.collect_storage_accesses(contract);
                    storage_accesses.extend(cond_accesses);
                    storage_accesses.extend(then_accesses);
                    storage_accesses.extend(else_accesses);
                    write_only_storage_accesses.extend(cond_write_only);
                    write_only_storage_accesses.extend(then_write_only);
                    write_only_storage_accesses.extend(else_write_only);
                }

                Expr::Index {
                    expr: expr_inner, ..
                } => {
                    let (inner_accesses, inner_write_only) =
                        expr_inner.collect_storage_accesses(contract);
                    storage_accesses.extend(&inner_accesses);
                    write_only_storage_accesses.extend(inner_write_only);
                    if storage_accesses.remove(expr_inner) {
                        storage_accesses.insert(*self);
                    }
                }

                Expr::TupleFieldAccess { tuple, .. } => {
                    let (tuple_accesses, tuple_write_only) =
                        tuple.collect_storage_accesses(contract);
                    storage_accesses.extend(&tuple_accesses);
                    write_only_storage_accesses.extend(tuple_write_only);
                    if storage_accesses.remove(tuple) {
                        storage_accesses.insert(*self);
                    }
                }

                Expr::Cast { value, .. } => {
                    let (accesses, write_only) = value.collect_storage_accesses(contract);
                    storage_accesses.extend(accesses);
                    write_only_storage_accesses.extend(write_only);
                }

                Expr::In {
                    value, collection, ..
                } => {
                    let (val_accesses, val_write_only) = value.collect_storage_accesses(contract);
                    let (col_accesses, col_write_only) =
                        collection.collect_storage_accesses(contract);
                    storage_accesses.extend(val_accesses);
                    storage_accesses.extend(col_accesses);
                    write_only_storage_accesses.extend(val_write_only);
                    write_only_storage_accesses.extend(col_write_only);
                }

                Expr::Range { lb, ub, .. } => {
                    let (lb_accesses, lb_write_only) = lb.collect_storage_accesses(contract);
                    let (ub_accesses, ub_write_only) = ub.collect_storage_accesses(contract);
                    storage_accesses.extend(lb_accesses);
                    storage_accesses.extend(ub_accesses);
                    write_only_storage_accesses.extend(lb_write_only);
                    write_only_storage_accesses.extend(ub_write_only);
                }

                Expr::Generator {
                    gen_ranges,
                    conditions,
                    body,
                    ..
                } => {
                    for (_, range) in gen_ranges {
                        let (accesses, write_only) = range.collect_storage_accesses(contract);
                        storage_accesses.extend(accesses);
                        write_only_storage_accesses.extend(write_only);
                    }
                    for cond in conditions {
                        let (accesses, write_only) = cond.collect_storage_accesses(contract);
                        storage_accesses.extend(accesses);
                        write_only_storage_accesses.extend(write_only);
                    }
                    let (body_accesses, body_write_only) = body.collect_storage_accesses(contract);
                    storage_accesses.extend(body_accesses);
                    write_only_storage_accesses.extend(body_write_only);
                }

                Expr::Map { range, body, .. } => {
                    let (range_accesses, range_write_only) =
                        range.collect_storage_accesses(contract);
                    let (body_accesses, body_write_only) = body.collect_storage_accesses(contract);
                    storage_accesses.extend(range_accesses);
                    storage_accesses.extend(body_accesses);
                    write_only_storage_accesses.extend(range_write_only);
                    write_only_storage_accesses.extend(body_write_only);
                }

                Expr::UnionTag { union_expr, .. } => {
                    let (accesses, write_only) = union_expr.collect_storage_accesses(contract);
                    storage_accesses.extend(accesses);
                    write_only_storage_accesses.extend(write_only);
                }

                Expr::UnionValue { union_expr, .. } => {
                    let (accesses, write_only) = union_expr.collect_storage_accesses(contract);
                    storage_accesses.extend(accesses);
                    write_only_storage_accesses.extend(write_only);
                }

                Expr::Error(_)
                | Expr::Nil(_)
                | Expr::Immediate { .. }
                | Expr::Path(..)
                | Expr::MacroCall { .. }
                | Expr::Match { .. } => {}
            }
        }

        (storage_accesses, write_only_storage_accesses)
    }

    /// Given an expression, collect all accesses to local variables
    ///
    pub fn collect_path_to_var_exprs(
        &self,
        contract: &Contract,
        pred: &Predicate,
    ) -> FxHashSet<String> {
        let mut path_to_var_exprs = FxHashSet::default();

        if let Some(expr) = self.try_get(contract) {
            match expr {
                Expr::Array { elements, .. } => {
                    elements.iter().for_each(|e| {
                        path_to_var_exprs.extend(e.collect_path_to_var_exprs(contract, pred));
                    });
                }

                Expr::Tuple { fields, .. } => {
                    fields.iter().for_each(|(_, f)| {
                        path_to_var_exprs.extend(f.collect_path_to_var_exprs(contract, pred));
                    });
                }

                Expr::UnionVariant { value, .. } => {
                    if let Some(value) = value {
                        path_to_var_exprs.extend(value.collect_path_to_var_exprs(contract, pred));
                    }
                }

                Expr::KeyValue { lhs, rhs, .. } => {
                    path_to_var_exprs.extend(lhs.collect_path_to_var_exprs(contract, pred));
                    path_to_var_exprs.extend(rhs.collect_path_to_var_exprs(contract, pred));
                }

                Expr::Path(path, _) => {
                    // collect paths to variables
                    if pred.variables().any(|(_, var)| &var.name == path) {
                        path_to_var_exprs.insert(path.clone());
                    }
                }

                Expr::ExternalStorageAccess { address, .. } => {
                    path_to_var_exprs.extend(address.collect_path_to_var_exprs(contract, pred))
                }

                Expr::UnaryOp { op, expr, .. } => {
                    if *op == UnaryOp::NextState {
                        // collect "next state" paths to variables
                        if let Expr::Path(path, _) = expr.get(contract) {
                            if pred.variables().any(|(_, var)| &var.name == path) {
                                path_to_var_exprs.insert(path.to_owned());
                            }
                        }
                    } else {
                        path_to_var_exprs.extend(expr.collect_path_to_var_exprs(contract, pred));
                    }
                }

                Expr::BinaryOp { lhs, rhs, .. } => {
                    path_to_var_exprs.extend(lhs.collect_path_to_var_exprs(contract, pred));
                    path_to_var_exprs.extend(rhs.collect_path_to_var_exprs(contract, pred));
                }

                Expr::IntrinsicCall { args, .. } => {
                    args.iter().for_each(|arg| {
                        path_to_var_exprs.extend(arg.collect_path_to_var_exprs(contract, pred));
                    });
                }

                Expr::AsmBlock { args, .. } => {
                    args.iter().for_each(|arg| {
                        path_to_var_exprs.extend(arg.collect_path_to_var_exprs(contract, pred));
                    });
                }

                Expr::ExternalPredicateCall {
                    c_addr,
                    p_addr,
                    args,
                    ..
                } => {
                    path_to_var_exprs.extend(c_addr.collect_path_to_var_exprs(contract, pred));
                    path_to_var_exprs.extend(p_addr.collect_path_to_var_exprs(contract, pred));
                    args.iter().for_each(|arg| {
                        path_to_var_exprs.extend(arg.collect_path_to_var_exprs(contract, pred));
                    });
                }

                Expr::LocalPredicateCall { args, .. } => {
                    args.iter().for_each(|arg| {
                        path_to_var_exprs.extend(arg.collect_path_to_var_exprs(contract, pred));
                    });
                }

                Expr::Select {
                    condition,
                    then_expr,
                    else_expr,
                    ..
                } => {
                    path_to_var_exprs.extend(condition.collect_path_to_var_exprs(contract, pred));
                    path_to_var_exprs.extend(then_expr.collect_path_to_var_exprs(contract, pred));
                    path_to_var_exprs.extend(else_expr.collect_path_to_var_exprs(contract, pred));
                }

                Expr::Index {
                    expr: expr_inner, ..
                } => {
                    path_to_var_exprs.extend(expr_inner.collect_path_to_var_exprs(contract, pred));
                }

                Expr::TupleFieldAccess { tuple, .. } => {
                    path_to_var_exprs.extend(tuple.collect_path_to_var_exprs(contract, pred));
                }

                Expr::Cast { value, .. } => {
                    path_to_var_exprs.extend(value.collect_path_to_var_exprs(contract, pred));
                }

                Expr::In {
                    value, collection, ..
                } => {
                    path_to_var_exprs.extend(value.collect_path_to_var_exprs(contract, pred));
                    path_to_var_exprs.extend(collection.collect_path_to_var_exprs(contract, pred));
                }

                Expr::Range { lb, ub, .. } => {
                    path_to_var_exprs.extend(lb.collect_path_to_var_exprs(contract, pred));
                    path_to_var_exprs.extend(ub.collect_path_to_var_exprs(contract, pred));
                }

                Expr::Generator {
                    gen_ranges,
                    conditions,
                    body,
                    ..
                } => {
                    gen_ranges.iter().for_each(|(_, range)| {
                        path_to_var_exprs.extend(range.collect_path_to_var_exprs(contract, pred));
                    });
                    conditions.iter().for_each(|condition| {
                        path_to_var_exprs
                            .extend(condition.collect_path_to_var_exprs(contract, pred));
                    });
                    path_to_var_exprs.extend(body.collect_path_to_var_exprs(contract, pred));
                }

                Expr::Map { range, body, .. } => {
                    path_to_var_exprs.extend(range.collect_path_to_var_exprs(contract, pred));
                    path_to_var_exprs.extend(body.collect_path_to_var_exprs(contract, pred));
                }

                Expr::UnionTag { union_expr, .. } => {
                    path_to_var_exprs.extend(union_expr.collect_path_to_var_exprs(contract, pred));
                }

                Expr::UnionValue { union_expr, .. } => {
                    path_to_var_exprs.extend(union_expr.collect_path_to_var_exprs(contract, pred));
                }

                Expr::LocalStorageAccess { .. }
                | Expr::Nil(_)
                | Expr::Error(_)
                | Expr::Immediate { .. }
                | Expr::MacroCall { .. }
                | Expr::Match { .. } => {}
            }
        }

        path_to_var_exprs
    }

    pub fn is_storage_vec_len(&self, contract: &Contract) -> bool {
        self.get(contract).is_storage_vec_len(contract)
    }

    pub fn is_storage_access_intrinsic(&self, contract: &Contract) -> bool {
        self.get(contract).is_storage_access_intrinsic()
    }

    pub fn is_pre_storage_access_intrinsic(&self, contract: &Contract) -> bool {
        self.get(contract).is_pre_storage_access_intrinsic()
    }

    pub fn is_post_storage_access_intrinsic(&self, contract: &Contract) -> bool {
        self.get(contract).is_post_storage_access_intrinsic()
    }

    pub fn is_pre_storage_access(&self, contract: &Contract) -> bool {
        self.get(contract).is_pre_storage_access(contract)
    }

    pub fn is_post_storage_access(&self, contract: &Contract) -> bool {
        self.get(contract).is_post_storage_access(contract)
    }

    pub fn is_storage_access(&self, contract: &Contract) -> bool {
        self.get(contract).is_storage_access(contract)
    }
}

/// [`ExprsIter`] is an iterator for all the _reachable_ expressions in the Predicate.
///
/// Items are popped off the queue and are returned next.  If they're a branch then their children
/// are queued.  The visited set is updated and used to avoid following a branch multiple
/// times.
///
/// An alternative is to use `[Predicate::visitor]` which will also iterate
/// for each reachable expression but does not implement `Iterator` and instead takes a closure.

#[derive(Debug)]
pub(crate) struct ExprsIter<'a> {
    contract: &'a Contract,
    queue: Vec<ExprKey>,
    visited: HashSet<ExprKey>,
}

impl<'a> ExprsIter<'a> {
    pub(super) fn new(contract: &'a Contract, pred_key: PredKey) -> ExprsIter<'a> {
        // We start with all the constraint and variable exprs.
        let queue = contract.root_set(pred_key).collect();

        ExprsIter {
            contract,
            queue,
            visited: HashSet::default(),
        }
    }

    pub(super) fn new_by_expr_set(
        contract: &'a Contract,
        pred_key: Option<PredKey>,
        with_consts: bool,
        with_array_ranges: bool,
    ) -> ExprsIter<'a> {
        let mut queue = Vec::default();

        // Add the predicate root set.
        if let Some(pred_key) = pred_key {
            queue.extend(contract.root_set(pred_key));
        }

        // Add the consts.
        if with_consts {
            queue.extend(
                contract
                    .consts
                    .iter()
                    .map(|(_, super::Const { expr, .. })| expr),
            );
        }

        // Add the array range expressions from storage, interfaces and new-types.
        if with_array_ranges {
            queue.extend(contract.root_array_range_exprs());
        }

        ExprsIter {
            contract,
            queue,
            visited: HashSet::default(),
        }
    }
}

impl Iterator for ExprsIter<'_> {
    type Item = ExprKey;

    fn next(&mut self) -> Option<Self::Item> {
        if self.queue.is_empty() {
            return None;
        }

        macro_rules! queue_if_new {
            ($self: ident, $key: expr) => {
                if !$self.visited.contains($key) {
                    $self.queue.push(*$key);
                }
            };
        }

        // Get the next key and mark it as visited.
        let next_key = self.queue.pop().unwrap();
        self.visited.insert(next_key);

        // Keep macro calls that do not return as expressions but used as expressions. We should
        // error out early when we find these anyways, but we'll do it in type checking.
        if self.contract.removed_macro_calls.get(next_key).is_some() {
            return Some(next_key);
        }

        // Push its children to the queue.
        match next_key.get(self.contract) {
            Expr::Immediate { .. } => {}

            Expr::Array {
                elements,
                range_expr,
                ..
            } => {
                for el in elements {
                    queue_if_new!(self, el);
                }
                queue_if_new!(self, range_expr);
            }

            Expr::Tuple { fields, .. } => {
                for (_, field) in fields {
                    queue_if_new!(self, field);
                }
            }

            Expr::UnionVariant { value, .. } => {
                if let Some(value) = value {
                    queue_if_new!(self, value);
                }
            }

            Expr::KeyValue { lhs, rhs, .. } => {
                queue_if_new!(self, lhs);
                queue_if_new!(self, rhs);
            }

            Expr::ExternalStorageAccess { address, .. } => queue_if_new!(self, address),

            Expr::UnaryOp { expr, .. } => queue_if_new!(self, expr),

            Expr::BinaryOp { lhs, rhs, .. } => {
                queue_if_new!(self, lhs);
                queue_if_new!(self, rhs);
            }

            Expr::IntrinsicCall { args, .. } => {
                for arg in args {
                    queue_if_new!(self, arg);
                }
            }

            Expr::AsmBlock { args, .. } => {
                for arg in args {
                    queue_if_new!(self, arg);
                }
            }

            Expr::ExternalPredicateCall {
                c_addr,
                p_addr,
                args,
                ..
            } => {
                queue_if_new!(self, c_addr);
                queue_if_new!(self, p_addr);
                for arg in args {
                    queue_if_new!(self, arg);
                }
            }

            Expr::LocalPredicateCall { args, .. } => {
                for arg in args {
                    queue_if_new!(self, arg);
                }
            }

            Expr::Select {
                condition,
                then_expr,
                else_expr,
                ..
            } => {
                queue_if_new!(self, condition);
                queue_if_new!(self, then_expr);
                queue_if_new!(self, else_expr);
            }

            Expr::Match {
                match_expr,
                match_branches,
                else_branch,
                ..
            } => {
                queue_if_new!(self, match_expr);

                for MatchBranch {
                    constraints, expr, ..
                } in match_branches
                {
                    for c_expr in constraints {
                        queue_if_new!(self, c_expr);
                    }
                    queue_if_new!(self, expr);
                }

                if let Some(MatchElse {
                    constraints, expr, ..
                }) = else_branch
                {
                    for c_expr in constraints {
                        queue_if_new!(self, c_expr);
                    }
                    queue_if_new!(self, expr);
                }
            }

            Expr::Index { expr, index, .. } => {
                queue_if_new!(self, expr);
                queue_if_new!(self, index);
            }

            Expr::TupleFieldAccess { tuple, .. } => queue_if_new!(self, tuple),

            Expr::Cast { value, .. } => queue_if_new!(self, value),

            Expr::In {
                value, collection, ..
            } => {
                queue_if_new!(self, value);
                queue_if_new!(self, collection);
            }

            Expr::Range { lb, ub, .. } => {
                queue_if_new!(self, lb);
                queue_if_new!(self, ub);
            }

            Expr::Generator {
                gen_ranges,
                conditions,
                body,
                ..
            } => {
                for (_, range) in gen_ranges {
                    queue_if_new!(self, range);
                }

                for cond in conditions {
                    queue_if_new!(self, cond);
                }

                queue_if_new!(self, body);
            }

            Expr::Map { range, body, .. } => {
                queue_if_new!(self, range);
                queue_if_new!(self, body);
            }

            Expr::UnionTag { union_expr, .. } | Expr::UnionValue { union_expr, .. } => {
                queue_if_new!(self, union_expr)
            }

            Expr::Error(_)
            | Expr::Nil(_)
            | Expr::LocalStorageAccess { .. }
            | Expr::Path(_, _)
            | Expr::MacroCall { .. } => {}
        };

        // If it has an array type then it also has an associated expr in the range.
        next_key
            .get_ty(self.contract)
            .get_array_range_expr()
            .iter()
            .for_each(|range| queue_if_new!(self, range));

        Some(next_key)
    }
}
