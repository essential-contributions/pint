use super::{Contract, PredKey, Predicate};
use crate::{
    expr::{Expr, ExternalIntrinsic, InternalIntrinsic, IntrinsicKind, MatchBranch, MatchElse},
    predicate::Immediate,
    span::empty_span,
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
    pub fn try_get<'a>(&'a self, contract: &'a Contract) -> Option<&Expr> {
        contract.exprs.exprs.get(*self)
    }

    /// Returns the `Expr` corresponding to key `self`. Panics if the key can't be found in the
    /// `exprs` map.
    pub fn get<'a>(&'a self, contract: &'a Contract) -> &Expr {
        contract.exprs.exprs.get(*self).unwrap()
    }

    /// Returns a mutable reference to the `Expr` corresponding to key `self`. Panics if the key
    /// can't be found in the `exprs` map.
    pub fn get_mut<'a>(&'a self, contract: &'a mut Contract) -> &mut Expr {
        contract.exprs.exprs.get_mut(*self).unwrap()
    }

    /// Returns the type of key `self` given a `Contract`. Panics if the type can't be
    /// found in the `expr_types` map.
    pub fn get_ty<'a>(&'a self, contract: &'a Contract) -> &Type {
        contract.exprs.expr_types.get(*self).unwrap()
    }

    /// Returns a mutable reference to the type of key `self` given a `Contract`. Panics if the type can't be
    /// found in the `expr_types` map.
    pub fn get_ty_mut<'a>(&'a self, contract: &'a mut Contract) -> &mut Type {
        contract.exprs.expr_types.get_mut(*self).unwrap()
    }

    /// Set the type of key `self` in a `Contract`. Panics if the type can't be found in
    /// the `expr_types` map.
    pub fn set_ty<'a>(&'a self, ty: Type, contract: &'a mut Contract) {
        contract.exprs.expr_types.insert(*self, ty);
    }

    /// Return whether this expression can panic (typically related to storage).
    pub fn can_panic(&self, contract: &Contract, pred: &Predicate) -> bool {
        contract.exprs.get(*self).map_or(false, |expr| match expr {
            Expr::StorageAccess { .. } | Expr::ExternalStorageAccess { .. } => true,

            Expr::Path(path, _) => pred.states().any(|(_, state)| &state.name == path),

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

            Expr::UnaryOp { expr, .. } => expr.can_panic(contract, pred),

            Expr::BinaryOp { lhs, rhs, .. } => {
                lhs.can_panic(contract, pred) || rhs.can_panic(contract, pred)
            }

            Expr::IntrinsicCall { kind, args, .. } => {
                matches!(
                    kind.0,
                    IntrinsicKind::Internal(
                        InternalIntrinsic::StorageGet | InternalIntrinsic::StorageGetExtern
                    ) | IntrinsicKind::External(ExternalIntrinsic::VecLen)
                ) || args.iter().any(|arg| arg.can_panic(contract, pred))
            }

            Expr::PredicateCall {
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
    pub fn collect_storage_accesses(&self, contract: &Contract) -> FxHashSet<ExprKey> {
        let mut storage_accesses = FxHashSet::default();

        if let Some(expr) = self.try_get(contract) {
            match expr {
                Expr::Array { elements, .. } => {
                    elements.iter().for_each(|e| {
                        storage_accesses.extend(e.collect_storage_accesses(contract));
                    });
                }

                Expr::Tuple { fields, .. } => {
                    fields.iter().for_each(|(_, f)| {
                        storage_accesses.extend(f.collect_storage_accesses(contract));
                    });
                }

                Expr::UnionVariant { value, .. } => {
                    if let Some(value) = value {
                        storage_accesses.extend(value.collect_storage_accesses(contract));
                    }
                }

                Expr::StorageAccess { .. } | Expr::ExternalStorageAccess { .. } => {
                    storage_accesses.insert(*self);
                }

                Expr::UnaryOp { expr, .. } => {
                    storage_accesses.extend(expr.collect_storage_accesses(contract));
                }

                Expr::BinaryOp { lhs, rhs, .. } => {
                    storage_accesses.extend(lhs.collect_storage_accesses(contract));
                    storage_accesses.extend(rhs.collect_storage_accesses(contract));
                }

                Expr::IntrinsicCall { kind, args, .. } => {
                    if let (
                        IntrinsicKind::External(ExternalIntrinsic::VecLen)
                        | IntrinsicKind::Internal(
                            InternalIntrinsic::StorageGet | InternalIntrinsic::StorageGetExtern,
                        ),
                        _,
                    ) = kind
                    {
                        storage_accesses.insert(*self);
                    } else {
                        args.iter().for_each(|arg| {
                            storage_accesses.extend(arg.collect_storage_accesses(contract));
                        });
                    }
                }

                Expr::PredicateCall {
                    c_addr,
                    p_addr,
                    args,
                    ..
                } => {
                    storage_accesses.extend(c_addr.collect_storage_accesses(contract));
                    storage_accesses.extend(p_addr.collect_storage_accesses(contract));
                    args.iter().for_each(|arg| {
                        storage_accesses.extend(arg.collect_storage_accesses(contract));
                    });
                }

                Expr::Select {
                    condition,
                    then_expr,
                    else_expr,
                    ..
                } => {
                    storage_accesses.extend(condition.collect_storage_accesses(contract));
                    storage_accesses.extend(then_expr.collect_storage_accesses(contract));
                    storage_accesses.extend(else_expr.collect_storage_accesses(contract));
                }

                Expr::Index {
                    expr: expr_inner, ..
                } => {
                    storage_accesses.extend(expr_inner.collect_storage_accesses(contract));
                    if storage_accesses.remove(expr_inner) {
                        storage_accesses.insert(*self);
                    }
                }

                Expr::TupleFieldAccess { tuple, .. } => {
                    storage_accesses.extend(tuple.collect_storage_accesses(contract));
                    if storage_accesses.remove(tuple) {
                        storage_accesses.insert(*self);
                    }
                }

                Expr::Cast { value, .. } => {
                    storage_accesses.extend(value.collect_storage_accesses(contract));
                }

                Expr::In {
                    value, collection, ..
                } => {
                    storage_accesses.extend(value.collect_storage_accesses(contract));
                    storage_accesses.extend(collection.collect_storage_accesses(contract));
                }

                Expr::Range { lb, ub, .. } => {
                    storage_accesses.extend(lb.collect_storage_accesses(contract));
                    storage_accesses.extend(ub.collect_storage_accesses(contract));
                }

                Expr::Generator {
                    gen_ranges,
                    conditions,
                    body,
                    ..
                } => {
                    gen_ranges.iter().for_each(|(_, range)| {
                        storage_accesses.extend(range.collect_storage_accesses(contract));
                    });
                    conditions.iter().for_each(|condition| {
                        storage_accesses.extend(condition.collect_storage_accesses(contract));
                    });
                    storage_accesses.extend(body.collect_storage_accesses(contract));
                }

                Expr::UnionTag { union_expr, .. } => {
                    storage_accesses.extend(union_expr.collect_storage_accesses(contract));
                }

                Expr::UnionValue { union_expr, .. } => {
                    storage_accesses.extend(union_expr.collect_storage_accesses(contract));
                }

                Expr::Error(_)
                | Expr::Immediate { .. }
                | Expr::Path(..)
                | Expr::MacroCall { .. }
                | Expr::Match { .. } => {}
            }
        }

        storage_accesses
    }

    pub fn is_storage_access(&self, contract: &Contract) -> bool {
        match self.get(contract) {
            Expr::StorageAccess { .. } | Expr::ExternalStorageAccess { .. } => true,
            Expr::TupleFieldAccess { tuple, .. } => tuple.is_storage_access(contract),
            Expr::Index { expr, .. } => expr.is_storage_access(contract),
            _ => false,
        }
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
        // We start with all the constraint and state exprs.
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

impl<'a> Iterator for ExprsIter<'a> {
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

            Expr::PredicateCall {
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

            Expr::UnionTag { union_expr, .. } | Expr::UnionValue { union_expr, .. } => {
                queue_if_new!(self, union_expr)
            }

            Expr::Error(_)
            | Expr::StorageAccess { .. }
            | Expr::ExternalStorageAccess { .. }
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
