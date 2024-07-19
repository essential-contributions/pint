use super::{Contract, Predicate};
use crate::{expr::Expr, types::Type};
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

    /// Returns the type of key `self` given an `Predicate`. Panics if the type can't be
    /// found in the `expr_types` map.
    pub fn get_ty<'a>(&'a self, contract: &'a Contract) -> &Type {
        contract.exprs.expr_types.get(*self).unwrap()
    }

    /// Set the type of key `self` in an `Predicate`. Panics if the type can't be found in
    /// the `expr_types` map.
    pub fn set_ty<'a>(&'a self, ty: Type, contract: &'a mut Contract) {
        contract.exprs.expr_types.insert(*self, ty);
    }

    /// Return whether this expression can panic (typically related to storage).
    pub fn can_panic(&self, contract: &Contract, pred: &Predicate) -> bool {
        contract.exprs.get(*self).map_or(false, |expr| match expr {
            Expr::StorageAccess(_, _) | Expr::ExternalStorageAccess { .. } => true,

            Expr::PathByName(path, _) => pred.states().any(|(_, state)| &state.name == path),

            Expr::Error(_)
            | Expr::Immediate { .. }
            | Expr::PathByKey(_, _)
            | Expr::MacroCall { .. } => false,

            Expr::Array {
                elements,
                range_expr,
                ..
            } => {
                elements.iter().any(|el| el.can_panic(contract, pred))
                    || range_expr.can_panic(contract, pred)
            }

            Expr::Tuple { fields, .. } => fields.iter().any(|fld| fld.1.can_panic(contract, pred)),

            Expr::UnaryOp { expr, .. } => expr.can_panic(contract, pred),

            Expr::BinaryOp { lhs, rhs, .. } => {
                lhs.can_panic(contract, pred) || rhs.can_panic(contract, pred)
            }

            Expr::IntrinsicCall { args, .. } => {
                args.iter().any(|arg| arg.can_panic(contract, pred))
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
        })
    }
}

/// [`ExprsIter`] is an iterator for all the _reachable_ expressions in the Predicate.
///
/// Items are popped off the queue and are returned next.  If they're a branch then their children
/// are queued.  The visited set is updated and used to avoid following a branch multiple
/// times.
///
/// An alternative is to use `[Predicate::visitor]` which will also iterate
/// for each reachable expression but does not implement `Interator` and instead takes a closure.

#[derive(Debug)]
pub(crate) struct ExprsIter<'a> {
    contract: &'a Contract,
    queue: Vec<ExprKey>,
    visited: HashSet<ExprKey>,
}

impl<'a> ExprsIter<'a> {
    pub(super) fn new(contract: &'a Contract, pred: &Predicate) -> ExprsIter<'a> {
        // We start with all the constraint and state exprs.
        let queue = pred.root_set().collect();

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

            Expr::Error(_)
            | Expr::StorageAccess(..)
            | Expr::ExternalStorageAccess { .. }
            | Expr::PathByKey(_, _)
            | Expr::PathByName(_, _)
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
