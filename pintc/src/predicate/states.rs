use super::{Contract, DisplayWithPred, ExprKey, Ident, Predicate};
use crate::{
    error::{ErrorEmitted, Handler},
    span::Span,
    types::{Path, Type},
};
use std::fmt::{self, Formatter};

/// A state specification with an optional type.
#[derive(Clone, Debug)]
pub struct State {
    pub name: Path,
    pub expr: ExprKey,
    pub span: Span,
}

slotmap::new_key_type! { pub struct StateKey; }

#[derive(Debug, Default, Clone)]
pub struct States {
    states: slotmap::SlotMap<StateKey, State>,
    state_types: slotmap::SecondaryMap<StateKey, Type>,
    order: Vec<StateKey>,
}

impl States {
    /// Returns a read-only iterator to the `states` map
    pub fn states(&self) -> impl Iterator<Item = (StateKey, &State)> {
        self.order.iter().map(|&key| (key, &self.states[key]))
    }

    /// Returns the order of the provided `StateKey` as tracked in the `order` vector
    pub fn position(&self, key: StateKey) -> Option<usize> {
        self.order.iter().position(|k| *k == key)
    }

    /// Inserts a state variable with its type
    pub fn insert(&mut self, state: State, ty: Type) -> StateKey {
        let key = self.states.insert(state);
        self.state_types.insert(key, ty);
        self.order.push(key);
        key
    }

    /// Inserts a state variable with its type at a particular position
    pub fn insert_at(&mut self, index: usize, state: State, ty: Type) -> StateKey {
        let key = self.states.insert(state);
        self.state_types.insert(key, ty);
        self.order.insert(index, key);
        key
    }

    /// Removes a state variable and its type given a `StateKey`
    pub fn remove(&mut self, key: StateKey) {
        self.states.remove(key);
        self.state_types.remove(key);
        self.order.retain(|el| *el != key);
    }

    /// Apply function `f` on every state
    pub fn update_states(&mut self, f: impl FnOnce(&mut State) + std::marker::Copy) {
        for (_, state) in self.states.iter_mut() {
            f(state)
        }
    }

    /// Apply function `f` on every state type
    /// Only the `Type`s can be mutated, not the keys
    pub fn update_types(&mut self, f: impl FnOnce(StateKey, &mut Type) + std::marker::Copy) {
        for (key, ty) in self.state_types.iter_mut() {
            f(key, ty)
        }
    }
}

impl StateKey {
    /// Returns an `Option` containing the `State` corresponding to key `self`. Returns `None` if
    /// the key can't be found in the `states` map.
    pub fn try_get<'a>(&'a self, pred: &'a Predicate) -> Option<&State> {
        pred.states.states.get(*self)
    }

    /// Returns the `State` corresponding to key `self`. Panics if the key can't be found in the
    /// `states` map.
    pub fn get<'a>(&'a self, pred: &'a Predicate) -> &State {
        pred.states.states.get(*self).unwrap()
    }

    /// Returns the type of key `self` given an `Predicate`. Panics if the type can't be
    /// found in the `state_types` map.
    pub fn get_ty<'a>(&'a self, pred: &'a Predicate) -> &Type {
        pred.states.state_types.get(*self).unwrap()
    }

    /// Set the type of key `self` in an `Predicate`. Panics if the type can't be found in
    /// the `state_types` map.
    pub fn set_ty<'a>(&'a self, ty: Type, pred: &'a mut Predicate) {
        pred.states.state_types.insert(*self, ty);
    }
}

impl DisplayWithPred for StateKey {
    fn fmt(&self, f: &mut Formatter, contract: &Contract, pred: &Predicate) -> fmt::Result {
        let state = &self.get(pred);
        write!(f, "state {}", state.name)?;
        let ty = self.get_ty(pred);
        if !ty.is_unknown() {
            write!(f, ": {}", contract.with_ctrct(ty))?;
        }
        write!(f, " = {}", contract.with_ctrct(&state.expr))
    }
}

impl Predicate {
    pub fn insert_state(
        &mut self,
        handler: &Handler,
        mod_prefix: &str,
        name: &Ident,
        ty: Option<Type>,
        expr: ExprKey,
        span: Span,
    ) -> std::result::Result<StateKey, ErrorEmitted> {
        let name = self
            .symbols
            .add_symbol(handler, mod_prefix, None, name, span.clone())?;
        let state_key = self.states.insert(
            State {
                name,
                expr,
                span: span.clone(),
            },
            if let Some(ty) = ty {
                ty
            } else {
                Type::Unknown(span.clone())
            },
        );

        Ok(state_key)
    }

    pub(crate) fn states(&self) -> impl Iterator<Item = (StateKey, &State)> {
        self.states.states()
    }
}
