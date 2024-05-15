use super::{IntermediateIntent, State};
use crate::types::Type;

slotmap::new_key_type! { pub struct StateKey; }

#[derive(Debug, Default, Clone)]
pub struct _States {
    states: slotmap::SlotMap<StateKey, State>,
    state_types: slotmap::SecondaryMap<StateKey, Type>,
}

impl _States {
    /// Returns a read-only iterator to the `states` map
    pub fn states(&self) -> slotmap::basic::Iter<StateKey, State> {
        self.states.iter()
    }

    /// Inserts a state variable with its type
    pub fn insert(&mut self, state: State, ty: Type) -> StateKey {
        let key = self.states.insert(state);
        self.state_types.insert(key, ty);
        key
    }

    /// Removes a state variable and its type given an `StateKey`
    pub fn remove(&mut self, key: StateKey) {
        self.states.remove(key);
        self.state_types.remove(key);
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
    pub fn try_get<'a>(&'a self, ii: &'a IntermediateIntent) -> Option<&State> {
        ii._states.states.get(*self)
    }

    /// Returns the `State` corresponding to key `self`. Panics if the key can't be found in the
    /// `states` map.
    pub fn get<'a>(&'a self, ii: &'a IntermediateIntent) -> &State {
        ii._states.states.get(*self).unwrap()
    }

    /// Returns the type of key `self` given an `IntermediateIntent`. Panics if the type can't be
    /// found in the `state_types` map.
    pub fn get_ty<'a>(&'a self, ii: &'a IntermediateIntent) -> &Type {
        ii._states.state_types.get(*self).unwrap()
    }

    /// Set the type of key `self` in an `IntermediateIntent`. Panics if the type can't be found in
    /// the `state_types` map.
    pub fn set_ty<'a>(&'a self, ty: Type, ii: &'a mut IntermediateIntent) {
        ii._states.state_types.insert(*self, ty);
    }
}
