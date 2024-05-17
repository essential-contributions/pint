use super::{IntermediateIntent, Var};
use crate::types::Type;

slotmap::new_key_type! { pub struct VarKey; }

#[derive(Debug, Default, Clone)]
pub struct Vars {
    vars: slotmap::SlotMap<VarKey, Var>,
    var_types: slotmap::SecondaryMap<VarKey, Type>,
}

impl Vars {
    /// Returns a read-only iterator to the `vars` map
    pub fn vars(&self) -> slotmap::basic::Iter<VarKey, Var> {
        self.vars.iter()
    }

    /// Inserts a variable with its type
    pub fn insert(&mut self, var: Var, ty: Type) -> VarKey {
        let key = self.vars.insert(var);
        self.var_types.insert(key, ty);
        key
    }

    /// Removes a variable and its type given an `VarKey`
    pub fn remove(&mut self, key: VarKey) {
        self.vars.remove(key);
        self.var_types.remove(key);
    }

    /// Apply function `f` on every var
    pub fn update_vars(&mut self, f: impl FnOnce(&mut Var) + std::marker::Copy) {
        for (_, var) in self.vars.iter_mut() {
            f(var)
        }
    }

    /// Apply function `f` on every var type
    /// Only the `Type`s can be mutated, not the keys
    pub fn update_types(&mut self, f: impl FnOnce(VarKey, &mut Type) + std::marker::Copy) {
        for (key, ty) in self.var_types.iter_mut() {
            f(key, ty)
        }
    }
}

impl VarKey {
    /// Returns an `Option` containing the `Var` corresponding to key `self`. Returns `None` if
    /// the key can't be found in the `vars` map.
    pub fn try_get<'a>(&'a self, ii: &'a IntermediateIntent) -> Option<&Var> {
        ii.vars.vars.get(*self)
    }

    /// Returns the `Var` corresponding to key `self`. Panics if the key can't be found in the
    /// `vars` map.
    pub fn get<'a>(&'a self, ii: &'a IntermediateIntent) -> &Var {
        ii.vars.vars.get(*self).unwrap()
    }

    /// Returns the type of key `self` given an `IntermediateIntent`. Panics if the type can't be
    /// found in the `var_types` map.
    pub fn get_ty<'a>(&'a self, ii: &'a IntermediateIntent) -> &Type {
        ii.vars.var_types.get(*self).unwrap()
    }

    /// Set the type of key `self` in an `IntermediateIntent`. Panics if the type can't be found in
    /// the `var_types` map.
    pub fn set_ty<'a>(&'a self, ty: Type, ii: &'a mut IntermediateIntent) {
        ii.vars.var_types.insert(*self, ty);
    }
}
