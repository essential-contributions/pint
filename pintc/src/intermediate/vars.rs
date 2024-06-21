use super::{DisplayWithII, Ident, IntermediateIntent};
use crate::{
    error::{CompileError, ErrorEmitted, Handler},
    span::Span,
    types::{Path, Type},
};
use abi_types::VarABI;
use std::fmt::{self, Formatter};

/// A decision variable with an optional type.
#[derive(Clone, Debug)]
pub struct Var {
    pub name: Path,
    pub is_pub: bool,
    pub span: Span,
}

slotmap::new_key_type! { pub struct VarKey; }

#[derive(Debug, Default, Clone)]
pub struct Vars {
    vars: slotmap::SlotMap<VarKey, Var>,
    var_types: slotmap::SecondaryMap<VarKey, Type>,
    order: Vec<VarKey>,
}

impl Vars {
    /// Returns a read-only iterator to the `vars` map
    pub fn vars(&self) -> impl Iterator<Item = (VarKey, &Var)> {
        self.order.iter().map(|&key| (key, &self.vars[key]))
    }

    /// Returns the order of the provided `VarKey` as tracked in the `order` vector
    pub fn position(&self, key: VarKey) -> Option<usize> {
        self.order.iter().position(|k| *k == key)
    }

    /// Inserts a variable with its type
    pub fn insert(&mut self, var: Var, ty: Type) -> VarKey {
        let key = self.vars.insert(var);
        self.var_types.insert(key, ty);
        self.order.push(key);
        key
    }

    /// Inserts a variable with its type at a particular position
    pub fn insert_at(&mut self, index: usize, var: Var, ty: Type) -> VarKey {
        let key = self.vars.insert(var);
        self.var_types.insert(key, ty);
        self.order.insert(index, key);
        key
    }

    /// Removes a variable and its type given an `VarKey`
    pub fn remove(&mut self, key: VarKey) {
        self.vars.remove(key);
        self.var_types.remove(key);
        self.order.retain(|&k| k != key);
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

    /// Generate a `VarABI` given a `VarKey` and an `IntermediateIntent`
    pub fn abi(&self, ii: &IntermediateIntent) -> Result<VarABI, CompileError> {
        Ok(VarABI {
            name: self.get(ii).name.clone(),
            ty: self.get_ty(ii).abi()?,
        })
    }
}

impl DisplayWithII for VarKey {
    fn fmt(&self, f: &mut Formatter, ii: &IntermediateIntent) -> fmt::Result {
        let var = &self.get(ii);
        if var.is_pub {
            write!(f, "pub ")?;
        }
        write!(f, "var {}", var.name)?;
        let ty = self.get_ty(ii);
        if !ty.is_unknown() {
            write!(f, ": {}", ii.with_ii(ty))?;
        }
        Ok(())
    }
}

impl IntermediateIntent {
    pub fn insert_var(
        &mut self,
        handler: &Handler,
        mod_prefix: &str,
        local_scope: Option<&str>,
        is_pub: bool,
        name: &Ident,
        ty: Option<Type>,
    ) -> std::result::Result<VarKey, ErrorEmitted> {
        let full_name =
            self.add_top_level_symbol(handler, mod_prefix, local_scope, name, name.span.clone())?;
        let var_key = self.vars.insert(
            Var {
                name: full_name,
                is_pub,
                span: name.span.clone(),
            },
            if let Some(ty) = ty {
                ty
            } else {
                Type::Unknown(name.span.clone())
            },
        );

        Ok(var_key)
    }

    pub(crate) fn vars(&self) -> impl Iterator<Item = (VarKey, &Var)> {
        self.vars.vars()
    }
}
