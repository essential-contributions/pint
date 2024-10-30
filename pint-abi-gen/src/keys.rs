//! Items for the `Keys` type generated for `storage` and `pub vars`, aimed
//! at making it easier to build a set of [`Key`][essential_types::Key]s for
//! a solution.

use crate::{array, map, tuple};
use pint_abi_types::{ParamABI, TypeABI};
use pint_abi_visit::{KeyedVarTree, Nesting, NodeIx};
use proc_macro2::Span;

/// Recursively traverse the given keyed var and create a builder struct and
/// associated impl for each tuple, map and array.
fn nested_items_from_node(tree: &KeyedVarTree, n: NodeIx) -> Vec<syn::Item> {
    let mut items = vec![];
    match tree[n].ty {
        TypeABI::Array { ty, size: _ } => {
            items.extend(array::keys_items(tree, n, ty));
        }
        TypeABI::Map { ty_from, ty_to } => {
            items.extend(map::keys_items(tree, n, ty_from, ty_to));
        }
        TypeABI::Tuple(_fields) => {
            items.extend(tuple::keys_items(tree, n));
        }
        TypeABI::Bool
        | TypeABI::Int
        | TypeABI::Real
        | TypeABI::String
        | TypeABI::B256
        | TypeABI::Union { .. } => (),
    }
    items
}

/// Recursively traverse the given keyed vars and create a builder structs and
/// impls for each tuple, map and array.
fn nested_items_from_keyed_vars(vars: &[ParamABI]) -> Vec<syn::Item> {
    let mut items = vec![];
    let tree = KeyedVarTree::from_keyed_vars(vars);
    tree.dfs(|n| {
        items.extend(nested_items_from_node(&tree, n));
    });
    items
}

/// The `From<Keys>` implementation for `Vec<Key>`.
fn impl_from_keys_for_vec() -> syn::ItemImpl {
    syn::parse_quote! {
        impl From<Keys> for Vec<pint_abi::types::essential::Key> {
            fn from(m: Keys) -> Self {
                m.set
            }
        }
    }
}

/// The docstring for a `Keys` method.
fn method_doc_str(name: &str) -> String {
    format!(
        "Add a key for the `{name}` field into the set.\n\n\
        Relaces any existing entry for the given key.",
    )
}

/// A `Keys` builder method for an array field.
fn method_for_array(name: &str, array_nesting: &[Nesting]) -> syn::ImplItemFn {
    let struct_name = array::struct_name(array_nesting);
    let method_ident = syn::Ident::new(name, Span::call_site());
    let struct_ident = syn::Ident::new(&struct_name, Span::call_site());
    let doc_str = method_doc_str(name);
    syn::parse_quote! {
        #[doc = #doc_str]
        pub fn #method_ident(mut self, f: impl FnOnce(#struct_ident) -> #struct_ident) -> Self {
            f(#struct_ident { keys: &mut self });
            self
        }
    }
}

/// A `Keys` builder method for a map field.
fn method_for_map(name: &str, map_nesting: &[Nesting]) -> syn::ImplItemFn {
    let struct_name = map::struct_name(map_nesting);
    let method_ident = syn::Ident::new(name, Span::call_site());
    let struct_ident = syn::Ident::new(&struct_name, Span::call_site());
    let doc_str = method_doc_str(name);
    syn::parse_quote! {
        #[doc = #doc_str]
        pub fn #method_ident(mut self, f: impl FnOnce(#struct_ident) -> #struct_ident) -> Self {
            f(#struct_ident { keys: &mut self });
            self
        }
    }
}

/// A `Keys` builder method for a tuple field.
fn method_for_tuple(name: &str, nesting: &[Nesting]) -> syn::ImplItemFn {
    let struct_name = tuple::struct_name(nesting);
    let method_ident = syn::Ident::new(name, Span::call_site());
    let struct_ident = syn::Ident::new(&struct_name, Span::call_site());
    let doc_str = method_doc_str(name);
    syn::parse_quote! {
        #[doc = #doc_str]
        pub fn #method_ident(mut self, f: impl FnOnce(#struct_ident) -> #struct_ident) -> Self {
            f(#struct_ident { keys: &mut self });
            self
        }
    }
}

/// A `Keys` builder method for a single-key.
fn method_for_single_key(name: &str, nesting: &[Nesting]) -> syn::ImplItemFn {
    let method_ident = syn::Ident::new(name, Span::call_site());
    let nesting_key_doc_str = crate::nesting_key_doc_str(nesting);
    let doc_str = format!("{}\n\nKey: `{nesting_key_doc_str}`", method_doc_str(name));
    let nesting_expr: syn::ExprArray = crate::nesting_expr(nesting);
    let construct_key_expr: syn::Expr = crate::construct_key_expr();
    syn::parse_quote! {
        #[doc = #doc_str]
        pub fn #method_ident(mut self) -> Self {
            use pint_abi::types::essential::Key;
            let nesting = #nesting_expr;
            let key: Key = #construct_key_expr;
            self.set.retain(|k: &Key| k != &key);
            self.set.push(key);
            self
        }
    }
}

/// A builder method that takes an argument of the keyed type at the given index
/// within the tree.
///
/// This is shared between both the `Keys` and `Tuple_*` implementations.
pub(crate) fn method_from_node(tree: &KeyedVarTree, n: NodeIx, name: &str) -> syn::ImplItemFn {
    let nesting = tree.nesting(n);
    match &tree[n].ty {
        TypeABI::Bool
        | TypeABI::Int
        | TypeABI::Real
        | TypeABI::String
        | TypeABI::B256
        | TypeABI::Union { .. } => (),
        TypeABI::Array { ty: _, size: _ } => {
            return method_for_array(name, &nesting);
        }
        TypeABI::Tuple(_) => {
            return method_for_tuple(name, &nesting);
        }
        TypeABI::Map { .. } => {
            return method_for_map(name, &nesting);
        }
    };
    method_for_single_key(name, &nesting)
}

/// All builder methods for the `Keys` builder type.
fn impl_keys_methods(vars: &[ParamABI]) -> Vec<syn::ImplItemFn> {
    let tree = KeyedVarTree::from_keyed_vars(vars);
    tree.roots()
        .iter()
        .map(|&n| {
            let name = crate::field_name_from_var_name(tree[n].name.unwrap());
            method_from_node(&tree, n, &name)
        })
        .collect()
}

/// The implementation for the `Keys` builder type.
fn impl_keys(vars: &[ParamABI]) -> syn::ItemImpl {
    let methods = impl_keys_methods(vars);
    syn::parse_quote! {
        impl Keys {
            #(
                #methods
            )*
        }
    }
}

/// Shorthand constructor for the `Keys` builder.
fn keys_fn() -> syn::ItemFn {
    syn::parse_quote! {
        /// Begin building a set of [`Keys`].
        pub fn keys() -> Keys {
            Keys::default()
        }
    }
}

/// The builder struct for keys.
fn keys_struct() -> syn::ItemStruct {
    syn::parse_quote! {
        /// A builder for a set of keys.
        ///
        /// Can be constructed via the [`keys`] function.
        #[derive(Debug, Default)]
        pub struct Keys {
            /// The set of keys being built.
            set: Vec<pint_abi::types::essential::Key>,
            /// The stack of key elements that need to be merged with the
            /// `&[Nesting]` derived by the `TypeABI` traversal.
            ///
            /// For example, when a map's `entry` method is called, the provided
            /// key is pushed to this stack. Upon completion of the `entry`
            /// method, the key is popped.
            key_elems: Vec<pint_abi::key::Elem>,
        }
    }
}

/// A `DerefMut<Target = Keys>` impl for a nested builder struct.
///
/// This allows for easily accessing the `Keys` type's `key_elems` and
/// `set` fields within impls for the nested tuple, map and array builder types.
pub(crate) fn impl_deref_for_nested(struct_name: &str) -> Vec<syn::ItemImpl> {
    let struct_ident = syn::Ident::new(struct_name, Span::call_site());
    let deref_impl = syn::parse_quote! {
        impl<'a> core::ops::Deref for #struct_ident<'a> {
            type Target = Keys;
            fn deref(&self) -> &Self::Target {
                &*self.keys
            }
        }
    };
    let deref_mut_impl = syn::parse_quote! {
        impl<'a> core::ops::DerefMut for #struct_ident<'a> {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut *self.keys
            }
        }
    };
    vec![deref_impl, deref_mut_impl]
}

/// All items for the `Keys` type, nested keys builder types and their impls.
fn items(vars: &[ParamABI]) -> Vec<syn::Item> {
    let mut items = vec![
        keys_struct().into(),
        keys_fn().into(),
        impl_keys(vars).into(),
        impl_from_keys_for_vec().into(),
    ];
    items.extend(nested_items_from_keyed_vars(vars));
    items
}

/// A `keys` module for all `Keys`-related items.
pub(crate) fn module(vars: &[ParamABI]) -> syn::ItemMod {
    let items = items(vars);

    syn::parse_quote! {
        pub mod keys {
            //! All items related to building a set of [`Keys`].
            #(
                #items
            )*
        }
    }
}
