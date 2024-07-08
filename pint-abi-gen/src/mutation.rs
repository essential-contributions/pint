//! Items for the `Mutations` type generated for `storage` and `pub vars`,
//! aimed at making it easier to build a set of
//! [`Mutation`][essential_types::solution::Mutation]s for a solution.

use crate::{array, map, tuple, SingleKeyTy};
use pint_abi_types::{TypeABI, VarABI};
use pint_abi_visit::{KeyedVarTree, Nesting, NodeIx};
use proc_macro2::Span;

/// Recursively traverse the given keyed var and create a builder struct and
/// associated impl for each tuple, map and array.
fn nested_builder_items_from_node(tree: &KeyedVarTree, n: NodeIx) -> Vec<syn::Item> {
    let mut items = vec![];
    match tree[n].ty {
        TypeABI::Array { ty, size: _ } => {
            items.extend(array::builder_items(tree, n, ty));
        }
        TypeABI::Map { ty_from, ty_to } => {
            items.extend(map::builder_items(tree, n, ty_from, ty_to));
        }
        TypeABI::Tuple(_fields) => {
            items.extend(tuple::builder_items(tree, n));
        }
        TypeABI::Bool | TypeABI::Int | TypeABI::Real | TypeABI::String | TypeABI::B256 => (),
    }
    items
}

/// Recursively traverse the given keyed vars and create a builder structs and
/// impls for each tuple, map and array.
fn nested_builder_items_from_keyed_vars(vars: &[VarABI]) -> Vec<syn::Item> {
    let mut items = vec![];
    let tree = KeyedVarTree::from_keyed_vars(vars);
    tree.dfs(|n| {
        items.extend(nested_builder_items_from_node(&tree, n));
    });
    items
}

/// The `From<Mutations>` implementation for `Vec<Mutation>`.
fn impl_from_mutations_for_vec() -> syn::ItemImpl {
    syn::parse_quote! {
        impl From<Mutations> for Vec<pint_abi::types::essential::solution::Mutation> {
            fn from(m: Mutations) -> Self {
                m.set
            }
        }
    }
}

/// The docstring for a `Mutations` method.
fn method_doc_str(name: &str) -> String {
    format!(
        "Add a mutation for the `{name}` field into the set.\n\n\
        Relaces any existing mutation for the given key.",
    )
}

/// A `Mutations` builder method for an array field.
fn method_for_array(name: &str, array_nesting: &[Nesting]) -> syn::ImplItemFn {
    let struct_name = array::mutations_struct_name(array_nesting);
    let method_ident = syn::Ident::new(name, Span::call_site());
    let struct_ident = syn::Ident::new(&struct_name, Span::call_site());
    let doc_str = method_doc_str(name);
    syn::parse_quote! {
        #[doc = #doc_str]
        pub fn #method_ident(mut self, f: impl FnOnce(#struct_ident) -> #struct_ident) -> Self {
            f(#struct_ident { mutations: &mut self });
            self
        }
    }
}

/// A `Mutations` builder method for a map field.
fn method_for_map(name: &str, map_nesting: &[Nesting]) -> syn::ImplItemFn {
    let struct_name = map::mutations_struct_name(map_nesting);
    let method_ident = syn::Ident::new(name, Span::call_site());
    let struct_ident = syn::Ident::new(&struct_name, Span::call_site());
    let doc_str = method_doc_str(name);
    syn::parse_quote! {
        #[doc = #doc_str]
        pub fn #method_ident(mut self, f: impl FnOnce(#struct_ident) -> #struct_ident) -> Self {
            f(#struct_ident { mutations: &mut self });
            self
        }
    }
}

/// A `Mutations` builder method for a tuple field.
fn method_for_tuple(name: &str, nesting: &[Nesting]) -> syn::ImplItemFn {
    let struct_name = tuple::mutations_struct_name(nesting);
    let method_ident = syn::Ident::new(name, Span::call_site());
    let struct_ident = syn::Ident::new(&struct_name, Span::call_site());
    let doc_str = method_doc_str(name);
    syn::parse_quote! {
        #[doc = #doc_str]
        pub fn #method_ident(mut self, f: impl FnOnce(#struct_ident) -> #struct_ident) -> Self {
            f(#struct_ident { mutations: &mut self });
            self
        }
    }
}

/// A `Mutations` builder method for a single-key mutation.
fn method_for_single_key(name: &str, arg_ty: &SingleKeyTy, nesting: &[Nesting]) -> syn::ImplItemFn {
    let method_ident = syn::Ident::new(name, Span::call_site());
    let nesting_key_doc_str = crate::nesting_key_doc_str(nesting);
    let doc_str = format!("{}\n\nKey: `{nesting_key_doc_str}`", method_doc_str(name));
    let arg_ty = arg_ty.syn_ty();
    let nesting_expr: syn::ExprArray = crate::nesting_expr(nesting);
    let construct_key_expr: syn::Expr = crate::construct_key_expr();
    syn::parse_quote! {
        #[doc = #doc_str]
        pub fn #method_ident(mut self, val: #arg_ty) -> Self {
            use pint_abi::types::essential::{Key, Value, solution::Mutation};
            let nesting = #nesting_expr;
            let key: Key = #construct_key_expr;
            let value: Value = pint_abi::encode(&val);
            self.set.retain(|m: &Mutation| &m.key != &key);
            let mutation = Mutation { key, value };
            self.set.push(mutation);
            self
        }
    }
}

/// A builder method that takes an argument of the keyed type at the given index
/// within the tree.
///
/// This is shared between both the `Mutations` and `Tuple_*` implementations.
pub(crate) fn method_from_node(tree: &KeyedVarTree, n: NodeIx, name: &str) -> syn::ImplItemFn {
    let nesting = tree.nesting(n);
    let arg_ty = match &tree[n].ty {
        TypeABI::Bool => SingleKeyTy::Bool,
        TypeABI::Int => SingleKeyTy::Int,
        TypeABI::Real => SingleKeyTy::Real,
        TypeABI::String => SingleKeyTy::String,
        TypeABI::B256 => SingleKeyTy::B256,
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
    // A mutation builder method for a single mutation.
    method_for_single_key(name, &arg_ty, &nesting)
}

/// All builder methods for the `Mutations` builder type.
fn impl_mutations_methods(vars: &[VarABI]) -> Vec<syn::ImplItemFn> {
    let tree = KeyedVarTree::from_keyed_vars(vars);
    tree.roots()
        .iter()
        .map(|&n| {
            let name = crate::field_name_from_var_name(tree[n].name.unwrap());
            method_from_node(&tree, n, &name)
        })
        .collect()
}

/// The implementation for the `Mutations` builder type.
fn impl_mutations(vars: &[VarABI]) -> syn::ItemImpl {
    let methods = impl_mutations_methods(vars);
    syn::parse_quote! {
        impl Mutations {
            #(
                #methods
            )*
        }
    }
}

/// Shorthand constructor for the `Mutations` builder.
fn mutations_fn() -> syn::ItemFn {
    syn::parse_quote! {
        /// Begin building a set of [`Mutations`].
        pub fn mutations() -> Mutations {
            Mutations::default()
        }
    }
}

/// The builder struct for mutations.
fn mutations_struct() -> syn::ItemStruct {
    syn::parse_quote! {
        /// A builder for a set of mutations.
        ///
        /// Can be constructed via the [`mutations`] function.
        #[derive(Debug, Default)]
        pub struct Mutations {
            /// The set of mutations being built.
            set: Vec<pint_abi::types::essential::solution::Mutation>,
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

/// A `DerefMut<Target = Mutations>` impl for a nested builder struct.
///
/// This allows for easily accessing the `Mutations` type's `key_elems` and
/// `set` fields within impls for the nested tuple, map and array builder types.
pub(crate) fn impl_deref_for_nested(struct_name: &str) -> Vec<syn::ItemImpl> {
    let struct_ident = syn::Ident::new(struct_name, Span::call_site());
    let deref_impl = syn::parse_quote! {
        impl<'a> core::ops::Deref for #struct_ident<'a> {
            type Target = Mutations;
            fn deref(&self) -> &Self::Target {
                &*self.mutations
            }
        }
    };
    let deref_mut_impl = syn::parse_quote! {
        impl<'a> core::ops::DerefMut for #struct_ident<'a> {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut *self.mutations
            }
        }
    };
    vec![deref_impl, deref_mut_impl]
}

/// All items for the `Mutations` type, nested mutations builder types and their impls.
pub(crate) fn items(vars: &[VarABI]) -> Vec<syn::Item> {
    let mut items = vec![
        mutations_struct().into(),
        mutations_fn().into(),
        impl_mutations(vars).into(),
        impl_from_mutations_for_vec().into(),
    ];
    items.extend(nested_builder_items_from_keyed_vars(vars));
    items
}
