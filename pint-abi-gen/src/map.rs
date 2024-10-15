//! Items related to generating the `Map` mutations and keys builders.

use crate::{
    array, construct_key_expr, keys, mutations, nesting_expr, nesting_key_doc_str, nesting_ty_str,
    tuple, ty_from_pint_ty, SingleKeyTy,
};
use pint_abi_types::TypeABI;
use pint_abi_visit::{KeyedVarTree, Nesting, NodeIx};
use proc_macro2::Span;

/// The name for the a map builder struct.
pub(crate) fn struct_name(nesting: &[Nesting]) -> String {
    format!("Map_{}", nesting_ty_str(nesting))
}

// ------------------------------------
// Keys
// ------------------------------------

/// A builder struct for a map field.
fn keys_struct(struct_name: &str, nesting: &[Nesting]) -> syn::ItemStruct {
    let struct_ident = syn::Ident::new(struct_name, Span::call_site());
    let nesting_key_doc_str = nesting_key_doc_str(nesting);
    let doc_str = format!(
        "A keys builder struct for the map at key `{nesting_key_doc_str}`.\n\n\
        Generated solely for use within the `Keys` builder pattern.",
    );
    syn::parse_quote! {
        #[doc = #doc_str]
        #[allow(non_camel_case_types)]
        pub struct #struct_ident<'a> {
            keys: &'a mut Keys,
        }
    }
}

/// A map key builder method for entries with nested array values.
fn key_method_for_array(ty_from: &TypeABI, array_nesting: &[Nesting]) -> syn::ImplItemFn {
    let key_ty = ty_from_pint_ty(ty_from, 2);
    let struct_name = array::struct_name(array_nesting);
    let struct_ident = syn::Ident::new(&struct_name, Span::call_site());
    syn::parse_quote! {
        /// Add keys for the nested map at the given key.
        pub fn entry(mut self, key: #key_ty, f: impl FnOnce(#struct_ident) -> #struct_ident) -> Self {
            let key_words: pint_abi::types::essential::Key = pint_abi::encode(&key);
            self.key_elems.push(pint_abi::key::Elem::MapKey(key_words));
            f(#struct_ident { keys: &mut self.keys });
            self.key_elems.pop();
            self
        }
    }
}

/// A map key builder method for entries with nested map values.
fn key_method_for_map(ty_from: &TypeABI, map_nesting: &[Nesting]) -> syn::ImplItemFn {
    let key_ty = ty_from_pint_ty(ty_from, 2);
    let struct_name = struct_name(map_nesting);
    let struct_ident = syn::Ident::new(&struct_name, Span::call_site());
    syn::parse_quote! {
        /// Add keys for the nested map at the given key.
        pub fn entry(mut self, key: #key_ty, f: impl FnOnce(#struct_ident) -> #struct_ident) -> Self {
            let key_words: pint_abi::types::essential::Key = pint_abi::encode(&key);
            self.key_elems.push(pint_abi::key::Elem::MapKey(key_words));
            f(#struct_ident { keys: &mut self.keys });
            self.key_elems.pop();
            self
        }
    }
}

/// A map key builder method for entries with tuple values.
fn key_method_for_tuple(ty_from: &TypeABI, tup_nesting: &[Nesting]) -> syn::ImplItemFn {
    let key_ty = ty_from_pint_ty(ty_from, 2);
    let struct_name = tuple::struct_name(tup_nesting);
    let struct_ident = syn::Ident::new(&struct_name, Span::call_site());
    syn::parse_quote! {
        /// Add keys for the tuple at the given key.
        pub fn entry(mut self, key: #key_ty, f: impl FnOnce(#struct_ident) -> #struct_ident) -> Self {
            let key_words: pint_abi::types::essential::Key = pint_abi::encode(&key);
            self.key_elems.push(pint_abi::key::Elem::MapKey(key_words));
            f(#struct_ident { keys: &mut self.keys });
            self.key_elems.pop();
            self
        }
    }
}

/// A map key builder method for an entry with a single-key value.
fn key_method_for_single_key(ty_from: &TypeABI, val_nesting: &[Nesting]) -> syn::ImplItemFn {
    let key_ty = ty_from_pint_ty(ty_from, 2);
    let nesting_expr: syn::ExprArray = nesting_expr(val_nesting);
    let construct_key_expr: syn::Expr = construct_key_expr();
    let abi_key_doc_str = nesting_key_doc_str(val_nesting);
    let merge_doc_str = format!(
        "The given key will be encoded as words and merged into the full key `{abi_key_doc_str}`."
    );
    syn::parse_quote! {
        /// Add a key for the entry at the given key.
        ///
        #[doc = #merge_doc_str]
        pub fn entry(mut self, key: #key_ty) -> Self {
            use pint_abi::types::essential::Key;
            // Add the map key to the stack.
            let key: Key = pint_abi::encode(&key);
            self.key_elems.push(pint_abi::key::Elem::MapKey(key));
            // Merge the key stack with the ABI key.
            let nesting = #nesting_expr;
            let key: Key = #construct_key_expr;
            // Add the key to the set.
            self.set.retain(|k: &Key| k != &key);
            self.set.push(key);
            // Pop the entry key from the stack.
            self.key_elems.pop();
            self
        }
    }
}

/// A map method for inserting keys for an entry associated with a given key.
fn key_method(
    tree: &KeyedVarTree,
    entry: NodeIx,
    ty_from: &TypeABI,
    ty_to: &TypeABI,
) -> syn::ImplItemFn {
    let nesting = tree.nesting(entry);
    match ty_to {
        TypeABI::Bool
        | TypeABI::Int
        | TypeABI::Real
        | TypeABI::String
        | TypeABI::B256
        | TypeABI::Union { .. } => (),
        TypeABI::Array { ty: _, size: _ } => {
            return key_method_for_array(ty_from, &nesting);
        }
        TypeABI::Tuple(_) => {
            return key_method_for_tuple(ty_from, &nesting);
        }
        TypeABI::Map { .. } => {
            return key_method_for_map(ty_from, &nesting);
        }
    };
    key_method_for_single_key(ty_from, &nesting)
}

/// The implementation for the map keys builder of the given name.
fn keys_impl(
    tree: &KeyedVarTree,
    map: NodeIx,
    struct_name: &str,
    ty_from: &TypeABI,
    ty_to: &TypeABI,
) -> syn::ItemImpl {
    let struct_ident = syn::Ident::new(struct_name, Span::call_site());
    // The node for a map entry is its only child.
    let entry = tree.children(map)[0];
    let method = key_method(tree, entry, ty_from, ty_to);
    syn::parse_quote! {
        impl<'a> #struct_ident<'a> {
            #method
        }
    }
}

/// Map builder types and impls for a given keyed map type.
pub(crate) fn keys_items(
    tree: &KeyedVarTree,
    map: NodeIx,
    ty_from: &TypeABI,
    ty_to: &TypeABI,
) -> Vec<syn::Item> {
    let nesting = tree.nesting(map);
    let struct_name = struct_name(&nesting);
    let mut items = vec![];
    items.push(keys_struct(&struct_name, &nesting).into());
    items.push(keys_impl(tree, map, &struct_name, ty_from, ty_to).into());
    items.extend(
        keys::impl_deref_for_nested(&struct_name)
            .into_iter()
            .map(syn::Item::from),
    );
    items
}

// ------------------------------------
// Mutations
// ------------------------------------

/// A builder struct for a map field.
fn mutations_struct(struct_name: &str, nesting: &[Nesting]) -> syn::ItemStruct {
    let struct_ident = syn::Ident::new(struct_name, Span::call_site());
    let nesting_key_doc_str = nesting_key_doc_str(nesting);
    let doc_str = format!(
        "A mutations builder struct for the map at key `{nesting_key_doc_str}`.\n\n\
        Generated solely for use within the `Mutations` builder pattern.",
    );
    syn::parse_quote! {
        #[doc = #doc_str]
        #[allow(non_camel_case_types)]
        pub struct #struct_ident<'a> {
            mutations: &'a mut Mutations,
        }
    }
}

/// A map mutation builder method for entries with nested array values.
fn mutation_method_for_array(ty_from: &TypeABI, array_nesting: &[Nesting]) -> syn::ImplItemFn {
    let key_ty = ty_from_pint_ty(ty_from, 2);
    let struct_name = array::struct_name(array_nesting);
    let struct_ident = syn::Ident::new(&struct_name, Span::call_site());
    syn::parse_quote! {
        /// Add mutations for the nested map at the given key.
        pub fn entry(mut self, key: #key_ty, f: impl FnOnce(#struct_ident) -> #struct_ident) -> Self {
            let key_words: pint_abi::types::essential::Key = pint_abi::encode(&key);
            self.key_elems.push(pint_abi::key::Elem::MapKey(key_words));
            f(#struct_ident { mutations: &mut self.mutations });
            self.key_elems.pop();
            self
        }
    }
}

/// A map mutation builder method for entries with nested map values.
fn mutation_method_for_map(ty_from: &TypeABI, map_nesting: &[Nesting]) -> syn::ImplItemFn {
    let key_ty = ty_from_pint_ty(ty_from, 2);
    let struct_name = struct_name(map_nesting);
    let struct_ident = syn::Ident::new(&struct_name, Span::call_site());
    syn::parse_quote! {
        /// Add mutations for the nested map at the given key.
        pub fn entry(mut self, key: #key_ty, f: impl FnOnce(#struct_ident) -> #struct_ident) -> Self {
            let key_words: pint_abi::types::essential::Key = pint_abi::encode(&key);
            self.key_elems.push(pint_abi::key::Elem::MapKey(key_words));
            f(#struct_ident { mutations: &mut self.mutations });
            self.key_elems.pop();
            self
        }
    }
}

/// A map mutation builder method for entries with tuple values.
fn mutation_method_for_tuple(ty_from: &TypeABI, tup_nesting: &[Nesting]) -> syn::ImplItemFn {
    let key_ty = ty_from_pint_ty(ty_from, 2);
    let struct_name = tuple::struct_name(tup_nesting);
    let struct_ident = syn::Ident::new(&struct_name, Span::call_site());
    syn::parse_quote! {
        /// Add mutations for the tuple at the given key.
        pub fn entry(mut self, key: #key_ty, f: impl FnOnce(#struct_ident) -> #struct_ident) -> Self {
            let key_words: pint_abi::types::essential::Key = pint_abi::encode(&key);
            self.key_elems.push(pint_abi::key::Elem::MapKey(key_words));
            f(#struct_ident { mutations: &mut self.mutations });
            self.key_elems.pop();
            self
        }
    }
}

/// A map mutation builder method for an entry with a single-key value.
fn mutation_method_for_single_key(
    ty_from: &TypeABI,
    val_ty: &SingleKeyTy,
    val_nesting: &[Nesting],
) -> syn::ImplItemFn {
    let key_ty = ty_from_pint_ty(ty_from, 2);
    let val_ty = val_ty.syn_ty(2);
    let nesting_expr: syn::ExprArray = nesting_expr(val_nesting);
    let construct_key_expr: syn::Expr = construct_key_expr();
    let abi_key_doc_str = nesting_key_doc_str(val_nesting);
    let merge_doc_str = format!(
        "The given key will be encoded as words and merged into the full key `{abi_key_doc_str}`."
    );
    syn::parse_quote! {
        /// Add a mutation for the entry at the given key.
        ///
        #[doc = #merge_doc_str]
        pub fn entry(mut self, key: #key_ty, val: #val_ty) -> Self {
            use pint_abi::types::essential::{solution::Mutation, Key, Value};
            // Add the map key to the stack.
            let key: Key = pint_abi::encode(&key);
            self.key_elems.push(pint_abi::key::Elem::MapKey(key));

            // Merge the key stack with the ABI key.
            let nesting = #nesting_expr;
            let key: Key = #construct_key_expr;
            let value: Value = pint_abi::encode(&val);

            // Add the mutation to the set.
            self.set.retain(|m: &Mutation| &m.key != &key);
            let mutation = Mutation { key, value };
            self.set.push(mutation);

            // Pop the entry key from the stack.
            self.key_elems.pop();
            self
        }
    }
}

/// A map method for inserting mutations for an entry associated with a given key.
fn mutation_method(
    tree: &KeyedVarTree,
    entry: NodeIx,
    ty_from: &TypeABI,
    ty_to: &TypeABI,
) -> syn::ImplItemFn {
    let nesting = tree.nesting(entry);
    let val_ty = match ty_to {
        TypeABI::Bool => SingleKeyTy::Bool,
        TypeABI::Int => SingleKeyTy::Int,
        TypeABI::Real => SingleKeyTy::Real,
        TypeABI::String => SingleKeyTy::String,
        TypeABI::B256 => SingleKeyTy::B256,
        TypeABI::Array { ty: _, size: _ } => {
            return mutation_method_for_array(ty_from, &nesting);
        }
        TypeABI::Tuple(_) => {
            return mutation_method_for_tuple(ty_from, &nesting);
        }
        TypeABI::Union { name, .. } => SingleKeyTy::Union(name.clone()),
        TypeABI::Map { .. } => {
            return mutation_method_for_map(ty_from, &nesting);
        }
    };
    mutation_method_for_single_key(ty_from, &val_ty, &nesting)
}

/// The implementation for the map mutations builder of the given name.
fn mutations_impl(
    tree: &KeyedVarTree,
    map: NodeIx,
    struct_name: &str,
    ty_from: &TypeABI,
    ty_to: &TypeABI,
) -> syn::ItemImpl {
    let struct_ident = syn::Ident::new(struct_name, Span::call_site());
    // The node for a map entry is its only child.
    let entry = tree.children(map)[0];
    let method = mutation_method(tree, entry, ty_from, ty_to);
    syn::parse_quote! {
        impl<'a> #struct_ident<'a> {
            #method
        }
    }
}

/// Map builder types and impls for a given keyed map type.
pub(crate) fn mutations_items(
    tree: &KeyedVarTree,
    map: NodeIx,
    ty_from: &TypeABI,
    ty_to: &TypeABI,
) -> Vec<syn::Item> {
    let nesting = tree.nesting(map);
    let struct_name = struct_name(&nesting);
    let mut items = vec![];
    items.push(mutations_struct(&struct_name, &nesting).into());
    items.push(mutations_impl(tree, map, &struct_name, ty_from, ty_to).into());
    items.extend(
        mutations::impl_deref_for_nested(&struct_name)
            .into_iter()
            .map(syn::Item::from),
    );
    items
}
