//! Items related to generating the `Map` mutations and keys builders.

use crate::{
    abi_key_from_keyed_type, construct_key_expr, mutation_impl_deref, nesting_expr,
    nesting_key_doc_str, nesting_ty_str, tuple, ty_from_pint_ty, KeyedTypeABI, SingleKeyTy,
};
use pint_abi_types::TypeABI;
use pint_abi_visit::{KeyedVarTree, Nesting, NodeIx};
use proc_macro2::Span;

/// The name for the a map builder struct.
pub(crate) fn mutations_struct_name(nesting: &[Nesting]) -> String {
    format!("Map_{}", nesting_ty_str(nesting))
}

/// A builder struct for a map field.
fn mutations_struct(struct_name: &str, nesting: &[Nesting]) -> syn::ItemStruct {
    let struct_ident = syn::Ident::new(struct_name, Span::call_site());
    let abi_key_doc_str = nesting_key_doc_str(nesting);
    let doc_str = format!(
        "A mutations builder struct for the map at key `{abi_key_doc_str}`.\n\n\
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

/// A map mutation builder method for entries with tuple values.
fn mutation_method_for_tuple(ty_from: &TypeABI, tup_nesting: &[Nesting]) -> syn::ImplItemFn {
    let key_ty = ty_from_pint_ty(ty_from);
    let struct_name = tuple::mutations_struct_name(tup_nesting);
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

/// A map mutation builder method for entries with nested map values.
fn mutation_method_for_map(ty_from: &TypeABI, map_nesting: &[Nesting]) -> syn::ImplItemFn {
    let key_ty = ty_from_pint_ty(ty_from);
    let struct_name = mutations_struct_name(map_nesting);
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

/// A map mutation builder method for an entry with a single-key value.
fn mutation_method_for_single_key(
    ty_from: &TypeABI,
    val_ty: &SingleKeyTy,
    val_nesting: &[Nesting],
) -> syn::ImplItemFn {
    let key_ty = ty_from_pint_ty(ty_from);
    let val_ty = val_ty.syn_ty();
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
fn map_mutation_method(
    tree: &KeyedVarTree,
    entry: NodeIx,
    ty_from: &TypeABI,
    ty_to: &KeyedTypeABI,
) -> syn::ImplItemFn {
    let nesting = tree.nesting(entry);
    let val_ty = match ty_to {
        KeyedTypeABI::Bool(_key) => SingleKeyTy::Bool,
        KeyedTypeABI::Int(_key) => SingleKeyTy::Int,
        KeyedTypeABI::Real(_key) => SingleKeyTy::Real,
        KeyedTypeABI::Array { ty, size: _ } => {
            let _key = abi_key_from_keyed_type(ty);
            todo!()
        }
        KeyedTypeABI::String(_key) => SingleKeyTy::String,
        KeyedTypeABI::B256(_key) => SingleKeyTy::B256,
        KeyedTypeABI::Tuple(_) => {
            return mutation_method_for_tuple(ty_from, &nesting);
        }
        KeyedTypeABI::Map { .. } => {
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
    ty_to: &KeyedTypeABI,
) -> syn::ItemImpl {
    let struct_ident = syn::Ident::new(struct_name, Span::call_site());
    // The node for a map entry is its only child.
    let entry = tree.children(map)[0];
    let method = map_mutation_method(tree, entry, ty_from, ty_to);
    syn::parse_quote! {
        impl<'a> #struct_ident<'a> {
            #method
        }
    }
}

/// Map builder types and impls for a given keyed map type.
pub(crate) fn builder_items(
    tree: &KeyedVarTree,
    map: NodeIx,
    ty_from: &TypeABI,
    ty_to: &KeyedTypeABI,
) -> Vec<syn::Item> {
    let nesting = tree.nesting(map);
    let struct_name = mutations_struct_name(&nesting);
    let mut items = vec![];
    items.push(mutations_struct(&struct_name, &nesting).into());
    items.push(mutations_impl(tree, map, &struct_name, ty_from, ty_to).into());
    items.extend(
        mutation_impl_deref(&struct_name)
            .into_iter()
            .map(syn::Item::from),
    );
    items
}
