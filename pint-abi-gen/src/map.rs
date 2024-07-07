//! Items related to generating the `Map` mutations and keys builders.

use crate::{
    abi_key_doc_str, abi_key_expr, abi_key_from_keyed_type, key_str, merge_key_expr,
    mutation_impl_deref, tuple, ty_from_pint_ty, KeyedTypeABI, SingleKeyTy,
};
use pint_abi_types::TypeABI;
use proc_macro2::Span;

/// The name for the a map builder struct.
pub(crate) fn mutations_struct_name(key: &pint_abi_types::Key) -> String {
    format!("Map_{}", key_str(key))
}

/// A builder struct for a map field.
fn mutations_struct(struct_name: &str, key: &pint_abi_types::Key) -> syn::ItemStruct {
    let struct_ident = syn::Ident::new(struct_name, Span::call_site());
    let abi_key_doc_str = abi_key_doc_str(key);
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
fn mutation_method_for_tuple(ty_from: &TypeABI, tup_key: &pint_abi_types::Key) -> syn::ImplItemFn {
    let key_ty = ty_from_pint_ty(ty_from);
    let struct_name = tuple::mutations_struct_name(tup_key);
    let struct_ident = syn::Ident::new(&struct_name, Span::call_site());
    syn::parse_quote! {
        /// Add mutations for the tuple at the given key.
        pub fn entry(mut self, key: #key_ty, f: impl FnOnce(#struct_ident) -> #struct_ident) -> Self {
            let key_words: pint_abi::types::essential::Key = pint_abi::encode(&key);
            self.keys.push(key_words);
            f(#struct_ident { mutations: &mut self.mutations });
            self.keys.pop();
            self
        }
    }
}

/// A map mutation builder method for entries with nested map values.
fn mutation_method_for_map(ty_from: &TypeABI, map_key: &pint_abi_types::Key) -> syn::ImplItemFn {
    let key_ty = ty_from_pint_ty(ty_from);
    let struct_name = mutations_struct_name(map_key);
    let struct_ident = syn::Ident::new(&struct_name, Span::call_site());
    syn::parse_quote! {
        /// Add mutations for the nested map at the given key.
        pub fn entry(mut self, key: #key_ty, f: impl FnOnce(#struct_ident) -> #struct_ident) -> Self {
            let key_words: pint_abi::types::essential::Key = pint_abi::encode(&key);
            self.keys.push(key_words);
            f(#struct_ident { mutations: &mut self.mutations });
            self.keys.pop();
            self
        }
    }
}

/// A map mutation builder method for an entry with a single-key value.
fn mutation_method_for_single_key(
    ty_from: &TypeABI,
    val_ty: &SingleKeyTy,
    val_key: &pint_abi_types::Key,
) -> syn::ImplItemFn {
    let key_ty = ty_from_pint_ty(ty_from);
    let val_ty = val_ty.syn_ty();
    let abi_key_expr: syn::ExprArray = abi_key_expr(val_key);
    let merge_key_expr: syn::Expr = merge_key_expr();
    let abi_key_doc_str = abi_key_doc_str(val_key);
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
            self.keys.push(key);

            // Merge the key stack with the ABI key.
            let abi_key = #abi_key_expr;
            let key: Key = #merge_key_expr;
            let value: Value = pint_abi::encode(&val);

            // Add the mutation to the set.
            self.set.retain(|m: &Mutation| &m.key != &key);
            let mutation = Mutation { key, value };
            self.set.push(mutation);

            // Pop the entry key from the stack.
            self.keys.pop();
            self
        }
    }
}

/// A map method for inserting mutations for an entry associated with a given key.
fn map_mutation_method(ty_from: &TypeABI, ty_to: &KeyedTypeABI) -> syn::ImplItemFn {
    let (val_ty, key) = match ty_to {
        KeyedTypeABI::Bool(key) => (SingleKeyTy::Bool, key),
        KeyedTypeABI::Int(key) => (SingleKeyTy::Int, key),
        KeyedTypeABI::Real(key) => (SingleKeyTy::Real, key),
        KeyedTypeABI::Array { ty, size: _ } => {
            let _key = abi_key_from_keyed_type(ty);
            todo!()
        }
        KeyedTypeABI::String(key) => (SingleKeyTy::String, key),
        KeyedTypeABI::B256(key) => (SingleKeyTy::B256, key),
        KeyedTypeABI::Tuple(fields) => {
            let key = abi_key_from_keyed_type(&fields[0].ty);
            return mutation_method_for_tuple(ty_from, key);
        }
        KeyedTypeABI::Map { key, .. } => {
            return mutation_method_for_map(ty_from, key);
        }
    };
    mutation_method_for_single_key(ty_from, &val_ty, key)
}

/// The implementation for the map mutations builder of the given name.
fn mutations_impl(struct_name: &str, ty_from: &TypeABI, ty_to: &KeyedTypeABI) -> syn::ItemImpl {
    let struct_ident = syn::Ident::new(struct_name, Span::call_site());
    let method = map_mutation_method(ty_from, ty_to);
    syn::parse_quote! {
        impl<'a> #struct_ident<'a> {
            #method
        }
    }
}

/// Map builder types and impls for a given keyed map type.
pub(crate) fn builder_items(
    ty_from: &TypeABI,
    ty_to: &KeyedTypeABI,
    key: &pint_abi_types::Key,
) -> Vec<syn::Item> {
    let struct_name = mutations_struct_name(key);
    let mut items = vec![];
    items.push(mutations_struct(&struct_name, key).into());
    items.push(mutations_impl(&struct_name, ty_from, ty_to).into());
    items.extend(
        mutation_impl_deref(&struct_name)
            .into_iter()
            .map(syn::Item::from),
    );
    items
}
