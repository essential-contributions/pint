//! Items related to generating the `Map` mutations and keys builders.

use crate::{abi_key_doc_str, key_str, mutation_impl_deref, mutation_method_from_keyed_var};
use pint_abi_types::KeyedTupleField;
use proc_macro2::Span;

/// The name for the a tuple builder struct.
pub(crate) fn mutations_struct_name(key: &pint_abi_types::Key) -> String {
    format!("Tuple_{}", key_str(key))
}

/// A builder struct for a tuple field.
fn mutations_struct(struct_name: &str, key: &pint_abi_types::Key) -> syn::ItemStruct {
    let struct_ident = syn::Ident::new(struct_name, Span::call_site());
    let abi_key_doc_str = abi_key_doc_str(key);
    let doc_str = format!(
        "A mutations builder struct for the tuple at key `{abi_key_doc_str}`.\n\n\
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

/// A mutation buidler method for a tuple struct.
fn mutation_method(ix: usize, field: &KeyedTupleField) -> syn::ImplItemFn {
    let name = field.name.clone().unwrap_or_else(|| format!("_{ix}"));
    mutation_method_from_keyed_var(&name, &field.ty)
}

/// The mutation builder methods for a tuple struct.
fn mutations_methods(fields: &[KeyedTupleField]) -> Vec<syn::ImplItemFn> {
    fields
        .iter()
        .enumerate()
        .map(|(ix, field)| mutation_method(ix, field))
        .collect()
}

/// The implementation for the tuple mutations builder of the given name.
fn mutations_impl(struct_name: &str, fields: &[KeyedTupleField]) -> syn::ItemImpl {
    let struct_ident = syn::Ident::new(struct_name, Span::call_site());
    let methods = mutations_methods(fields);
    syn::parse_quote! {
        impl<'a> #struct_ident<'a> {
            #(
                #methods
            )*
        }
    }
}

/// Tuple builder types and impls for a given keyed tuple type.
pub(crate) fn builder_items(
    fields: &[KeyedTupleField],
    key: &pint_abi_types::Key,
) -> Vec<syn::Item> {
    let struct_name = mutations_struct_name(key);
    let mut items = vec![];
    items.push(mutations_struct(&struct_name, key).into());
    items.push(mutations_impl(&struct_name, fields).into());
    items.extend(
        mutation_impl_deref(&struct_name)
            .into_iter()
            .map(syn::Item::from),
    );
    items
}
