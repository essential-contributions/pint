//! Items related to generating the `Map` mutations and keys builders.

use crate::{mutation_impl_deref, mutation_method_from_node, nesting_key_doc_str, nesting_ty_str};
use pint_abi_visit::{KeyedVarTree, Nesting, NodeIx};
use proc_macro2::Span;

/// The name for the a tuple builder struct.
pub(crate) fn mutations_struct_name(nesting: &[Nesting]) -> String {
    format!("Tuple_{}", nesting_ty_str(nesting))
}

/// A builder struct for a tuple field.
fn mutations_struct(nesting: &[Nesting], struct_name: &str) -> syn::ItemStruct {
    let struct_ident = syn::Ident::new(struct_name, Span::call_site());
    let key_doc_str = nesting_key_doc_str(nesting);
    let doc_str = format!(
        "A mutations builder struct for the tuple at key `{key_doc_str}`.\n\n\
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

/// A mutation builder method for a tuple struct.
fn mutation_method(tree: &KeyedVarTree, field: NodeIx) -> syn::ImplItemFn {
    let field_keyed = &tree[field];
    let name = field_keyed.name.map(|s| s.to_string()).unwrap_or_else(|| {
        let ix = match &field_keyed.nesting {
            Nesting::TupleField { ix, .. } => ix,
            nesting => todo!("expected tuple field, found {nesting:?}"),
        };
        format!("_{ix}")
    });
    mutation_method_from_node(tree, field, &name)
}

/// The mutation builder methods for a tuple struct.
fn mutations_methods(tree: &KeyedVarTree, tuple: NodeIx) -> Vec<syn::ImplItemFn> {
    tree.children(tuple)
        .into_iter()
        .map(|field| mutation_method(tree, field))
        .collect()
}

/// The implementation for the tuple mutations builder of the given name.
/// `n` is the node index of the tuple within the tree.
fn mutations_impl(tree: &KeyedVarTree, tuple: NodeIx, struct_name: &str) -> syn::ItemImpl {
    let struct_ident = syn::Ident::new(struct_name, Span::call_site());
    let methods = mutations_methods(tree, tuple);
    syn::parse_quote! {
        impl<'a> #struct_ident<'a> {
            #(
                #methods
            )*
        }
    }
}

/// Tuple builder types and impls for a given keyed tuple type.
pub(crate) fn builder_items(tree: &KeyedVarTree, tuple: NodeIx) -> Vec<syn::Item> {
    let nesting = tree.nesting(tuple);
    let struct_name = mutations_struct_name(&nesting);
    let mut items = vec![];
    items.push(mutations_struct(&nesting, &struct_name).into());
    items.push(mutations_impl(tree, tuple, &struct_name).into());
    items.extend(
        mutation_impl_deref(&struct_name)
            .into_iter()
            .map(syn::Item::from),
    );
    items
}
