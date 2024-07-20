//! Items related to generating the `Array` mutations and keys builders.

use crate::{
    construct_key_expr, map, mutation, nesting_expr, nesting_key_doc_str, nesting_ty_str, tuple,
    SingleKeyTy, TypeABI,
};
use pint_abi_visit::{KeyedVarTree, Nesting, NodeIx};
use proc_macro2::Span;

/// The name for the array builder struct.
pub(crate) fn mutations_struct_name(nesting: &[Nesting]) -> String {
    format!("Array_{}", nesting_ty_str(nesting))
}

/// A builder struct for an array field.
fn mutations_struct(struct_name: &str, nesting: &[Nesting]) -> syn::ItemStruct {
    let struct_ident = syn::Ident::new(struct_name, Span::call_site());
    let nesting_key_doc_str = nesting_key_doc_str(nesting);
    let doc_str = format!(
        "A mutations builder struct for the array at key `{nesting_key_doc_str}`.\n\n\
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
fn mutation_method_for_array(arr_nesting: &[Nesting]) -> syn::ImplItemFn {
    let struct_name = mutations_struct_name(arr_nesting);
    let struct_ident = syn::Ident::new(&struct_name, Span::call_site());
    syn::parse_quote! {
        /// Add mutations for the nested array at the given key.
        pub fn entry(mut self, ix: usize, f: impl FnOnce(#struct_ident) -> #struct_ident) -> Self {
            self.key_elems.push(pint_abi::key::Elem::ArrayIx(ix));
            f(#struct_ident { mutations: &mut self.mutations });
            self.key_elems.pop();
            self
        }
    }
}

/// An array mutation builder method for entries with nested map values.
fn mutation_method_for_map(map_nesting: &[Nesting]) -> syn::ImplItemFn {
    let struct_name = map::mutations_struct_name(map_nesting);
    let struct_ident = syn::Ident::new(&struct_name, Span::call_site());
    syn::parse_quote! {
        /// Add mutations for the nested map at the given key.
        pub fn entry(mut self, ix: usize, f: impl FnOnce(#struct_ident) -> #struct_ident) -> Self {
            self.key_elems.push(pint_abi::key::Elem::ArrayIx(ix));
            f(#struct_ident { mutations: &mut self.mutations });
            self.key_elems.pop();
            self
        }
    }
}

/// An array mutation builder method for entries with tuple values.
fn mutation_method_for_tuple(tup_nesting: &[Nesting]) -> syn::ImplItemFn {
    let struct_name = tuple::mutations_struct_name(tup_nesting);
    let struct_ident = syn::Ident::new(&struct_name, Span::call_site());
    syn::parse_quote! {
        /// Add mutations for the tuple at the given index.
        pub fn entry(mut self, ix: usize, f: impl FnOnce(#struct_ident) -> #struct_ident) -> Self {
            self.key_elems.push(pint_abi::key::Elem::ArrayIx(ix));
            f(#struct_ident { mutations: &mut self.mutations });
            self.key_elems.pop();
            self
        }
    }
}

/// An array mutation builder method for an element with a single-key value.
fn mutation_method_for_single_key(ty: &SingleKeyTy, elem_nesting: &[Nesting]) -> syn::ImplItemFn {
    let ty = ty.syn_ty();
    let nesting_expr: syn::ExprArray = nesting_expr(elem_nesting);
    let construct_key_expr: syn::Expr = construct_key_expr();
    let nesting_key_doc_str = nesting_key_doc_str(elem_nesting);
    let method_doc_str = format!(
        "The given index will be flattened and encoded as a word and merged into the full key `{nesting_key_doc_str}`."
    );
    syn::parse_quote! {
        /// Add a mutation for the element at the given index.
        ///
        #[doc = #method_doc_str]
        pub fn entry(mut self, ix: usize, val: #ty) -> Self {
            use pint_abi::types::essential::{solution::Mutation, Key, Value};
            // Add the array index to the key stack.
            self.key_elems.push(pint_abi::key::Elem::ArrayIx(ix));

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

/// An array method for inserting mutations for an entry associated with a given index.
fn mutation_method(tree: &KeyedVarTree, elem: NodeIx, elem_ty: &TypeABI) -> syn::ImplItemFn {
    let elem_nesting = tree.nesting(elem);
    let val_ty = match elem_ty {
        TypeABI::Bool => SingleKeyTy::Bool,
        TypeABI::Int => SingleKeyTy::Int,
        TypeABI::Real => SingleKeyTy::Real,
        TypeABI::Array { .. } => return mutation_method_for_array(&elem_nesting),
        TypeABI::String => SingleKeyTy::String,
        TypeABI::B256 => SingleKeyTy::B256,
        TypeABI::Tuple { .. } => {
            return mutation_method_for_tuple(&elem_nesting);
        }
        TypeABI::Map { .. } => {
            return mutation_method_for_map(&elem_nesting);
        }
    };
    mutation_method_for_single_key(&val_ty, &elem_nesting)
}

/// The implementation for the map mutations builder of the given name.
fn mutations_impl(
    tree: &KeyedVarTree,
    array: NodeIx,
    struct_name: &str,
    ty: &TypeABI,
) -> syn::ItemImpl {
    let struct_ident = syn::Ident::new(struct_name, Span::call_site());
    // The node for an array elem is its only child.
    let elem = tree.children(array)[0];
    let method = mutation_method(tree, elem, ty);
    syn::parse_quote! {
        impl<'a> #struct_ident<'a> {
            #method
        }
    }
}

/// Array builder types and impls for a given keyed array type.
pub(crate) fn builder_items(tree: &KeyedVarTree, array: NodeIx, ty: &TypeABI) -> Vec<syn::Item> {
    let nesting = tree.nesting(array);
    let struct_name = mutations_struct_name(&nesting);
    let mut items = vec![];
    items.push(mutations_struct(&struct_name, &nesting).into());
    items.push(mutations_impl(tree, array, &struct_name, ty).into());
    items.extend(
        mutation::impl_deref_for_nested(&struct_name)
            .into_iter()
            .map(syn::Item::from),
    );
    items
}
