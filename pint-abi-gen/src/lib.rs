//! Macros for generating items from pint-generated contract ABI JSON.
//!
//! For a given contract, the following items are generated:
//!
//! - A `mod` representing `storage`.
//! - For each `predicate`, a module with the following:
//!     - A `struct` for the predicate's decision variables.
//!     - A `mod` for the predicate's public decision variables.
//!
//! Implementations are generated for using these types to write solutions to the
//! format expected by the Essential protocol.

use pint_abi_types::{
    KeyedTupleField, KeyedTypeABI, KeyedVarABI, PredicateABI, ProgramABI, TupleField, TypeABI,
    VarABI,
};
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::ToTokens;
use syn::parse_macro_input;

/// The name of the root module within the predicate set produced by the compiler.
const ROOT_MOD_NAME: &str = "";

/// Convert the given pint tuple fields to unnamed Rust fields.
fn fields_from_tuple_fields(fields: &[TupleField]) -> Vec<syn::Field> {
    fields
        .iter()
        .map(|TupleField { name, ty }| {
            // NOTE: Currently we ignore tuple field names.
            let _name = name;
            let ty = ty_from_pint_ty(ty);
            syn::parse_quote!(#ty)
        })
        .collect()
}

/// Convert the given pint tuple to an equivalent Rust tuple type.
fn ty_from_tuple(tuple: &[TupleField]) -> syn::Type {
    let fields = fields_from_tuple_fields(tuple);
    syn::parse_quote! {
        ( #( #fields ),* )
    }
}

/// Convert the given pint ABI type to an equivalent Rust type.
// TODO: Ideally we should use proper `Word` and `B256` types here,
// but that would require users needing to have a particular crate as a
// dependency :thinking_face:
fn ty_from_pint_ty(ty: &TypeABI) -> syn::Type {
    match ty {
        TypeABI::Bool => syn::parse_quote!(bool),
        TypeABI::Int => syn::parse_quote!(i64),
        TypeABI::Real => syn::parse_quote!(f64),
        TypeABI::String => syn::parse_quote!(String),
        TypeABI::B256 => syn::parse_quote!([i64; 4]),
        TypeABI::Tuple(tuple) => ty_from_tuple(tuple),
    }
}

/// Names for fields are emitted by pint with a `::` prefix.
/// This function checks for the `::` prefix and strips it if it exists.
fn strip_colons_prefix(name: &str) -> &str {
    let mut split = name.split("::");
    let first = split.next();
    split.next().or(first).expect("name had unexpected format")
}

/// Var names have the `::` prefix, and sometimes have `.` separators for
/// flattened tuple fields. We strip the `::` prefix, and replace all `.`
/// occurrences with `_`.
fn field_name_from_var_name(name: &str) -> String {
    strip_colons_prefix(name).replace('.', "_")
}

/// A named field for each of the decision variables.
fn fields_from_vars(vars: &[VarABI]) -> Vec<syn::Field> {
    vars.iter()
        .map(|var| {
            let name = field_name_from_var_name(&var.name);
            let ident = syn::Ident::new(&name, Span::call_site());
            let ty = ty_from_pint_ty(&var.ty);
            syn::parse_quote! {
                pub #ident: #ty
            }
        })
        .collect()
}

/// Generate a struct for an predicate's decision variables.
fn struct_from_vars(vars: &[VarABI]) -> syn::ItemStruct {
    let fields = fields_from_vars(vars);
    syn::parse_quote! {
        /// The predicate's decision variables.
        #[derive(Clone, Debug)]
        pub struct Vars {
            #(
                #fields
            ),*
        }
    }
}

/// Generate a `Encode` implementation for a `Vars` type.
fn impl_encode_for_vars(vars: &[VarABI]) -> syn::ItemImpl {
    let field_idents = vars
        .iter()
        .map(|var| field_name_from_var_name(&var.name))
        .map(|name| syn::Ident::new(&name, Span::call_site()));
    syn::parse_quote! {
        impl pint_abi::Encode for Vars {
            fn encode<W: pint_abi::Write>(&self, w: &mut W) -> Result<(), W::Error> {
                #(
                    pint_abi::Encode::encode(&self.#field_idents, w).expect("cannot fail");
                )*
                Ok(())
            }
        }
    }
}

/// Generate a `From<Vars>` implementation for converting `Vars` to `Vec<Value>`.
fn impl_from_vars(vars: &[VarABI]) -> syn::ItemImpl {
    let field_idents = vars
        .iter()
        .map(|var| field_name_from_var_name(&var.name))
        .map(|name| syn::Ident::new(&name, Span::call_site()));
    syn::parse_quote! {
        impl From<Vars> for Vec<pint_abi::types::essential::Value> {
            fn from(vars: Vars) -> Self {
                let mut values: Vec<pint_abi::types::essential::Value> = vec![];
                #(
                    values.push(pint_abi::encode(&vars.#field_idents));
                )*
                values
            }
        }
    }
}

/// Generate all items for the given predicate.
fn items_from_predicate(predicate: &PredicateABI) -> Vec<syn::Item> {
    let mut items = vec![];
    if !predicate.vars.is_empty() {
        items.push(struct_from_vars(&predicate.vars).into());
        items.push(impl_encode_for_vars(&predicate.vars).into());
        items.push(impl_from_vars(&predicate.vars).into());
    }
    if !predicate.pub_vars.is_empty() {
        items.push(mod_from_keyed_vars("pub_vars", &predicate.pub_vars).into());
    }
    items
}

/// Generate a module for the given predicate.
/// Modules are only generated for named predicates.
fn mod_from_predicate(predicate: &PredicateABI) -> syn::ItemMod {
    let name = strip_colons_prefix(&predicate.name);
    let doc_str = format!("Items for the `{name}` predicate.");
    let ident = syn::Ident::new(name, Span::call_site());
    let items = items_from_predicate(predicate);
    syn::parse_quote! {
        #[allow(non_snake_case)]
        #[doc = #doc_str]
        pub mod #ident {
            #(
                #items
            )*
        }
    }
}

/// Whether or not the given predicate contains any items.
fn is_predicate_empty(pred: &PredicateABI) -> bool {
    pred.vars.is_empty() && pred.pub_vars.is_empty()
}

/// Generate a module for each named predicate.
fn mods_from_named_predicates(predicates: &[PredicateABI]) -> Vec<syn::ItemMod> {
    predicates
        .iter()
        .filter(|&predicate| !is_predicate_empty(predicate))
        .filter(|predicate| predicate.name != ROOT_MOD_NAME)
        .map(mod_from_predicate)
        .collect()
}

/// Find the root predicate.
fn find_root_predicate(predicates: &[PredicateABI]) -> Option<&PredicateABI> {
    predicates
        .iter()
        .find(|predicate| predicate.name == ROOT_MOD_NAME)
}

/// Given the set of predicates, generate all items.
///
/// This includes a module for each named predicate, and types for the root predicate.
fn items_from_predicates(predicates: &[PredicateABI]) -> Vec<syn::Item> {
    let mut items = vec![];
    // Add the root predicate items.
    if let Some(root_pred) = find_root_predicate(predicates) {
        items.extend(items_from_predicate(root_pred));
    }
    // Add the named predicate modules.
    items.extend(
        mods_from_named_predicates(predicates)
            .into_iter()
            .map(syn::Item::from),
    );
    items
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
            /// The stack of map keys that need to be merged with the key
            /// provided by the `KeyedTypeABI`.
            ///
            /// When a map's `entry` method is called, the provided key is
            /// pushed to this stack. Upon completion of the `entry` method, the
            /// key is popped.
            map_keys: Vec<pint_abi::types::essential::Key>,
        }
    }
}

fn mutation_method_doc_str(name: &str) -> String {
    format!(
        "Add a mutation for the `{name}` field into the set.\n\n\
        Relaces any existing mutation for the given key.",
    )
}

/// Pint keyed var types that occupy only a single key.
enum SingleKeyTy {
    Bool,
    Int,
    Real,
    String,
    B256,
}

impl SingleKeyTy {
    /// The type of the builder method value.
    fn syn_ty(&self) -> syn::Type {
        match self {
            SingleKeyTy::Bool => syn::parse_quote!(bool),
            SingleKeyTy::Int => syn::parse_quote!(i64),
            SingleKeyTy::Real => syn::parse_quote!(f64),
            SingleKeyTy::String => syn::parse_quote!(String),
            SingleKeyTy::B256 => syn::parse_quote!([i64; 4]),
        }
    }
}

/// Given an ABI key, create a Rust expression that results in its value.
fn abi_key_expr(key: &pint_abi_types::Key) -> syn::ExprArray {
    let elems = key
        .iter()
        .map(|&opt| {
            let expr: syn::Expr = match opt {
                None => syn::parse_quote!(None),
                Some(u) => {
                    let w = essential_types::Word::try_from(u)
                        .expect("failed to convert ABI key value to a `Word`");
                    syn::parse_quote!(Some(#w))
                }
            };
            expr
        })
        .collect();
    syn::ExprArray {
        attrs: vec![],
        bracket_token: Default::default(),
        elems,
    }
}

/// Given an ABI key, create a string for presenting the ABI key in docs.
///
/// E.g. `[0, 1, _, _, _, _, 6, 7]`.
fn abi_key_doc_str(key: &pint_abi_types::Key) -> String {
    use core::fmt::Write;
    let mut s = "[".to_string();
    let mut opts = key.iter();
    fn write_opt(s: &mut String, opt: &Option<usize>) {
        match opt {
            None => write!(s, "_"),
            Some(u) => write!(s, "{u}"),
        }
        .expect("failed to write key element to string")
    }
    if let Some(opt) = opts.next() {
        write_opt(&mut s, opt);
        for opt in opts {
            write!(&mut s, ", ").unwrap();
            write_opt(&mut s, opt);
        }
    }
    write!(&mut s, "]").unwrap();
    s
}

/// Assuming a `Mutations` instance is accessible via `self`, and a key from a
/// `KeyedTypeABI` instance is accessible via `abi_key`, produce an expression
/// that merges the current `Mutations`' `map_keys` into the ABI key.
fn merge_key_expr() -> syn::Expr {
    syn::parse_quote! {
        pint_abi::__merge_key(&abi_key[..], &self.map_keys[..])
    }
}

/// A `Mutations` builder method for a single-key mutation.
fn mutation_method_for_single_key(
    name: &str,
    arg_ty: &SingleKeyTy,
    key: &pint_abi_types::Key,
) -> syn::ImplItemFn {
    let method_ident = syn::Ident::new(name, Span::call_site());
    let abi_key_doc_str = abi_key_doc_str(key);
    let doc_str = format!(
        "{}\n\nKey: `{abi_key_doc_str}`",
        mutation_method_doc_str(name)
    );
    let arg_ty = arg_ty.syn_ty();
    let abi_key_expr: syn::ExprArray = abi_key_expr(key);
    let merge_key_expr: syn::Expr = merge_key_expr();
    syn::parse_quote! {
        #[doc = #doc_str]
        pub fn #method_ident(mut self, val: #arg_ty) -> Self {
            use pint_abi::types::essential::{Key, Value, solution::Mutation};
            let abi_key = #abi_key_expr;
            let key: Key = #merge_key_expr;
            let value: Value = pint_abi::encode(&val);
            self.set.retain(|m: &Mutation| &m.key != &key);
            let mutation = Mutation { key, value };
            self.set.push(mutation);
            self
        }
    }
}

/// Convert the key into a string to use for a generated tuple struct name.
///
/// E.g. `[Some(1), None, Some(42)]` becomes `1_nil_42`, so that it may be
/// appended to a type name, e.g. `Tuple_1_nil_42`.
fn key_str(key: &pint_abi_types::Key) -> String {
    fn opt_to_str(opt: &Option<usize>) -> String {
        opt.map(|u| u.to_string())
            .unwrap_or_else(|| "nil".to_string())
    }
    let mut opts = key.iter();
    let mut s = opt_to_str(opts.next().expect("key must have at least one element"));
    for opt in opts {
        use std::fmt::Write;
        let _ = write!(&mut s, "_{}", opt_to_str(opt));
    }
    s
}

/// The name for the a map builder struct.
fn map_mutations_struct_name(key: &pint_abi_types::Key) -> String {
    format!("Map_{}", key_str(key))
}

/// A builder struct for a map field.
fn map_mutations_struct(struct_name: &str, key: &pint_abi_types::Key) -> syn::ItemStruct {
    let struct_ident = syn::Ident::new(&struct_name, Span::call_site());
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
fn map_mutation_method_for_tuple(
    ty_from: &TypeABI,
    tup_key: &pint_abi_types::Key,
) -> syn::ImplItemFn {
    let key_ty = ty_from_pint_ty(ty_from);
    let struct_name = tuple_mutations_struct_name(tup_key);
    let struct_ident = syn::Ident::new(&struct_name, Span::call_site());
    syn::parse_quote! {
        /// Add mutations for the tuple at the given key.
        pub fn entry(mut self, key: #key_ty, f: impl FnOnce(#struct_ident) -> #struct_ident) -> Self {
            let key_words: pint_abi::types::essential::Key = pint_abi::encode(&key);
            self.map_keys.push(key_words);
            f(#struct_ident { mutations: &mut self.mutations });
            self.map_keys.pop();
            self
        }
    }
}

/// A map mutation builder method for entries with nested map values.
fn map_mutation_method_for_map(
    ty_from: &TypeABI,
    map_key: &pint_abi_types::Key,
) -> syn::ImplItemFn {
    let key_ty = ty_from_pint_ty(ty_from);
    let struct_name = map_mutations_struct_name(map_key);
    let struct_ident = syn::Ident::new(&struct_name, Span::call_site());
    syn::parse_quote! {
        /// Add mutations for the nested map at the given key.
        pub fn entry(mut self, key: #key_ty, f: impl FnOnce(#struct_ident) -> #struct_ident) -> Self {
            let key_words: pint_abi::types::essential::Key = pint_abi::encode(&key);
            self.map_keys.push(key_words);
            f(#struct_ident { mutations: &mut self.mutations });
            self.map_keys.pop();
            self
        }
    }
}

/// A map mutation builder method for an entry with a single-key value.
fn map_mutation_method_for_single_key(
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
            self.map_keys.push(key);

            // Merge the map_keys with the ABI key.
            let abi_key = #abi_key_expr;
            let key: Key = #merge_key_expr;
            let value: Value = pint_abi::encode(&val);

            // Add the mutation to the set.
            self.set.retain(|m: &Mutation| &m.key != &key);
            let mutation = Mutation { key, value };
            self.set.push(mutation);

            // Pop the entry key from the stack.
            self.map_keys.pop();
            self
        }
    }
}

/// Extract the key from a keyed type.
fn abi_key_from_keyed_type(ty: &KeyedTypeABI) -> &Vec<Option<usize>> {
    match ty {
        KeyedTypeABI::Bool(key) => key,
        KeyedTypeABI::Int(key) => key,
        KeyedTypeABI::Real(key) => key,
        KeyedTypeABI::Array { ty, .. } => abi_key_from_keyed_type(ty),
        KeyedTypeABI::String(key) => key,
        KeyedTypeABI::B256(key) => key,
        KeyedTypeABI::Tuple { key, .. } => key,
        KeyedTypeABI::Map { key, .. } => key,
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
        KeyedTypeABI::Tuple { fields: _, key } => {
            return map_mutation_method_for_tuple(ty_from, key);
        }
        KeyedTypeABI::Map { key, .. } => {
            return map_mutation_method_for_map(ty_from, key);
        }
    };
    map_mutation_method_for_single_key(ty_from, &val_ty, key)
}

/// The implementation for the map mutations builder of the given name.
fn map_mutations_impl(struct_name: &str, ty_from: &TypeABI, ty_to: &KeyedTypeABI) -> syn::ItemImpl {
    let struct_ident = syn::Ident::new(struct_name, Span::call_site());
    let method = map_mutation_method(ty_from, ty_to);
    syn::parse_quote! {
        impl<'a> #struct_ident<'a> {
            #method
        }
    }
}

/// The name for the a tuple builder struct.
fn tuple_mutations_struct_name(key: &pint_abi_types::Key) -> String {
    format!("Tuple_{}", key_str(key))
}

/// A builder struct for a tuple field.
fn tuple_mutations_struct(struct_name: &str, key: &pint_abi_types::Key) -> syn::ItemStruct {
    let struct_ident = syn::Ident::new(&struct_name, Span::call_site());
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
fn tuple_mutation_method(ix: usize, field: &KeyedTupleField) -> syn::ImplItemFn {
    let name = field.name.clone().unwrap_or_else(|| format!("_{ix}"));
    mutation_method_from_keyed_var(&name, &field.ty)
}

/// The mutation builder methods for a tuple struct.
fn tuple_mutations_methods(fields: &[KeyedTupleField]) -> Vec<syn::ImplItemFn> {
    fields
        .iter()
        .enumerate()
        .map(|(ix, field)| tuple_mutation_method(ix, field))
        .collect()
}

/// A `DerefMut<Target = Mutations>` impl for the tuple struct.
///
/// This allows for sharing the `mutation_method_from_keyed_var` for method
/// generation between the `Mutations` impl and the `Tuple_*` impls.
fn mutation_impl_deref(struct_name: &str) -> Vec<syn::ItemImpl> {
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

/// The implementation for the tuple mutations builder of the given name.
fn tuple_mutations_impl(struct_name: &str, fields: &[KeyedTupleField]) -> syn::ItemImpl {
    let struct_ident = syn::Ident::new(struct_name, Span::call_site());
    let methods = tuple_mutations_methods(fields);
    syn::parse_quote! {
        impl<'a> #struct_ident<'a> {
            #(
                #methods
            )*
        }
    }
}

/// Recursively traverse the given keyed var and create a builder struct for each tuple.
fn mutations_builders_from_keyed_type(ty: &KeyedTypeABI) -> Vec<syn::Item> {
    let mut items = vec![];
    match ty {
        KeyedTypeABI::Map {
            ty_from,
            ty_to,
            key,
        } => {
            let struct_name = map_mutations_struct_name(key);
            // Items for this map.
            items.push(map_mutations_struct(&struct_name, key).into());
            items.push(map_mutations_impl(&struct_name, ty_from, ty_to).into());
            items.extend(
                mutation_impl_deref(&struct_name)
                    .into_iter()
                    .map(syn::Item::from),
            );
            items.extend(
                mutations_builders_from_keyed_type(&ty_to)
                    .into_iter()
                    .map(syn::Item::from),
            );

            // todo!()
        }
        KeyedTypeABI::Tuple { fields, key } => {
            let struct_name = tuple_mutations_struct_name(key);
            // Items for this tuple.
            items.push(tuple_mutations_struct(&struct_name, key).into());
            items.push(tuple_mutations_impl(&struct_name, fields).into());
            items.extend(
                mutation_impl_deref(&struct_name)
                    .into_iter()
                    .map(syn::Item::from),
            );
            // Recursively collect remaining items from child tuples.
            items.extend(fields.iter().flat_map(|field| {
                mutations_builders_from_keyed_type(&field.ty)
                    .into_iter()
                    .map(syn::Item::from)
            }));
        }
        _ => (),
    }
    items
}

/// Recursively traverse the given keyed vars and create a builder structs and
/// impls for each tuple.
fn mutations_builders_from_keyed_vars(vars: &[KeyedVarABI]) -> Vec<syn::Item> {
    vars.iter()
        .flat_map(|var| mutations_builders_from_keyed_type(&var.ty))
        .collect()
}

/// A `Mutations` builder method for a map field.
fn mutation_method_for_map(name: &str, key: &pint_abi_types::Key) -> syn::ImplItemFn {
    let struct_name = map_mutations_struct_name(key);
    let method_ident = syn::Ident::new(name, Span::call_site());
    let struct_ident = syn::Ident::new(&struct_name, Span::call_site());
    let doc_str = mutation_method_doc_str(name);
    syn::parse_quote! {
        #[doc = #doc_str]
        pub fn #method_ident(mut self, f: impl FnOnce(#struct_ident) -> #struct_ident) -> Self {
            f(#struct_ident { mutations: &mut self });
            self
        }
    }
}

/// A `Mutations` builder method for a tuple field.
fn mutation_method_for_tuple(name: &str, key: &pint_abi_types::Key) -> syn::ImplItemFn {
    let struct_name = tuple_mutations_struct_name(key);
    let method_ident = syn::Ident::new(name, Span::call_site());
    let struct_ident = syn::Ident::new(&struct_name, Span::call_site());
    let doc_str = mutation_method_doc_str(name);
    syn::parse_quote! {
        #[doc = #doc_str]
        pub fn #method_ident(mut self, f: impl FnOnce(#struct_ident) -> #struct_ident) -> Self {
            f(#struct_ident { mutations: &mut self });
            self
        }
    }
}

/// A `Mutations` builder method for the given keyed var.
fn mutation_method_from_keyed_var(name: &str, ty: &KeyedTypeABI) -> syn::ImplItemFn {
    let (arg_ty, key) = match ty {
        KeyedTypeABI::Bool(key) => (SingleKeyTy::Bool, key),
        KeyedTypeABI::Int(key) => (SingleKeyTy::Int, key),
        KeyedTypeABI::Real(key) => (SingleKeyTy::Real, key),
        KeyedTypeABI::String(key) => (SingleKeyTy::String, key),
        KeyedTypeABI::B256(key) => (SingleKeyTy::B256, key),
        KeyedTypeABI::Array { ty: _, size: _ } => todo!(),
        // Tuple types take a closure.
        KeyedTypeABI::Tuple { fields: _, key } => return mutation_method_for_tuple(name, key),
        // Map types take a closure.
        KeyedTypeABI::Map { key, .. } => return mutation_method_for_map(name, key),
    };
    // A mutation builder method for a single mutation.
    mutation_method_for_single_key(name, &arg_ty, key)
}

/// All builder methods for the `Mutations builder type.
fn mutations_methods_from_keyed_vars(vars: &[KeyedVarABI]) -> Vec<syn::ImplItemFn> {
    vars.iter()
        .map(|var| {
            let name = strip_colons_prefix(&var.name);
            mutation_method_from_keyed_var(name, &var.ty)
        })
        .collect()
}

/// The implementation for the `Mutations` builder type.
fn impl_mutations_from_keyed_vars(vars: &[KeyedVarABI]) -> syn::ItemImpl {
    let methods = mutations_methods_from_keyed_vars(vars);
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

/// The `From<Mutations>` implementation for `Vec<Mutation>`.
fn impl_from_mutations() -> syn::ItemImpl {
    syn::parse_quote! {
        impl From<Mutations> for Vec<pint_abi::types::essential::solution::Mutation> {
            fn from(m: Mutations) -> Self {
                m.set
            }
        }
    }
}

/// The `mutations` and `keys` items for the given keyed vars.
fn items_from_keyed_vars(vars: &[KeyedVarABI]) -> Vec<syn::Item> {
    let mut items: Vec<syn::Item> = vec![
        mutations_struct().into(),
        mutations_fn().into(),
        impl_from_mutations().into(),
        impl_mutations_from_keyed_vars(vars).into(),
    ];
    items.extend(mutations_builders_from_keyed_vars(vars));
    items
}

/// Create a module with `mutations` and `keys` fns for the given keyed vars.
fn mod_from_keyed_vars(mod_name: &str, vars: &[KeyedVarABI]) -> syn::ItemMod {
    let items = items_from_keyed_vars(vars);
    let mod_ident = syn::Ident::new(&mod_name, Span::call_site());
    syn::parse_quote! {
        pub mod #mod_ident {
            //! Items related to simplifying the process of building a `Vec` of
            //! [`Mutation`][pint_abi::types::essential::solution::Mutation]s for a
            //! [`Solution`][pint_abi::types::essential::solution::Solution].
            //!
            //! See the [`mutations`] fn to start constructing a set of [`Mutations`].
            //!
            //! The [`Mutations`] impl provides a set of builder methods that
            //! allow for writing `Mutation`s to an inner `Vec` from higher-level values.
            //!
            //! The final `Vec<Mutation>` can be produced using the `From<Mutations>` impl.
            #(
                #items
            )*
        }
    }
}

/// Given an ABI, generate all items.
fn items_from_abi(abi: &ProgramABI) -> Vec<syn::Item> {
    let mut items = vec![];
    items.extend(items_from_predicates(&abi.predicates));
    if !abi.storage.is_empty() {
        items.push(mod_from_keyed_vars("storage", &abi.storage).into());
    }
    items
}

/// Shorthand for producing tokens given the full deserialized ABI.
fn tokens_from_abi(abi: &ProgramABI) -> TokenStream {
    let items = items_from_abi(&abi);
    items
        .into_iter()
        .map(|item| TokenStream::from(item.into_token_stream()))
        .collect()
}

/// Generate all items from a raw ABI JSON string.
#[proc_macro]
pub fn from_str(input: TokenStream) -> TokenStream {
    let input_lit_str = parse_macro_input!(input as syn::LitStr);
    let string = input_lit_str.value();
    let abi: ProgramABI =
        serde_json::from_str(&string).expect("failed to deserialize str from JSON to `ProgramABI`");
    tokens_from_abi(&abi)
}

/// Generate all items from an ABI JSON file at the given path.
///
/// ## Supported Paths
///
/// The given path must be either:
///
/// 1. An absolute path or
/// 2. A path that is relative to the project's `CARGO_MANIFEST_DIR`.
///
/// The limiation around relative paths is due to having no way to know the path
/// to the source file in which the macro is being invoked in stable Rust.
#[proc_macro]
pub fn from_file(input: TokenStream) -> TokenStream {
    let input_lit_str = parse_macro_input!(input as syn::LitStr);
    let string = input_lit_str.value();
    let mut path = std::path::Path::new(&string).to_path_buf();
    if path.is_relative() {
        // If the path is relative, assume it is relative to the `CARGO_MANIFEST_DIR`.
        let manifest_dir = std::env::var("CARGO_MANIFEST_DIR")
            .expect("`CARGO_MANIFEST_DIR` not set, but required for relative path expansion");
        let manifest_dir_path = std::path::Path::new(&manifest_dir);
        path = manifest_dir_path.join(&path);
    }

    let file = std::fs::File::open(&path).unwrap_or_else(|err| {
        panic!("failed to open {path:?}: {err}");
    });
    let reader = std::io::BufReader::new(file);
    let abi: ProgramABI = serde_json::from_reader(reader).unwrap_or_else(|err| {
        panic!("failed to deserialize {path:?} from JSON to `ProgramABI`: {err}");
    });
    tokens_from_abi(&abi)
}
