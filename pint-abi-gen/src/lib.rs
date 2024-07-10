//! Macros for generating items from pint-generated contract ABI JSON.
//!
//! The entry points for this crate are:
//!
//! - [`from_file!`][crate::from_file!]
//! - [`from_str!`][crate::from_str!]
//!
//! For a given contract, the following items are generated:
//!
//! - A `mod` representing `storage`.
//! - For each `predicate`, a module with the following:
//!     - A `Vars` struct for the predicate's decision variables.
//!     - A `pub_vars` mod for the predicate's public decision variables.
//!
//! The aim for the generated items is to ease the construction of solutions
//! including the encoding of keys, values and mutations from higher-level types.

use pint_abi_types::{ContractABI, KeyedTypeABI, KeyedVarABI, PredicateABI, TupleField, TypeABI};
use pint_abi_visit::{tree_from_keyed_vars, KeyedVarTree, Nesting, NodeIx};
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::ToTokens;
use syn::parse_macro_input;

mod map;
mod tuple;
mod vars;

/// The name of the root module within the predicate set produced by the compiler.
const ROOT_MOD_NAME: &str = "";

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

/// Convert the given pint array to an equivalent Rust array type.
fn ty_from_array(ty: &TypeABI, size: i64) -> syn::Type {
    let syn_ty = ty_from_pint_ty(ty);
    let len = usize::try_from(size).expect("array size out of range of `usize`");
    syn::parse_quote! {
        [#syn_ty; #len]
    }
}

/// Convert the given pint ABI type to an equivalent Rust type.
fn ty_from_pint_ty(ty: &TypeABI) -> syn::Type {
    match ty {
        TypeABI::Bool => syn::parse_quote!(bool),
        TypeABI::Int => syn::parse_quote!(i64),
        TypeABI::Real => syn::parse_quote!(f64),
        TypeABI::String => syn::parse_quote!(String),
        TypeABI::B256 => syn::parse_quote!([i64; 4]),
        TypeABI::Tuple(tuple) => ty_from_tuple(tuple),
        TypeABI::Array { ty, size } => ty_from_array(ty, *size),
    }
}

/// Names for fields are emitted by pint with a `::` prefix.
/// This function checks for the `::` prefix and strips it if it exists.
fn strip_colons_prefix(name: &str) -> &str {
    name.trim_start_matches("::")
}

/// Var names have the `::` prefix, and sometimes have `.` or `@` or `::` separators for
/// flattened tuple fields. We strip the `::` prefix, and replace all `.` or `@` or `::`
/// occurrences with `_`.
fn field_name_from_var_name(name: &str) -> String {
    strip_colons_prefix(name)
        .replace(['.', '@'], "_")
        .replace("::", "_")
}

/// Generate all items for the given predicate.
fn items_from_predicate(predicate: &PredicateABI) -> Vec<syn::Item> {
    let mut items = vec![];
    if !predicate.vars.is_empty() {
        items.extend(vars::items(&predicate.vars));
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
            /// The stack of key elements that need to be merged with the
            /// `&[Nesting]` derived by the `KeyedTypeABI` traversal.
            ///
            /// For example, when a map's `entry` method is called, the provided
            /// key is pushed to this stack. Upon completion of the `entry`
            /// method, the key is popped.
            key_elems: Vec<pint_abi::key::Elem>,
        }
    }
}

fn mutation_method_doc_str(name: &str) -> String {
    format!(
        "Add a mutation for the `{name}` field into the set.\n\n\
        Relaces any existing mutation for the given key.",
    )
}

/// Given a keyed var `Nesting`, create a Rust expression that results in its value.
fn nesting_expr(nesting: &[Nesting]) -> syn::ExprArray {
    let elems = nesting
        .iter()
        .map(|n| {
            let expr: syn::Expr = match n {
                Nesting::Var { ix } => {
                    syn::parse_quote!(pint_abi::visit::Nesting::Var { ix: #ix })
                }
                Nesting::TupleField { ix, flat_ix } => {
                    syn::parse_quote!(pint_abi::visit::Nesting::TupleField { ix: #ix, flat_ix: #flat_ix })
                }
                Nesting::MapEntry { key_size } => {
                    syn::parse_quote!(pint_abi::visit::Nesting::MapEntry { key_size: #key_size })
                }
                Nesting::ArrayElem { array_len } => {
                    syn::parse_quote!(pint_abi::visit::Nesting::ArrayElem { array_len: #array_len })
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

/// A small expression for constructing a key from a keyed var nesting and a
/// `Mutations` builder's `key_elems` stack.
fn construct_key_expr() -> syn::Expr {
    syn::parse_quote! {
        pint_abi::key::construct(&nesting[..], &self.key_elems[..])
    }
}

/// A `Mutations` builder method for a single-key mutation.
fn mutation_method_for_single_key(
    name: &str,
    arg_ty: &SingleKeyTy,
    nesting: &[Nesting],
) -> syn::ImplItemFn {
    let method_ident = syn::Ident::new(name, Span::call_site());
    let abi_key_doc_str = nesting_key_doc_str(nesting);
    let doc_str = format!(
        "{}\n\nKey: `{abi_key_doc_str}`",
        mutation_method_doc_str(name)
    );
    let arg_ty = arg_ty.syn_ty();
    let nesting_expr: syn::ExprArray = nesting_expr(nesting);
    let construct_key_expr: syn::Expr = construct_key_expr();
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

/// Convert the given type `&[Nesting]` into a string to use for a generated
/// builder struct name.
///
/// E.g. `[Var { ix: 1 }, MapEntry { key_ty: i64 }, TupleField { ix: 3, .. }]`
/// becomes `Var1_MapEntry_Tuple3` so that it may be appended to a builder type
/// name, e.g. `Tuple_Var1_MapEntry_Tuple3`.
fn nesting_ty_str<'a>(nesting: impl IntoIterator<Item = &'a Nesting>) -> String {
    fn elem_str(nesting: &Nesting) -> String {
        match nesting {
            Nesting::Var { ix } => ix.to_string(),
            Nesting::TupleField { ix, .. } => ix.to_string(),
            Nesting::MapEntry { .. } => "MapEntry".to_string(),
            Nesting::ArrayElem { .. } => "ArrayElem".to_string(),
        }
    }
    let mut iter = nesting.into_iter();
    let mut s = elem_str(iter.next().expect("nesting must contain at least one item"));
    for n in iter {
        use std::fmt::Write;
        write!(&mut s, "_{}", elem_str(n)).expect("failed to fmt nesting ty str");
    }
    s
}

/// Given a type nesting, create a string for presenting the associated key in docs.
///
/// E.g. `[0, 1, _, _, _, _, 6, 7]`.
fn nesting_key_doc_str(nesting: &[Nesting]) -> String {
    use core::fmt::Write;
    let partial_key = pint_abi_visit::partial_key_from_nesting(nesting);
    let mut s = "[".to_string();
    let mut opts = partial_key.iter();
    fn write_opt(s: &mut String, opt: &Option<i64>) {
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

/// Extract the key from a keyed type.
fn abi_key_from_keyed_type(ty: &KeyedTypeABI) -> &Vec<Option<usize>> {
    match ty {
        KeyedTypeABI::Bool(key) => key,
        KeyedTypeABI::Int(key) => key,
        KeyedTypeABI::Real(key) => key,
        KeyedTypeABI::Array { ty, .. } => abi_key_from_keyed_type(ty),
        KeyedTypeABI::String(key) => key,
        KeyedTypeABI::B256(key) => key,
        KeyedTypeABI::Tuple(fields) => abi_key_from_keyed_type(&fields[0].ty),
        KeyedTypeABI::Map { key, .. } => key,
    }
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

/// Recursively traverse the given keyed var and create a builder struct for each tuple.
fn mutations_builders_from_node(tree: &KeyedVarTree, n: NodeIx) -> Vec<syn::Item> {
    let mut items = vec![];
    match tree[n].ty {
        KeyedTypeABI::Map {
            ty_from,
            ty_to,
            key: _,
        } => {
            items.extend(map::builder_items(tree, n, ty_from, ty_to));
        }
        KeyedTypeABI::Tuple(_fields) => {
            items.extend(tuple::builder_items(tree, n));
        }
        _ => (),
    }
    items
}

/// Recursively traverse the given keyed vars and create a builder structs and
/// impls for each tuple.
fn mutations_builders_from_keyed_vars(vars: &[KeyedVarABI]) -> Vec<syn::Item> {
    let mut items = vec![];
    let tree = pint_abi_visit::tree_from_keyed_vars(vars);
    tree.dfs(|n| {
        items.extend(mutations_builders_from_node(&tree, n));
    });
    items
}

/// A `Mutations` builder method for a map field.
fn mutation_method_for_map(name: &str, map_nesting: &[Nesting]) -> syn::ImplItemFn {
    let struct_name = map::mutations_struct_name(map_nesting);
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
fn mutation_method_for_tuple(name: &str, nesting: &[Nesting]) -> syn::ImplItemFn {
    let struct_name = tuple::mutations_struct_name(nesting);
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
fn mutation_method_from_node(tree: &KeyedVarTree, n: NodeIx, name: &str) -> syn::ImplItemFn {
    let nesting = tree.nesting(n);
    let arg_ty = match &tree[n].ty {
        KeyedTypeABI::Bool(_key) => SingleKeyTy::Bool,
        KeyedTypeABI::Int(_key) => SingleKeyTy::Int,
        KeyedTypeABI::Real(_key) => SingleKeyTy::Real,
        KeyedTypeABI::String(_key) => SingleKeyTy::String,
        KeyedTypeABI::B256(_key) => SingleKeyTy::B256,
        KeyedTypeABI::Array { ty: _, size: _ } => todo!(),
        // Tuple types take a closure.
        KeyedTypeABI::Tuple(_) => {
            return mutation_method_for_tuple(name, &nesting);
        }
        // Map types take a closure.
        KeyedTypeABI::Map { .. } => {
            return mutation_method_for_map(name, &nesting);
        }
    };
    // A mutation builder method for a single mutation.
    mutation_method_for_single_key(name, &arg_ty, &nesting)
}

/// All builder methods for the `Mutations builder type.
fn mutations_methods_from_keyed_vars(vars: &[KeyedVarABI]) -> Vec<syn::ImplItemFn> {
    let tree = tree_from_keyed_vars(vars);
    tree.roots()
        .iter()
        .map(|&n| {
            let name = field_name_from_var_name(tree[n].name.unwrap());
            mutation_method_from_node(&tree, n, &name)
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
///
/// This is used for both `storage` and `pub_vars` mod generation.
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
///
/// This is used for both `storage` and `pub_vars` mod generation.
fn mod_from_keyed_vars(mod_name: &str, vars: &[KeyedVarABI]) -> syn::ItemMod {
    let items = items_from_keyed_vars(vars);
    let mod_ident = syn::Ident::new(mod_name, Span::call_site());
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
fn items_from_abi(abi: &ContractABI) -> Vec<syn::Item> {
    let mut items = vec![];
    items.extend(items_from_predicates(&abi.predicates));
    if !abi.storage.is_empty() {
        items.push(mod_from_keyed_vars("storage", &abi.storage).into());
    }
    items
}

/// Shorthand for producing tokens given the full deserialized ABI.
fn tokens_from_abi(abi: &ContractABI) -> TokenStream {
    let items = items_from_abi(abi);
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
    let abi: ContractABI = serde_json::from_str(&string)
        .expect("failed to deserialize str from JSON to `ContractABI`");
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
/// The limitation around relative paths is due to having no way to know the path
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
    let abi: ContractABI = serde_json::from_reader(reader).unwrap_or_else(|err| {
        panic!("failed to deserialize {path:?} from JSON to `ContractABI`: {err}");
    });
    tokens_from_abi(&abi)
}
