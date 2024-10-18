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
//!     - A `PubVars` struct for the predicate's public decision variables.
//!
//! The aim for the generated items is to ease the construction of solutions
//! including the encoding of keys, values and mutations from higher-level types.

use addr::Addresses;
use essential_types::{contract::Contract, PredicateAddress};
use pint_abi_types::{ContractABI, PredicateABI, TupleField, TypeABI, UnionVariant, VarABI};
use pint_abi_visit::Nesting;
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::ToTokens;
use std::collections::BTreeSet;
use syn::parse_macro_input;

mod addr;
mod args;
mod array;
mod keys;
mod map;
mod mutations;
mod pub_vars;
mod tuple;
mod unions;
mod utils;
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
    Union(String /* Pint path to the union*/),
}

impl SingleKeyTy {
    /// The type of the builder method value.
    fn syn_ty(&self, mod_level: usize) -> syn::Type {
        match self {
            SingleKeyTy::Bool => syn::parse_quote!(bool),
            SingleKeyTy::Int => syn::parse_quote!(i64),
            SingleKeyTy::Real => syn::parse_quote!(f64),
            SingleKeyTy::String => syn::parse_quote!(String),
            SingleKeyTy::B256 => syn::parse_quote!([i64; 4]),
            SingleKeyTy::Union(name) => unions::ty_from_union(name, mod_level),
        }
    }
}

/// Convert the given pint tuple fields to unnamed Rust fields.
fn fields_from_tuple_fields(fields: &[TupleField], mod_level: usize) -> Vec<syn::Field> {
    fields
        .iter()
        .map(|TupleField { name, ty }| {
            // NOTE: Currently we ignore tuple field names.
            let _name = name;
            let ty = ty_from_pint_ty(ty, mod_level);
            syn::parse_quote!(#ty)
        })
        .collect()
}

/// Convert the given pint tuple to an equivalent Rust tuple type.
fn ty_from_tuple(tuple: &[TupleField], mod_level: usize) -> syn::Type {
    let fields = fields_from_tuple_fields(tuple, mod_level);
    syn::parse_quote! {
        ( #( #fields ),* )
    }
}

/// Convert the given pint array to an equivalent Rust array type.
fn ty_from_array(ty: &TypeABI, size: i64, mod_level: usize) -> syn::Type {
    let syn_ty = ty_from_pint_ty(ty, mod_level);
    let len = usize::try_from(size).expect("array size out of range of `usize`");
    syn::parse_quote! {
        [#syn_ty; #len]
    }
}

/// Convert the given pint ABI type to an equivalent Rust type.
fn ty_from_pint_ty(ty: &TypeABI, mod_level: usize) -> syn::Type {
    match ty {
        TypeABI::Bool => syn::parse_quote!(bool),
        TypeABI::Int => syn::parse_quote!(i64),
        TypeABI::Real => syn::parse_quote!(f64),
        TypeABI::String => syn::parse_quote!(String),
        TypeABI::B256 => syn::parse_quote!([i64; 4]),
        TypeABI::Union { name, .. } => unions::ty_from_union(name, mod_level),
        TypeABI::Tuple(tuple) => ty_from_tuple(tuple, mod_level),
        TypeABI::Array { ty, size } => ty_from_array(ty, *size, mod_level),
        TypeABI::Map { .. } => unreachable!("Maps are not allowed as non-storage types"),
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
fn items_from_predicate(
    predicate: &PredicateABI,
    addr: Option<&PredicateAddress>,
) -> Vec<syn::Item> {
    let mut items = vec![];
    if let Some(addr) = addr {
        items.push(addr::predicate_const(&addr.contract, &addr.predicate).into());
    }
    if !predicate.vars.is_empty() {
        items.extend(vars::items(&predicate.vars));
    }
    if !predicate.pub_vars.is_empty() {
        items.extend(pub_vars::items(&predicate.pub_vars));
    }
    items
}

/// Generate a module for the given predicate.
/// Modules are only generated for named predicates.
fn mod_from_predicate(
    name: &str,
    predicate: &PredicateABI,
    addr: Option<PredicateAddress>,
) -> syn::ItemMod {
    let doc_str = format!("Items for the `{name}` predicate.");
    let ident = syn::Ident::new(name, Span::call_site());
    let items = items_from_predicate(predicate, addr.as_ref());
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

/// Zip the predicates with their addresses.
fn predicates_with_addrs<'a>(
    predicates: &'a [PredicateABI],
    addrs: Option<&'a Addresses>,
) -> impl 'a + Iterator<Item = (&'a PredicateABI, Option<PredicateAddress>)> {
    predicates.iter().enumerate().map(move |(ix, predicate)| {
        let addr = addrs.map(|addrs| PredicateAddress {
            contract: addrs.contract.clone(),
            predicate: addrs.predicates[ix].clone(),
        });
        (predicate, addr)
    })
}

/// Generate a module for each named predicate.
fn mods_from_named_predicates(
    predicates: &[PredicateABI],
    addrs: Option<&Addresses>,
) -> Vec<syn::ItemMod> {
    predicates_with_addrs(predicates, addrs)
        .filter(|(predicate, addr)| !is_predicate_empty(predicate) || addr.is_some())
        .filter(|(predicate, _)| predicate.name != ROOT_MOD_NAME)
        .map(|(predicate, addr)| {
            let name = strip_colons_prefix(&predicate.name);
            mod_from_predicate(name, predicate, addr)
        })
        .collect()
}

/// Find the root predicate.
fn find_root_predicate<'a>(
    predicates: &'a [PredicateABI],
    addrs: Option<&'a Addresses>,
) -> Option<(&'a PredicateABI, Option<PredicateAddress>)> {
    predicates_with_addrs(predicates, addrs).find(|(predicate, _)| predicate.name == ROOT_MOD_NAME)
}

/// Given the set of predicates, generate all items.
///
/// This includes a module for each named predicate, and types for the root predicate.
fn items_from_predicates(predicates: &[PredicateABI], addrs: Option<&Addresses>) -> Vec<syn::Item> {
    let mut items = vec![];
    // Add the root predicate items.
    if let Some((root_pred, addr)) = find_root_predicate(predicates, addrs) {
        // By default, the root predicate has no name. We name its predicate module `root`.
        let name = "root";
        items.push(mod_from_predicate(name, root_pred, addr).into());
    }
    // Add the named predicate modules.
    items.extend(
        mods_from_named_predicates(predicates, addrs)
            .into_iter()
            .map(syn::Item::from),
    );
    items
}

/// Given a keyed var `Nesting`, create a Rust expression that results in its value.
fn nesting_expr(nesting: &[Nesting]) -> syn::ExprArray {
    let elems = nesting
        .iter()
        .map(|n| {
            let expr: syn::Expr = match n {
                Nesting::Var { ix } => {
                    syn::parse_quote!(pint_abi::key::Nesting::Var { ix: #ix })
                }
                Nesting::TupleField { ix: _, flat_ix } => {
                    syn::parse_quote!(pint_abi::key::Nesting::TupleField { flat_ix: #flat_ix })
                }
                Nesting::MapEntry { key_size: _ } => {
                    syn::parse_quote!(pint_abi::key::Nesting::MapEntry)
                }
                Nesting::ArrayElem { elem_len, .. } => {
                    syn::parse_quote!(pint_abi::key::Nesting::ArrayElem { elem_len: #elem_len })
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

/// Convert the given type `&[Nesting]` into a string to use for a generated
/// builder struct name.
///
/// E.g. `[Var { ix: 1 }, MapEntry { key_ty: i64 }, TupleField { ix: 3, .. }]`
/// becomes `Var1_MapEntry_Tuple3` so that it may be appended to a builder type
/// name, e.g. `Tuple_1_MapEntry_3`.
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

/// The `mutations` and `keys` items for the given keyed vars.
///
/// This is used for both `storage` and `pub_vars` mod generation.
fn items_from_keyed_vars(vars: &[VarABI]) -> Vec<syn::Item> {
    let mut items = vec![];

    // The `mutations` module and re-exports.
    items.push(mutations::module(vars).into());
    items.push(syn::parse_quote! {
        #[doc(inline)]
        pub use mutations::{mutations, Mutations};
    });

    // The `keys` module and re-exports.
    items.push(keys::module(vars).into());
    items.push(syn::parse_quote! {
        #[doc(inline)]
        pub use keys::{keys, Keys};
    });

    items
}

/// Create a module with `mutations` and `keys` fns for the given keyed vars.
///
/// This is used for `storage` mod generation.
fn mod_from_keyed_vars(mod_name: &str, vars: &[VarABI]) -> syn::ItemMod {
    let items = items_from_keyed_vars(vars);
    let mod_ident = syn::Ident::new(mod_name, Span::call_site());
    syn::parse_quote! {
        pub mod #mod_ident {
            //! Items related to simplifying the process of building sets of
            //! [`Mutation`][pint_abi::types::essential::solution::Mutation]s and
            //! [`Key`][pint_abi::types::essential::Key]s for
            //! [`Solution`][pint_abi::types::essential::solution::Solution]s and queries.
            //!
            //!
            //! See the [`mutations`](./fn.mutations.html) fn to start constructing
            //! a set of [`Mutations`].
            //!
            //! See the [`keys`](./fn.mutations.html) fn to start constructing a set
            //! of [`Keys`].
            //!
            //! The [`Mutations`] and [`Keys`] impls provides a set of builder
            //! methods that allow for writing `Mutation`s and `Key`s to an
            //! inner `Vec` from higher-level values.
            //!
            //! The final `Vec<Mutation>` or `Vec<Key>` can be produced using the
            //! `From<Mutations>` or `From<Keys>` conversion impls.
            #(
                #items
            )*
        }
    }
}

/// Given an ABI, generate all items.
fn items_from_abi_and_addrs(abi: &ContractABI, addrs: Option<&Addresses>) -> Vec<syn::Item> {
    let mut items = vec![];

    // Collect all the union types encountered in the contract
    let mut unions: BTreeSet<(Vec<String>, Vec<UnionVariant>)> = BTreeSet::new();
    abi.storage
        .iter()
        .chain(
            abi.predicates
                .iter()
                .flat_map(|predicate| predicate.vars.iter().chain(predicate.pub_vars.iter())),
        )
        .for_each(|var| unions::collect_unions(&var.ty, &mut unions));

    items.extend(unions::items_from_unions(&unions));

    items.extend(items_from_predicates(&abi.predicates, addrs));
    if let Some(addrs) = addrs {
        items.push(addr::contract_const(&addrs.contract).into());
    }
    if !abi.storage.is_empty() {
        items.push(mod_from_keyed_vars("storage", &abi.storage).into());
    }
    items
}

/// Shorthand for producing tokens given the full deserialized ABI.
fn tokens_from_abi_and_addrs(abi: &ContractABI, addrs: Option<&Addresses>) -> TokenStream {
    let items = items_from_abi_and_addrs(abi, addrs);
    items
        .into_iter()
        .map(|item| TokenStream::from(item.into_token_stream()))
        .collect()
}

/// Given a path specified as an argument to the `from_file!` macro, resolve
/// whether it's relative to the `CARGO_MANIFEST_DIR` or absolute.
fn resolve_path(path: &std::path::Path) -> std::path::PathBuf {
    if path.is_relative() {
        let manifest_dir = std::env::var("CARGO_MANIFEST_DIR")
            .expect("`CARGO_MANIFEST_DIR` not set, but required for relative path expansion");
        let manifest_dir_path = std::path::Path::new(&manifest_dir);
        manifest_dir_path.join(path)
    } else {
        path.to_path_buf()
    }
}

/// Generate all items from a raw ABI JSON string.
#[proc_macro]
pub fn from_str(input: TokenStream) -> TokenStream {
    let input_lit_str = parse_macro_input!(input as syn::LitStr);
    let string = input_lit_str.value();
    let abi: ContractABI = serde_json::from_str(&string)
        .expect("failed to deserialize str from JSON to `ContractABI`");
    tokens_from_abi_and_addrs(&abi, None)
}

/// Read and deserialize an instance of type `T` from the JSON file.
fn read_from_json_file<T>(path: &std::path::Path) -> Result<T, serde_json::Error>
where
    T: for<'de> serde::Deserialize<'de>,
{
    let file = std::fs::File::open(path).unwrap_or_else(|err| {
        panic!("failed to open {path:?}: {err}");
    });
    let reader = std::io::BufReader::new(file);
    serde_json::from_reader(reader)
}

/// Generate all items from an ABI JSON file at the given path.
///
/// Also allows for optionally providing a path to the associated contract JSON
/// for generating `ADDRESS` consts for the contract and its predicates.
///
/// NOTE: This macro is designed for use via the `pint-abi` crate, from which
/// it is re-exported as `pint_abi::gen_from_file!`. Much of the code generation
/// assumes the `pint-abi` crate is available as a dependency.
///
/// ## Usage
///
/// ```ignore
/// pint_abi::gen_from_file! {
///     abi: "path/to/abi.json",
///     contract: "path/to/contract.json",
/// }
/// ```
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
    let args = parse_macro_input!(input as args::FromFile);

    // Load the contract ABI.
    let abi_path = resolve_path(args.abi.value().as_ref());
    let abi: ContractABI = read_from_json_file(&abi_path).unwrap_or_else(|err| {
        panic!("failed to deserialize {abi_path:?} from JSON to `ContractABI`: {err}");
    });

    // Load the contract itself if provided.
    let contract: Option<Contract> = args.contract.map(|contract| {
        let contract_path = resolve_path(contract.value().as_ref());
        read_from_json_file(&contract_path).unwrap_or_else(|err| {
            panic!("failed to deserialize {contract_path:?} from JSON to `Contract`: {err}");
        })
    });

    // Collect the contract and predicate addresses.
    let addrs = contract.as_ref().map(Addresses::from);

    tokens_from_abi_and_addrs(&abi, addrs.as_ref())
}
