//! Encoding, decoding and other helpers for working with the Pint Essential ABI.

// The `pint_abi_gen` macros require that the `pint_abi` crate is available, so
// we re-export the ABI item-generation macros here so that users don't need to
// import both crates separately.
#[cfg(feature = "pint-abi-gen")]
#[doc(inline)]
pub use pint_abi_gen::from_file as gen_from_file;
#[cfg(feature = "pint-abi-gen")]
#[doc(inline)]
pub use pint_abi_gen::from_str as gen_from_str;
#[doc(inline)]
pub use pint_abi_visit as visit;

#[doc(inline)]
pub use encode::Encode;
#[doc(inline)]
pub use write::Write;

use std::path::Path;
use thiserror::Error;
use types::{
    essential::{contract::Contract, predicate::Predicate},
    ContractABI, PredicateABI,
};

mod encode;
pub mod key;
mod write;

/// Both Pint ABI and Essential protocol types.
pub mod types {
    // Re-export the contents of `pint_abi_types` here so that it is accesible
    // via `pint_abi::types`.
    #[doc(inline)]
    pub use pint_abi_types::*;
    // Re-export the Essential protocol types behind the `essential` namespace.
    #[doc(inline)]
    pub use essential_types as essential;
}

/// Errors that might occur when loading a [`ContractABI`] or [`Contract`] from a path.
#[derive(Debug, Error)]
pub enum FromPathError {
    /// An I/O error occurred.
    #[error("an I/O error occurred: {0}")]
    Io(#[from] std::io::Error),
    /// Failed to deserialize from JSON.
    #[error("failed to deserialize the ABI from JSON: {0}")]
    Json(#[from] serde_json::Error),
}

/// Shorthand for encoding the given value into a `Vec` of Essential `Word`s.
pub fn encode<T: Encode>(t: &T) -> Vec<types::essential::Word> {
    let mut v = vec![];
    t.encode(&mut v)
        .expect("encoding into `Vec<Word>` cannot error");
    v
}

/// Shorthand for loading the [`ContractABI`] from a given ABI JSON file path.
///
/// By default, after building a pint package this will be located within the
/// package's output directory at `out/<profile>/<name>-abi.json`.
pub fn from_path(path: &Path) -> Result<ContractABI, FromPathError> {
    let json_str = std::fs::read_to_string(path)?;
    let abi = serde_json::from_str(&json_str)?;
    Ok(abi)
}

/// Shorthand for loading a [`Contract`] from a given JSON file path.
///
/// By default, after building a pint package this will be located within the
/// package's output directory at `out/<profile>/<name>.json`.
pub fn contract_from_path(path: &Path) -> Result<Contract, FromPathError> {
    let json_str = std::fs::read_to_string(path)?;
    let abi = serde_json::from_str(&json_str)?;
    Ok(abi)
}

/// Given a `Contract` and its associated `ContractABI`, find and return the
/// predicate with the given name.
///
/// Returns the predicate ABI alongside the predicate itself.
pub fn find_predicate<'a>(
    contract: &'a Contract,
    abi: &'a ContractABI,
    pred_name: &str,
) -> Option<(&'a Predicate, &'a PredicateABI)> {
    // Currently, the ABI always includes the root predicate, even if the
    // contract does not. Here, we determine whether or not the root predicate
    // should be skipped. We skip if it is not the only predicate and it exists
    // as the first predicate, otherwise we assume it is the only predicate and
    // should be included in the search.
    const ROOT_NAME: &str = "";
    let skip_root_predicate = abi.predicates.len() > 1
        && matches!(abi.predicates.first(), Some(p) if p.name == ROOT_NAME);

    // Skip the root predicate from the ABI here.
    let mut abi_predicates = abi.predicates.iter();
    if skip_root_predicate {
        abi_predicates.next();
    }

    // Find the matching predicate entry and return it alongside its ABI.
    contract
        .predicates
        .iter()
        .zip(abi_predicates)
        .find(|(_, pred_abi)| predicate_name_matches(&pred_abi.name, pred_name))
}

/// Checks if the predicate name matches exactly, and if not checks if the
/// predicate name matches with the `::` prefix.
fn predicate_name_matches(abi_pred_name: &str, pred_name: &str) -> bool {
    abi_pred_name == pred_name || abi_pred_name.split("::").nth(1) == Some(pred_name)
}

/// This function is used by `pint-abi-gen`-generated mutation builder methods in
/// order to merge a stack of map entry keys into a the full ABI-compatible key.
///
/// ## Example
///
/// Given the following:
///
/// - `abi_key`: `[Some(1), None, Some(3), None, None, Some(6)]`
/// - `key_stack`: `[[2], [4, 5]]`
///
/// Produces an expression with the value `[1, 2, 3, 4, 5, 6]`.
#[doc(hidden)]
pub fn __merge_key(
    abi_key: &[Option<types::essential::Word>],
    key_stack: &[types::essential::Key],
) -> types::essential::Key {
    let mut key_stack_words = key_stack.iter().flatten().copied();
    abi_key
        .iter()
        .copied()
        .map(|opt: Option<types::essential::Word>| {
            opt.or_else(|| key_stack_words.next())
                .expect("failed to merge key: missing words for key")
        })
        .collect()
}
