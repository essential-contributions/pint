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
pub use encode::Encode;
#[doc(inline)]
pub use write::Write;

mod encode;
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

/// Shorthand for encoding the given value into a `Vec` of Essential `Word`s.
pub fn encode<T: Encode>(t: &T) -> Vec<types::essential::Word> {
    let mut v = vec![];
    t.encode(&mut v)
        .expect("encoding into `Vec<Word>` cannot error");
    v
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
