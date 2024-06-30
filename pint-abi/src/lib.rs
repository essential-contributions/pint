//! Encoding, decoding and other helpers for working with the Pint Essential ABI.

#[doc(inline)]
pub use encode::Encode;
use essential_types::Word;
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
pub fn encode<T: Encode>(t: &T) -> Vec<Word> {
    let mut v = vec![];
    t.encode(&mut v)
        .expect("encoding into `Vec<Word>` cannot error");
    v
}
