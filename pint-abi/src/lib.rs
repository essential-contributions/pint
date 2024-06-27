//! Encoding, decoding and other helpers for working with the Pint Essential ABI.

#[doc(inline)]
pub use encode::Encode;
use essential_types::Word;
#[doc(inline)]
pub use write::Write;

mod encode;
mod write;

/// Shorthand for encoding the given value into a `Vec` of Essential `Word`s.
pub fn encode<T: Encode>(t: &T) -> Vec<Word> {
    let mut v = vec![];
    t.encode(&mut v)
        .expect("encoding into `Vec<Word>` cannot error");
    v
}

// pub fn decode<T: Decode>(words: &[Word]) -> Result<T, T::Error> {
//     todo!()
// }
