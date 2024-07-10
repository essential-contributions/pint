//! Items related to key construction.

#[doc(inline)]
pub use crate::visit::partial_key_from_nesting as partial_from_nesting;
use crate::{
    types::essential::{Key, Word},
    visit::Nesting,
};

/// Represents a dynamically provided key element (e.g. map key or array index).
///
/// ABI-generated `Mutations` builders maintain a stack of key `Elem`s as users
/// enter nested map/array entries.
#[derive(Debug)]
pub enum Elem {
    /// A key element is provided by a map key.
    MapKey(Key),
    /// A key element is provided by an array index.
    ArrayIx(usize),
}

/// Construct a full key for a given nested type and the associated stack of
/// dynamically provided key elements.
pub fn construct(nesting: &[Nesting], key_elems: &[Elem]) -> Key {
    let mut words = vec![];
    let mut nesting_iter = nesting.iter().peekable();
    let mut key_elems_iter = key_elems.iter();
    while let Some(nesting) = nesting_iter.next() {
        match nesting {
            // Push the storage/pub-var index directly. This is always the first nesting.
            Nesting::Var { ix } => {
                let word = Word::try_from(*ix).expect("out of Word range");
                words.push(word);
            }

            // If we've encountered a tuple field, flatten any further tuple
            // nestings and use the deepest `flat_ix` which represents the
            // flattened index of the tuple field.
            Nesting::TupleField { ix: _, flat_ix } => {
                let mut deepest_flat_ix = *flat_ix;
                while let Some(Nesting::TupleField { ix: _, flat_ix }) = nesting_iter.peek() {
                    deepest_flat_ix = *flat_ix;
                    nesting_iter.next();
                }
                let word = Word::try_from(deepest_flat_ix).expect("out of Word range");
                words.push(word);
            }

            // If this is a map entry, take the map key from the key elements.
            Nesting::MapEntry { .. } => {
                let key = expect_map_key(key_elems_iter.next());
                words.extend(key);
            }

            // If this is an array element, determine the key from the array
            // index. For directly nested arrays, detemine the flattened index
            // by multiplying the index by the inner array lenghts.
            Nesting::ArrayElem { array_len: _ } => {
                let mut ix = expect_array_ix(key_elems_iter.next());
                while let Some(Nesting::ArrayElem { array_len }) = nesting_iter.peek() {
                    ix = ix * array_len + expect_array_ix(key_elems_iter.next());
                    nesting_iter.next();
                }
                let word = Word::try_from(ix).expect("out of Word range");
                words.push(word);
            }
        }
    }
    words
}

fn expect_array_ix(elem: Option<&Elem>) -> usize {
    match elem {
        Some(Elem::ArrayIx(ix)) => *ix,
        elem => unreachable!(
            "invalid key `Elem` provided by ABI-generated code: \
            expected array index, found {elem:?}",
        ),
    }
}

fn expect_map_key(elem: Option<&Elem>) -> Key {
    match elem {
        Some(Elem::MapKey(key)) => key.clone(),
        elem => unreachable!(
            "invalid key `Elem` provided by ABI-generated code: \
            expected map key, found {elem:?}",
        ),
    }
}
