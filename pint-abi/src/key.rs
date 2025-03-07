//! Items related to key construction.

use crate::types::essential::{Key, Word};

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

/// Represents statically provided information about a storage var's type nesting to assist with
/// key construction.
///
/// This is similar to the `pint_abi_visit::Nesting` type, but only contains nesting information
/// relevant to key construction.
#[derive(Debug)]
pub enum Nesting {
    Var { ix: usize },
    MapEntry,
    TupleField { flat_ix: usize },
    ArrayElem { elem_len: usize },
}

/// Construct a full key for a given nested type and the associated stack of
/// dynamically provided key elements.
pub fn construct(nesting: &[Nesting], key_elems: &[Elem]) -> Key {
    let mut nesting_iter = nesting.iter().peekable();
    let mut key_elems_iter = key_elems.iter();
    let mut words = vec![];
    while let Some(nesting) = nesting_iter.next() {
        match nesting {
            // Push the storage var index directly. This is always the first nesting.
            Nesting::Var { ix } => {
                let word = Word::try_from(*ix).expect("out of `Word` range");
                words.push(word);
            }
            // If this is a map entry, take the map key from the key elements.
            Nesting::MapEntry => {
                let key = expect_map_key(key_elems_iter.next());
                words.extend(key);
            }
            // If we've encountered a tuple field, flatten any further tuple
            // nestings and use the deepest `flat_ix` which represents the
            // flattened index of the tuple field.
            Nesting::TupleField { flat_ix } => {
                let word = flattened_word(0, *flat_ix, &mut nesting_iter, &mut key_elems_iter);
                words.push(word);
            }
            // If this is an array element, determine the key from the array
            // index. For directly nested arrays, detemine the flattened index
            // by multiplying the index by the inner array lenghts.
            Nesting::ArrayElem { elem_len } => {
                let arr_ix = elem_len * expect_array_ix(key_elems_iter.next());
                let word = flattened_word(arr_ix, 0, &mut nesting_iter, &mut key_elems_iter);
                words.push(word);
            }
        }
    }
    words
}

/// Flattens directly nested tuple and array nestings into a single word for a key.
///
/// Flattening begins once an array or tuple nesting is encountered.
///
/// If the first nesting is an array, provide the `array_ix` and a zeroed
/// `tuple_flat_ix`.
///
/// If the first nesting is a tuple, provide the `tuple_flat_ix` and a zeroed
/// `array_ix`.
///
/// The `nestings` iterator's `next` method will only be called for each
/// directly nested array and tuple.
///
/// The `key_elems` iterator's `next` method will only be called for each
/// directly nested array.
fn flattened_word<'a>(
    mut array_ix: usize,
    mut tuple_flat_ix: usize,
    nestings: &mut std::iter::Peekable<impl Iterator<Item = &'a Nesting>>,
    key_elems: &mut impl Iterator<Item = &'a Elem>,
) -> Word {
    let mut sum_ix = 0;
    while let Some(nesting) = nestings.peek() {
        match nesting {
            Nesting::ArrayElem { elem_len } => {
                // If the previous nesting was a tuple field, add its flattened ix to the sum.
                sum_ix += tuple_flat_ix;
                tuple_flat_ix = 0;
                // Sum the new array index.
                array_ix += elem_len * expect_array_ix(key_elems.next());
            }
            Nesting::TupleField { flat_ix } => {
                // If the previous nesting was an array nesting, add it to the sum.
                sum_ix += array_ix;
                array_ix = 0;
                // Sum the nested tuple's flat index onto the current.
                tuple_flat_ix += *flat_ix;
            }
            // If we encounter a non-array, non-tuple nesting, flattening is complete.
            _ => break,
        }
        nestings.next();
    }
    sum_ix += array_ix + tuple_flat_ix;
    Word::try_from(sum_ix).expect("out of `Word` range")
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
