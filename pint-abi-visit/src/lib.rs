//! Items to assist traversal of Pint ABI Keyed Vars.
//!
//! This assists in traversal of the `storage` and `pub_vars` sections of the Pint ABI.

use essential_types::Word;
use pint_abi_types::{KeyedTypeABI, KeyedVarABI, TypeABI};

/// Represents how a pub var or storage field's key is constructed.
pub enum KeyElem {
    /// A fixed word element, e.g. for a top-level field or tuple field.
    Word(Word),
    /// A key element provided by a map key. If the map key size is fixed, `len
    MapKey {
        /// The type of the key.
        ty: TypeABI,
    },
    /// A key element provided by an array index.
    ArrayIx {
        /// The total length of the array if it's known.
        array_len: Option<usize>,
    },
}

/// Represents a visited keyed var or nested type alongside it's key.
pub struct Keyed<'a> {
    /// The name of the var if it is named.
    pub name: Option<&'a str>,
    /// The type of the var.
    pub ty: &'a KeyedTypeABI,
    /// A description of the key construction.
    pub key: &'a [KeyElem],
}

/// Visit all keyed vars in `storage` or `pub_vars` alongside their associated key.
pub fn keyed_vars(vars: &[KeyedVarABI], mut visit: impl FnMut(Keyed)) {
    /// Call `visit` with the given `var` and `key`, then recurse nested vars.
    fn visit_and_recurse(
        name: Option<&str>,
        ty: &KeyedTypeABI,
        key_elems: &mut Vec<KeyElem>,
        visit: &mut impl FnMut(Keyed),
    ) {
        visit(Keyed {
            name,
            ty,
            key: &key_elems[..],
        });
        match ty {
            KeyedTypeABI::Bool(_key)
            | KeyedTypeABI::Int(_key)
            | KeyedTypeABI::Real(_key)
            | KeyedTypeABI::String(_key)
            | KeyedTypeABI::B256(_key) => {}

            // Recurse for nested tuple types.
            KeyedTypeABI::Tuple(fields) => {
                for (i, field) in fields.iter().enumerate() {
                    let ix_word = Word::try_from(i).expect("index out of range");
                    let key_elem = KeyElem::Word(ix_word);
                    let name = field.name.as_deref();
                    key_elems.push(key_elem);
                    visit_and_recurse(name, &field.ty, key_elems, visit);
                    key_elems.pop();
                }
            }

            // Recurse for nested array element types.
            KeyedTypeABI::Array { ty, size } => {
                let array_len = Some(usize::try_from(*size).expect("size out of range"));
                let key_elem = KeyElem::ArrayIx { array_len };
                key_elems.push(key_elem);
                visit_and_recurse(None, &ty, key_elems, visit);
                key_elems.pop();
            }

            // Recurse for nested map element types.
            KeyedTypeABI::Map {
                ty_from,
                ty_to,
                key: _,
            } => {
                let ty = ty_from.clone();
                let key_elem = KeyElem::MapKey { ty };
                key_elems.push(key_elem);
                visit_and_recurse(None, &ty_to, key_elems, visit);
                key_elems.pop();
            }
        }
    }

    let mut key = vec![];
    for (i, var) in vars.iter().enumerate() {
        let ix_word = Word::try_from(i).expect("index out of range");
        let key_elem = KeyElem::Word(ix_word);
        key.push(key_elem);
        visit_and_recurse(Some(var.name.as_str()), &var.ty, &mut key, &mut visit);
        key.pop();
    }
}

/// The number of words used to represent an ABI type in key form.
pub fn ty_size(ty: &TypeABI) -> usize {
    match ty {
        TypeABI::Bool | TypeABI::Int | TypeABI::Real => 1,
        TypeABI::B256 => 4,
        TypeABI::String => panic!("unknown size of type `string`"),
        TypeABI::Tuple(fields) => fields.iter().map(|f| ty_size(&f.ty)).sum(),
        TypeABI::Array { ty, size } => {
            ty_size(ty) * usize::try_from(*size).expect("size out of range")
        }
    }
}

/// Construct an ABI key in the form output by the compiler given a sequence of key elements.
fn _abi_key_from_elems(elems: &[KeyElem]) -> Vec<Option<usize>> {
    let mut key = vec![];
    for elem in elems {
        match elem {
            KeyElem::Word(w) => key.push(Some(usize::try_from(*w).expect("out of range"))),
            KeyElem::MapKey { ty } => key.extend(vec![None; ty_size(ty)]),
            KeyElem::ArrayIx { .. } => key.push(None),
        }
    }
    key
}
