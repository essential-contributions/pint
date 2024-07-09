//! Items to assist traversal of Pint ABI Keyed Vars.
//!
//! This assists in traversal of the `storage` and `pub_vars` sections of the Pint ABI.

use pint_abi_types::{KeyedTupleField, KeyedTypeABI, KeyedVarABI, TypeABI};

/// A stack of `Nesting` describes how a [`Keyed`] var type is nested.
pub enum Nesting {
    /// A top-level storage field of pub var.
    ///
    /// This is always the first element in a `Keyed`'s `nesting: [Nesting]` field.
    Var {
        /// Represents the index of the storage field or pub var.
        ix: usize,
    },
    /// The `Keyed` var is within a tuple field.
    TupleField {
        /// The field index of the var within the tuple (not flattened).
        ix: usize,
        /// The flattened index of the var within a tree of directly nested tuples.
        ///
        /// This is required to match the way that pintc flattens tuples, which
        /// influences how their keys are constructed.
        flat_ix: usize,
    },
    /// A key element provided by a map key.
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
    /// Describes how the keyed var is nested within `storage` or `pub vars`.
    ///
    /// The first element is always
    pub nesting: &'a [Nesting],
}

/// Visit all keyed vars in `storage` or `pub_vars` alongside their associated key.
pub fn keyed_vars(vars: &[KeyedVarABI], mut visit: impl FnMut(Keyed)) {
    /// Call `visit` with the given `var` and `key`, then recurse nested vars.
    fn visit_and_recurse(
        name: Option<&str>,
        ty: &KeyedTypeABI,
        nesting: &mut Vec<Nesting>,
        visit: &mut impl FnMut(Keyed),
    ) {
        visit(Keyed {
            name,
            ty,
            nesting: &nesting[..],
        });
        match ty {
            KeyedTypeABI::Bool(_key)
            | KeyedTypeABI::Int(_key)
            | KeyedTypeABI::Real(_key)
            | KeyedTypeABI::String(_key)
            | KeyedTypeABI::B256(_key) => {}

            // Recurse for nested tuple types.
            KeyedTypeABI::Tuple(fields) => {
                for (ix, field) in fields.iter().enumerate() {
                    let flat_ix = {
                        let start_flat_ix = match nesting.last() {
                            Some(Nesting::TupleField { flat_ix, .. }) => *flat_ix,
                            _ => 0,
                        };
                        flattened_ix(fields, ix, start_flat_ix)
                    };
                    let name = field.name.as_deref();
                    nesting.push(Nesting::TupleField { ix, flat_ix });
                    visit_and_recurse(name, &field.ty, nesting, visit);
                    nesting.pop();
                }
            }

            // Recurse for nested array element types.
            KeyedTypeABI::Array { ty, size } => {
                let array_len = Some(usize::try_from(*size).expect("size out of range"));
                nesting.push(Nesting::ArrayIx { array_len });
                visit_and_recurse(None, &ty, nesting, visit);
                nesting.pop();
            }

            // Recurse for nested map element types.
            KeyedTypeABI::Map {
                ty_from,
                ty_to,
                key: _,
            } => {
                let ty = ty_from.clone();
                nesting.push(Nesting::MapKey { ty });
                visit_and_recurse(None, &ty_to, nesting, visit);
                nesting.pop();
            }
        }
    }

    let mut key = vec![];
    for (ix, var) in vars.iter().enumerate() {
        let key_elem = Nesting::Var { ix };
        key.push(key_elem);
        visit_and_recurse(Some(var.name.as_str()), &var.ty, &mut key, &mut visit);
        key.pop();
    }
}

/// Determine the flattened index of the tuple field at the given field index
/// within the given `fields`.
///
/// The `start_flat_ix` represents the flat index of the enclosing tuple.
fn flattened_ix(fields: &[KeyedTupleField], field_ix: usize, start_flat_ix: usize) -> usize {
    // Given a slice of tuple fields, determine the total number of leaf fields
    // within the directly nested tree of tuples.
    fn count_flattened_leaf_fields(fields: &[KeyedTupleField]) -> usize {
        fields
            .iter()
            .map(|field| match field.ty {
                KeyedTypeABI::Tuple(ref fields) => count_flattened_leaf_fields(fields),
                _ => 1,
            })
            .sum()
    }
    start_flat_ix + count_flattened_leaf_fields(&fields[0..field_ix])
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
