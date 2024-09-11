use crate::types::{any, b256, error, int, r#bool, string, tuple, vector, Type};
use std::fmt::{Display, Formatter, Result};

///////////////////
// IntrinsicKind //
///////////////////

#[derive(Clone, Debug)]
pub enum IntrinsicKind {
    External(ExternalIntrinsic),
    Internal(InternalIntrinsic),
    Error,
}

impl Display for IntrinsicKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Self::Internal(kind) => write!(f, "{kind}"),
            Self::External(kind) => write!(f, "{kind}"),
            Self::Error => write!(f, "error"),
        }
    }
}

impl IntrinsicKind {
    pub fn ty(&self) -> Type {
        match self {
            IntrinsicKind::External(kind) => kind.ty(),
            IntrinsicKind::Internal(kind) => kind.ty(),
            IntrinsicKind::Error => error(),
        }
    }

    pub fn args(&self) -> Vec<Type> {
        match self {
            IntrinsicKind::External(kind) => kind.args(),
            IntrinsicKind::Internal(kind) => kind.args(),
            IntrinsicKind::Error => vec![],
        }
    }
}

///////////////////////
// ExternalIntrinsic //
///////////////////////

#[derive(Clone, Debug)]
pub enum ExternalIntrinsic {
    // Returns the address of a predicate in the same contract
    AddressOf,

    // Returns the address of the predicate at a given pathway. Returns both the contract
    // address and the predicate address.
    PredicateAt,

    // Recovers the public key from a secp256k1 signature.
    RecoverSECP256k1,

    // Returns a SHA 256 hash from the specified data.
    Sha256,

    // Returns the length of a state variable.
    StateLen,

    // Returns the content hash of this predicate.
    ThisAddress,

    // Returns the content hash of the contract that this predicate belongs to.
    ThisContractAddress,

    // Returns the "pathway" of this predicate. The pathway of a predicate is the index of
    // the solution data currently being used to check the predicate.
    ThisPathway,

    // Returns the length of a storage vector.
    VecLen,

    // Validates an Ed25519 signature against a public key.
    VerifyEd25519,
}

impl Display for ExternalIntrinsic {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Self::AddressOf => write!(f, "__address_of"),
            Self::PredicateAt => write!(f, "__predicate_at"),
            Self::RecoverSECP256k1 => write!(f, "__recover_secp256k1"),
            Self::Sha256 => write!(f, "__sha256"),
            Self::StateLen => write!(f, "__state_len"),
            Self::ThisAddress => write!(f, "__this_address"),
            Self::ThisContractAddress => write!(f, "__this_contract_address"),
            Self::ThisPathway => write!(f, "__this_pathway"),
            Self::VecLen => write!(f, "__vec_len"),
            Self::VerifyEd25519 => write!(f, "__verify_ed25519"),
        }
    }
}

impl ExternalIntrinsic {
    pub fn args(&self) -> Vec<Type> {
        match self {
            Self::AddressOf => vec![
                string(), // path to a predicate in the contract
            ],
            Self::PredicateAt => vec![
                int(), // pathway
            ],
            Self::RecoverSECP256k1 => vec![
                b256(),                             // data hash
                tuple(vec![b256(), b256(), int()]), // signature
            ],
            Self::Sha256 => vec![
                any(), // data to hash
            ],
            Self::StateLen => vec![
                any(), // path to a state var
            ],
            Self::ThisAddress => vec![],
            Self::ThisContractAddress => vec![],
            Self::ThisPathway => vec![],
            Self::VecLen => vec![
                vector(any()), // storage vector to find the length of
            ],
            Self::VerifyEd25519 => vec![
                any(),                       // data
                tuple(vec![b256(), b256()]), // signature
                b256(),                      // public key
            ],
        }
    }

    pub fn ty(&self) -> Type {
        match self {
            Self::AddressOf => b256(),
            Self::PredicateAt => tuple(vec![b256(), b256()]),
            Self::RecoverSECP256k1 => tuple(vec![b256(), int()]),
            Self::Sha256 => b256(),
            Self::StateLen => int(),
            Self::ThisAddress => b256(),
            Self::ThisContractAddress => b256(),
            Self::ThisPathway => int(),
            Self::VecLen => int(),
            Self::VerifyEd25519 => r#bool(),
        }
    }
}

///////////////////////
// InternalIntrinsic //
///////////////////////

#[derive(Clone, Debug)]
pub enum InternalIntrinsic {
    // Determines if two sets are equal
    EqSet,

    // Returns the set of mutable keys in a solution
    MutKeys,

    // Reads from a local storage key.
    StorageGet,

    // Reads from an external storage key.
    StorageGetExtern,

    // Reads from a "pub var" key at a given pathway.
    PubVar,
}

impl Display for InternalIntrinsic {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Self::EqSet => write!(f, "__eq_set"),
            Self::MutKeys => write!(f, "__mut_keys"),
            Self::StorageGet => write!(f, "__storage_get"),
            Self::StorageGetExtern => write!(f, "__storage_get_extern"),
            Self::PubVar => write!(f, "__pub_var"),
        }
    }
}

impl InternalIntrinsic {
    pub fn args(&self) -> Vec<Type> {
        match self {
            Self::EqSet => vec![
                any(), // lhs - should be "set" if and when we have sets
                any(), // rhs - should be "set" if and when we have sets
            ],
            Self::MutKeys => vec![],
            Self::StorageGet => vec![
                any(), // storage key
            ],
            Self::StorageGetExtern => vec![
                b256(), // external contract address
                any(),  // storage key
            ],
            Self::PubVar => vec![
                int(), // pathway
                any(), // pub var key
            ],
        }
    }

    pub fn ty(&self) -> Type {
        match self {
            Self::EqSet => bool(),
            Self::MutKeys => any(), // should be "set" if and when we have sets.
            Self::StorageGet => any(),
            Self::StorageGetExtern => any(),
            Self::PubVar => any(),
        }
    }
}
