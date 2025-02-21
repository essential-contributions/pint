use crate::types::{any, b256, error, int, optional, r#bool, string, tuple, vector, Type};
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

#[derive(Clone, Debug, PartialEq)]
pub enum ExternalIntrinsic {
    // Returns the address of a predicate in the same contract
    AddressOf,

    // Panics if supplied condition is `true`
    PanicIf,

    // Recovers the public key from a secp256k1 signature.
    RecoverSECP256k1,

    // Returns a SHA 256 hash from the specified data.
    Sha256,

    // Returns the size, in words, of an expression.
    SizeOf,

    // Returns the content hash of this predicate.
    ThisAddress,

    // Returns the content hash of the contract that this predicate belongs to.
    ThisContractAddress,

    // Returns the length of a storage vector.
    VecLen,

    // Validates an Ed25519 signature against a public key.
    VerifyEd25519,
}

impl Display for ExternalIntrinsic {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Self::AddressOf => write!(f, "__address_of"),
            Self::PanicIf => write!(f, "__panic_if"),
            Self::RecoverSECP256k1 => write!(f, "__recover_secp256k1"),
            Self::Sha256 => write!(f, "__sha256"),
            Self::SizeOf => write!(f, "__size_of"),
            Self::ThisAddress => write!(f, "__this_address"),
            Self::ThisContractAddress => write!(f, "__this_contract_address"),
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
            Self::PanicIf => vec![
                r#bool(), // path to a predicate in the contract
            ],
            Self::RecoverSECP256k1 => vec![
                b256(),                             // data hash
                tuple(vec![b256(), b256(), int()]), // signature
            ],
            Self::Sha256 => vec![
                any(), // data to hash
            ],
            Self::SizeOf => vec![any()],
            Self::ThisAddress => vec![],
            Self::ThisContractAddress => vec![],
            Self::VecLen => vec![
                optional(vector(any())), // storage vector to find the length of
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
            Self::PanicIf => any(),
            Self::RecoverSECP256k1 => tuple(vec![b256(), int()]),
            Self::Sha256 => b256(),
            Self::SizeOf => int(),
            Self::ThisAddress => b256(),
            Self::ThisContractAddress => b256(),
            Self::VecLen => optional(int()),
            Self::VerifyEd25519 => r#bool(),
        }
    }
}

///////////////////////
// InternalIntrinsic //
///////////////////////

#[derive(Clone, Debug, PartialEq)]
pub enum InternalIntrinsic {
    // Determines if two sets are equal
    EqSet,

    // Returns the set of mutable keys in a solution
    MutKeys,

    // Reads local pre state.
    PreState,

    // Reads external pre state.
    PreStateExtern,

    // Reads local post state.
    PostState,

    // Reads external post state.
    PostStateExtern,
}

impl Display for InternalIntrinsic {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Self::EqSet => write!(f, "__eq_set"),
            Self::MutKeys => write!(f, "__mut_keys"),
            Self::PreState => write!(f, "__pre_state"),
            Self::PreStateExtern => write!(f, "__pre_state_extern"),
            Self::PostState => write!(f, "__post_state"),
            Self::PostStateExtern => write!(f, "__post_state_extern"),
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
            Self::PreState => vec![
                any(), // storage key
            ],
            Self::PreStateExtern => vec![
                b256(), // external contract address
                any(),  // storage key
            ],
            Self::PostState => vec![
                any(), // storage key
            ],
            Self::PostStateExtern => vec![
                b256(), // external contract address
                any(),  // storage key
            ],
        }
    }

    pub fn ty(&self) -> Type {
        match self {
            Self::EqSet => bool(),
            Self::MutKeys => any(), // should be "set" if and when we have sets.
            Self::PreState => any(),
            Self::PreStateExtern => any(),
            Self::PostState => any(),
            Self::PostStateExtern => any(),
        }
    }
}
