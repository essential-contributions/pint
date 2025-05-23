use crate::types::{any, b256, dyn_array, error, int, r#bool, string, tuple, Type};
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

    // Determines the length of an array.
    ArrayLen,

    // Panics the VM if the provided condition is true
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

    // Validates an Ed25519 signature against a public key.
    VerifyEd25519,
}

impl Display for ExternalIntrinsic {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Self::AddressOf => write!(f, "__address_of"),
            Self::ArrayLen => write!(f, "__len"),
            Self::PanicIf => write!(f, "__panic_if"),
            Self::RecoverSECP256k1 => write!(f, "__recover_secp256k1"),
            Self::Sha256 => write!(f, "__sha256"),
            Self::SizeOf => write!(f, "__size_of"),
            Self::ThisAddress => write!(f, "__this_address"),
            Self::ThisContractAddress => write!(f, "__this_contract_address"),
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
            Self::ArrayLen => vec![dyn_array(any())],
            Self::PanicIf => vec![
                r#bool(), // panic condition
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
            Self::ArrayLen => int(),
            Self::PanicIf => any(),
            Self::RecoverSECP256k1 => tuple(vec![b256(), int()]),
            Self::Sha256 => b256(),
            Self::SizeOf => int(),
            Self::ThisAddress => b256(),
            Self::ThisContractAddress => b256(),
            Self::VerifyEd25519 => r#bool(),
        }
    }
}

///////////////////////
// InternalIntrinsic //
///////////////////////

#[derive(Clone, Debug, PartialEq)]
pub enum InternalIntrinsic {
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
            Self::PreState => any(),
            Self::PreStateExtern => any(),
            Self::PostState => any(),
            Self::PostStateExtern => any(),
        }
    }
}
