use crate::types::{any, b256, int, r#bool, string, tuple, unknown, vector, Type};
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
        use ExternalIntrinsic::*;
        match self {
            IntrinsicKind::External(kind) => match kind {
                PredicateAt => tuple(vec![b256(), b256()]),
                RecoverSECP256k1 => tuple(vec![b256(), int()]),
                Sha256 => b256(),
                SiblingPredicateAddress => b256(),
                StateLen => int(),
                ThisAddress => b256(),
                ThisContractAddress => b256(),
                ThisPathway => int(),
                VecLen => int(),
                VerifyEd25519 => r#bool(),
            },
            IntrinsicKind::Internal(_) => {
                // Internal intrinsics are a bit of a mess right now. We'll do this later. These
                // are not user facing so it's not that critical to formalize their types yet.
                todo!()
            }
            IntrinsicKind::Error => unknown(),
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
    PredicateAt,
    RecoverSECP256k1,
    Sha256,
    SiblingPredicateAddress,
    StateLen,
    ThisAddress,
    ThisContractAddress,
    ThisPathway,
    VecLen,
    VerifyEd25519,
}

impl Display for ExternalIntrinsic {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Self::PredicateAt => write!(f, "__predicate_at"),
            Self::RecoverSECP256k1 => write!(f, "__recover_secp256k1"),
            Self::Sha256 => write!(f, "__sha256"),
            Self::SiblingPredicateAddress => write!(f, "__sibling_predicate_address"),
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
            Self::PredicateAt => vec![int()],
            Self::RecoverSECP256k1 => vec![b256(), tuple(vec![b256(), b256(), int()])],
            Self::Sha256 => vec![any()],
            Self::StateLen => vec![any()], // slightly problematic
            Self::SiblingPredicateAddress => vec![string()], // slightly problematic
            Self::ThisAddress => vec![],
            Self::ThisContractAddress => vec![],
            Self::ThisPathway => vec![],
            Self::VecLen => vec![vector(any())], // slightly problematic
            Self::VerifyEd25519 => vec![any(), tuple(vec![b256(), b256()]), b256()],
        }
    }
}

///////////////////////
// InternalIntrinsic //
///////////////////////

#[derive(Clone, Debug)]
pub enum InternalIntrinsic {
    EqSet,
    MutKeys,
    StorageGet,
    StorageGetExtern,
    Transient,
}

impl Display for InternalIntrinsic {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Self::EqSet => write!(f, "__eq_set"),
            Self::MutKeys => write!(f, "__mut_keys"),
            Self::StorageGet => write!(f, "__storage_get"),
            Self::StorageGetExtern => write!(f, "__storage_get_extern"),
            Self::Transient => write!(f, "__transient"),
        }
    }
}

impl InternalIntrinsic {
    pub fn args(&self) -> Vec<Type> {
        // Internal intrinsics are a bit of a mess right now. We'll do this later. These are not
        // user facing so it's not that critical to formalize their arguments yet.
        todo!()
    }
}
