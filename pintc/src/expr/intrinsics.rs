use crate::types::{any, b256, int, r#bool, tuple, vector, Type};

#[derive(Clone, Debug)]
pub enum ExternalIntrinsic {
    PredicateAt,
    RecoverSECP256k1,
    Sha256,
    StateLen,
    ThisAddress,
    ThisContractAddress,
    ThisPathway,
    VecLen,
    VerifyEd25519,
}

#[derive(Clone, Debug)]
pub enum InternalIntrinsic {
    EqSet,
    MutKeys,
    SiblingPredicateAddress,
    StorageGet,
    StorageGetExtern,
    Transient,
}

#[derive(Clone, Debug)]
pub enum IntrinsicKind {
    External(ExternalIntrinsic),
    Internal(InternalIntrinsic),
}

impl IntrinsicKind {
    pub fn ty(&self) -> Type {
        use ExternalIntrinsic::*;
        match self {
            IntrinsicKind::External(kind) => match kind {
                PredicateAt => tuple(vec![b256(), b256()]),
                RecoverSECP256k1 => tuple(vec![b256(), int()]),
                Sha256 => b256(),
                StateLen => int(),
                ThisAddress => b256(),
                ThisContractAddress => b256(),
                ThisPathway => int(),
                VecLen => int(),
                VerifyEd25519 => r#bool(),
            },
            IntrinsicKind::Internal(_) => todo!(),
        }
    }

    pub fn args(&self) -> Vec<Type> {
        use ExternalIntrinsic::*;
        match self {
            IntrinsicKind::External(kind) => match kind {
                PredicateAt => vec![int()],
                RecoverSECP256k1 => vec![b256(), tuple(vec![b256(), b256(), int()])],
                Sha256 => vec![any()],
                StateLen => vec![any()], // slightly problematic
                ThisAddress => vec![],
                ThisContractAddress => vec![],
                ThisPathway => vec![],
                VecLen => vec![vector(any())], // slightly problematic
                VerifyEd25519 => vec![any(), tuple(vec![b256(), b256()]), b256()],
            },
            IntrinsicKind::Internal(_) => todo!(),
        }
    }
}
