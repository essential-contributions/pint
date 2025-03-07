use serde::{Deserialize, Serialize};

/// A contract that consists of a set of predicates and a set of storage variables
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct ContractABI {
    pub predicates: Vec<PredicateABI>,
    pub storage: Vec<StorageVarABI>,
}

/// A predicate in a contract that has a name and a list of parameters
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct PredicateABI {
    pub name: String,
    pub params: Vec<ParamABI>,
}

/// A predicate parameter
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct ParamABI {
    pub name: String,
    pub ty: TypeABI,
}

/// A storage variable
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct StorageVarABI {
    pub name: String,
    pub ty: TypeABI,
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct TupleField {
    pub name: Option<String>,
    pub ty: TypeABI,
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct UnionVariant {
    pub name: String,
    pub ty: Option<TypeABI>,
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum TypeABI {
    Bool,
    Int,
    Real,
    String,
    B256,
    Optional(Box<Self>),
    Tuple(Vec<TupleField>),
    Array {
        ty: Box<Self>,
        size: i64,
    },
    Union {
        name: String,
        variants: Vec<UnionVariant>,
    },
    Map {
        ty_from: Box<Self>,
        ty_to: Box<Self>,
    },
}
