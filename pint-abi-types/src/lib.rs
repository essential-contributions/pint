use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct ContractABI {
    pub predicates: Vec<PredicateABI>,
    pub storage: Vec<VarABI>,
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct PredicateABI {
    pub name: String,
    pub vars: Vec<VarABI>,
}

////////////////////////////////////////
// Decision Variables and their Types //
////////////////////////////////////////

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct VarABI {
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
