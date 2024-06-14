use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct ProgramABI {
    pub intents: Vec<IntentABI>,
    pub storage: Vec<KeyedVarABI>,
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct IntentABI {
    pub name: String,
    pub vars: Vec<VarABI>,
    pub pub_vars: Vec<KeyedVarABI>,
}

////////////////////////////////////////
// Decision Variables and their Types //
////////////////////////////////////////

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct VarABI {
    pub name: String,
    pub ty: TypeABI,
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct TupleField {
    pub name: Option<String>,
    pub ty: TypeABI,
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum TypeABI {
    Bool,
    Int,
    Real,
    String,
    B256,
    Tuple(Vec<TupleField>),
}

//////////////////////////////////////////////////////////////////
// Pub Decision Variables and Storage Variables and their Types //
// Compared to the above, these variables have keys             //
//////////////////////////////////////////////////////////////////

pub type Key = Vec<Option<usize>>;

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct KeyedVarABI {
    pub name: String,
    pub ty: KeyedTypeABI,
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct KeyedTupleField {
    pub name: Option<String>,
    pub ty: KeyedTypeABI,
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum KeyedTypeABI {
    Bool(Key),
    Int(Key),
    Real(Key),
    String(Key),
    B256(Key),
    Tuple {
        fields: Vec<KeyedTupleField>,
        key: Key,
    },
    Map {
        ty_from: TypeABI, // not in storage hence `TypeABI` instead of `KeyedTypeABI`
        ty_to: Box<Self>,
        key: Key,
    },
}
