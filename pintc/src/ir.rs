#![allow(dead_code)]
#![allow(unused_variables)]

use crate::{
    error::{ErrorEmitted, Handler},
    //expr::{Expr, ExternalIntrinsic, Immediate, IntrinsicKind},
    predicate, //::{ConstraintDecl, Contract, ExprKey, Predicate, Variable},
               //span::{empty_span, Span, Spanned},
};

pub fn compile_contract(
    handler: &Handler,
    contract: &predicate::Contract,
) -> Result<Contract, ErrorEmitted> {
    let preds = contract
        .preds
        .iter()
        .map(|(key, pred)| compile_pred(handler, contract, pred, key))
        .collect::<Result<_, _>>()?;

    Ok(Contract { preds })
}

pub struct Contract {
    preds: Vec<Predicate>,
}

fn compile_pred(
    handler: &Handler,
    contract: &predicate::Contract,
    predicate: &predicate::Predicate,
    pred_key: predicate::PredKey,
) -> Result<Predicate, ErrorEmitted> {
    // Iterate for each constraint and build a sea of nodes.
    todo!()
}

slotmap::new_key_type! { pub struct NodeKey; }

pub struct Predicate {
    sea_of_nodes: slotmap::SlotMap<NodeKey, Node>,
}

// All nodes must maintain 'use-def' relationships.  All uses and defs must be reciprocated and
// kept in sync at all times.
struct Node {
    category: NodeCategory,
    kind: NodeKind,
    inputs: Vec<NodeKey>,  // Nodes which this node uses.
    outputs: Vec<NodeKey>, // Nodes which use this node.
}

enum NodeCategory {
    Data,
    Control,
    Scope,
}

enum NodeKind {
    ConstrainedData,
    ConstrainedState,
    AsmOp,
    Projection,
    SymTable,
}
