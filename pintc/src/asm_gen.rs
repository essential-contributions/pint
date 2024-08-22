use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::{Expr, ExternalIntrinsic, Immediate, IntrinsicKind},
    predicate::{ConstraintDecl, Contract, Predicate},
    span::empty_span,
};
use asm_builder::AsmBuilder;
use essential_types::{
    predicate::{Directive, Predicate as CompiledPredicate},
    ContentAddress,
};
use petgraph::{graph::NodeIndex, Graph};
use std::collections::{BTreeMap, HashMap};

mod asm_builder;
mod display;
#[cfg(test)]
mod tests;

#[derive(Debug, Default, Clone)]
pub struct CompiledContract {
    pub names: Vec<String>,
    pub salt: [u8; 32],
    pub predicates: Vec<CompiledPredicate>,
}

/// Convert a `Contract` into `CompiledContract`
pub fn compile_contract(
    handler: &Handler,
    contract: &Contract,
) -> Result<CompiledContract, ErrorEmitted> {
    let mut predicate_names = Vec::new();
    let mut compiled_predicates = Vec::new();

    // This is a dependnecy graph between predicates. Predicates may depend on other predicates via
    // predicate instances that reference other predicates in the same contract
    let mut dep_graph = Graph::<String, ()>::new();

    // This map keeps track of what node indices (in the dependency graph) are assigned to what
    // predicates (identified by names)
    let mut dep_graph_indices = HashMap::<String, NodeIndex>::new();

    // This is a map between node indices in the dependency graph and references to the
    // corresponding predicates.
    let mut predicates = HashMap::<NodeIndex, &Predicate>::new();

    for (_, pred) in contract.preds.iter() {
        let new_node = dep_graph.add_node(pred.name.clone());
        dep_graph_indices.insert(pred.name.clone(), new_node);
        predicates.insert(new_node, pred);
    }

    for (pred_key, pred) in contract.preds.iter() {
        // If this predicate references another predicate (via a `SiblingPredicateAddress`
        // intrinsic), then create a dependency edge from the other pedicate to this one.
        for expr in contract.exprs(pred_key) {
            if let Some(Expr::IntrinsicCall {
                kind: (IntrinsicKind::External(ExternalIntrinsic::SiblingPredicateAddress), _),
                args,
                ..
            }) = expr.try_get(contract)
            {
                if let Some(Expr::Immediate {
                    value: Immediate::String(s),
                    ..
                }) = args.first().and_then(|name| name.try_get(contract))
                {
                    let from = dep_graph_indices[s];
                    let to = dep_graph_indices[&pred.name];
                    dep_graph.add_edge(from, to, ());
                }
            }
        }
    }

    // Predicates should be sorted topologically based on the dependency graph. That way, predicate
    // addresses that are required by other predicates are known in time.
    let Ok(sorted_predicates) = petgraph::algo::toposort(&dep_graph, None) else {
        return Err(handler.emit_err(Error::Compile {
            error: CompileError::Internal {
                msg: "dependency cycles between predicates should have been caught before",
                span: empty_span(),
            },
        }));
    };

    // Now compile all predicates in topological order
    let mut predicate_addresses: BTreeMap<String, ContentAddress> = Default::default();
    for idx in &sorted_predicates {
        let predicate = predicates[idx];

        if let Ok(compiled_predicate) = handler
            .scope(|handler| compile_predicate(handler, contract, &predicate_addresses, predicate))
        {
            predicate_names.push(predicate.name.clone());
            predicate_addresses.insert(
                predicate.name.clone(),
                essential_hash::content_addr(&compiled_predicate),
            );
            compiled_predicates.push(compiled_predicate);
        }
    }

    if handler.has_errors() {
        Err(handler.cancel())
    } else {
        Ok(CompiledContract {
            names: predicate_names,
            // Salt is not used by pint yet.
            salt: Default::default(),
            predicates: compiled_predicates,
        })
    }
}

/// Converts a `crate::Predicate` into a `CompiledPredicate` which
/// includes generating assembly for the constraints and for state reads.
pub fn compile_predicate(
    handler: &Handler,
    contract: &Contract,
    predicate_addresses: &BTreeMap<String, ContentAddress>,
    pred: &Predicate,
) -> Result<CompiledPredicate, ErrorEmitted> {
    let mut builder = AsmBuilder {
        s_asm: Vec::new(),
        c_asm: Vec::new(),
        predicate_addresses,
    };

    // Compile all state declarations into state programs
    for (_, state) in pred.states() {
        builder.compile_state(handler, state, contract, pred)?;
    }

    // Compile all constraint declarations into constraint programs
    for ConstraintDecl {
        expr: constraint, ..
    } in &pred.constraints
    {
        builder.compile_constraint(handler, constraint, contract, pred)?;
    }

    if handler.has_errors() {
        return Err(handler.cancel());
    }

    Ok(CompiledPredicate {
        state_read: builder
            .s_asm
            .iter()
            .map(|s_asm| state_asm::to_bytes(s_asm.iter().copied()).collect())
            .collect(),
        constraints: builder
            .c_asm
            .iter()
            .map(|c_asm| constraint_asm::to_bytes(c_asm.iter().copied()).collect())
            .collect(),
        directive: Directive::Satisfy,
    })
}
