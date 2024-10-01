use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::{Expr, ExternalIntrinsic, Immediate, IntrinsicKind},
    predicate::{ConstraintDecl, Contract, Predicate},
    span::empty_span,
};
use asm_builder::AsmBuilder;
use essential_types::{predicate::Predicate as CompiledPredicate, ContentAddress};
use petgraph::{graph::NodeIndex, Graph};
use std::collections::HashMap;

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
    // This is a dependnecy graph between predicates. Predicates may depend on other predicates via
    // predicate instances that reference other predicates in the same contract
    let mut dep_graph = Graph::<String, ()>::new();

    // This map keeps track of what node indices (in the dependency graph) are assigned to what
    // predicates (identified by names)
    let mut names_to_indices = HashMap::<String, NodeIndex>::new();

    // This is a map between node indices in the dependency graph and references to the
    // corresponding predicates.
    let mut indices_to_predicates = HashMap::<NodeIndex, &Predicate>::new();

    for (_, pred) in contract.preds.iter() {
        let new_node = dep_graph.add_node(pred.name.clone());
        names_to_indices.insert(pred.name.clone(), new_node);
        indices_to_predicates.insert(new_node, pred);
    }

    for (pred_key, pred) in contract.preds.iter() {
        // If this predicate references another predicate (via a `AddressOf` intrinsic), then
        // create a dependency edge from the other pedicate to this one.
        for expr in contract.exprs(pred_key) {
            if let Some(Expr::IntrinsicCall {
                kind: (IntrinsicKind::External(ExternalIntrinsic::AddressOf), _),
                args,
                ..
            }) = expr.try_get(contract)
            {
                if let Some(Expr::Immediate {
                    value: Immediate::String(name),
                    ..
                }) = args.first().and_then(|name| name.try_get(contract))
                {
                    let from = names_to_indices[name];
                    let to = names_to_indices[&pred.name];
                    dep_graph.add_edge(from, to, ());
                }
            }
        }
    }

    // Predicates should be sorted topologically based on the dependency graph. That way, predicate
    // addresses that are required by other predicates are known in time.
    let Ok(sorted_nodes) = petgraph::algo::toposort(&dep_graph, None) else {
        return Err(handler.emit_err(Error::Compile {
            error: CompileError::Internal {
                msg: "dependency cycles between predicates should have been caught before",
                span: empty_span(),
            },
        }));
    };

    // This map keeps track of the compiled predicates and their addresses. We will use this later
    // when producing the final compiled contract. It is also useful when compiling predicates that
    // require the addresses of other predicates.
    let mut compiled_predicates: HashMap<String, (CompiledPredicate, ContentAddress)> =
        HashMap::new();

    // Now compile all predicates in topological order
    for idx in &sorted_nodes {
        let predicate = indices_to_predicates[idx];

        if let Ok(compiled_predicate) = handler
            .scope(|handler| compile_predicate(handler, contract, &compiled_predicates, predicate))
        {
            let compiled_predicate_address = essential_hash::content_addr(&compiled_predicate);
            compiled_predicates.insert(
                predicate.name.clone(),
                (compiled_predicate, compiled_predicate_address),
            );
        }
    }

    // Now, produce the two vectors needed for `CompiledContract`: A vector of all the predicate
    // names and a vector of all the compiled predicates. Note that the order here must match the
    // original order in `contract.preds`.
    let (names, predicates) = contract
        .preds
        .iter()
        .map(|(_, pred)| {
            compiled_predicates
                .remove(&pred.name)
                .map(|(compiled_predicate, _)| (pred.name.clone(), compiled_predicate))
                .ok_or_else(|| {
                    handler.emit_err(Error::Compile {
                        error: CompileError::Internal {
                            msg: "predicate must exist in the compiled_predicates map",
                            span: empty_span(),
                        },
                    })
                })
        })
        .collect::<Result<_, _>>()?;

    if handler.has_errors() {
        Err(handler.cancel())
    } else {
        Ok(CompiledContract {
            names,
            salt: Default::default(), // Salt is not used by pint yet.
            predicates,
        })
    }
}

/// Converts a `crate::Predicate` into a `CompiledPredicate` which
/// includes generating assembly for the constraints and for state reads.
pub fn compile_predicate(
    handler: &Handler,
    contract: &Contract,
    compiled_predicates: &HashMap<String, (CompiledPredicate, ContentAddress)>,
    pred: &Predicate,
) -> Result<CompiledPredicate, ErrorEmitted> {
    let mut builder = AsmBuilder::new(compiled_predicates);

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
            .state_programs
            .iter()
            .map(|state_programs| state_asm::to_bytes(state_programs.iter().copied()).collect())
            .collect(),
        constraints: builder
            .constraint_programs
            .iter()
            .map(|constraint_programs| {
                constraint_asm::to_bytes(constraint_programs.iter().copied()).collect()
            })
            .collect(),
    })
}
