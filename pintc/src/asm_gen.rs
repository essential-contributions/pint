use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::{Expr, ExternalIntrinsic, Immediate, InternalIntrinsic, IntrinsicKind},
    predicate::{ConstraintDecl, Contract, ExprKey, Predicate, Variable},
    span::{empty_span, Span, Spanned},
};

use asm_builder::AsmBuilder;
use either::Either;
use essential_types::{
    predicate::{Edge, Node, Predicate as CompiledPredicate, Program, Reads},
    ContentAddress,
};
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use petgraph::{graph::NodeIndex, visit::EdgeRef, Graph};
use std::collections::BTreeSet;

mod asm_builder;
mod display;
#[cfg(test)]
mod tests;

#[derive(Debug, Default, Clone)]
pub struct CompiledContract {
    pub contract: essential_types::contract::Contract,

    // 1. predicate_metadata.0 is the name of the predicate
    // 2. predicate_metadata.1[i] contains the index of the compute graph node corresponding to
    //    constraint i
    // 3. predicate_metadata.2[i] contains the index of the compute graph node corresponding to
    //    variable i
    pub predicate_metadata: Vec<(String, Vec<usize>, Vec<usize>)>,
    pub programs: BTreeSet<Program>,
}

/// Convert a `Contract` into `CompiledContract`
pub fn compile_contract(
    handler: &Handler,
    salt: [u8; 32],
    contract: &Contract,
) -> Result<CompiledContract, ErrorEmitted> {
    // This is a dependency graph between predicates. Predicates may depend on other predicates via
    // predicate instances that reference other predicates in the same contract
    let mut dep_graph = Graph::<String, ()>::new();

    // This map keeps track of what node indices (in the dependency graph) are assigned to what
    // predicates (identified by names)
    let mut names_to_indices = FxHashMap::<String, NodeIndex>::default();

    // This is a map between node indices in the dependency graph and references to the
    // corresponding predicates.
    let mut indices_to_predicates = FxHashMap::<NodeIndex, &Predicate>::default();

    for (_, pred) in contract.preds.iter() {
        let new_node = dep_graph.add_node(pred.name.name.clone());
        names_to_indices.insert(pred.name.name.clone(), new_node);
        indices_to_predicates.insert(new_node, pred);
    }

    for (pred_key, pred) in contract.preds.iter() {
        // If this predicate refers to another predicate using a `LocalPredicateCall` or an
        // `AddressOf` intrinsic, then create a dependency edge from the other pedicate to this
        // one.
        for expr in contract.exprs(pred_key) {
            if let Some(Expr::LocalPredicateCall { predicate, .. }) = expr.try_get(contract) {
                let from = names_to_indices[predicate];
                let to = names_to_indices[&pred.name.name];
                dep_graph.add_edge(from, to, ());
            } else if let Some(Expr::IntrinsicCall {
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
                    let to = names_to_indices[&pred.name.name];
                    dep_graph.add_edge(from, to, ());
                }
            }
        }
    }

    // Predicates should be sorted topologically based on the dependency graph. That way, predicate
    // addresses that are required by other predicates are known in time.
    let Ok(sorted_nodes) = petgraph::algo::toposort(&dep_graph, None) else {
        return Err(handler.emit_internal_err(
            "dependency cycles between predicates should have been caught before",
            empty_span(),
        ));
    };

    // This map keeps track of the compiled predicates and their addresses. We will use this later
    // when producing the final compiled contract. It is also useful when compiling predicates that
    // require the addresses of other predicates.
    let mut compiled_predicates: CompiledPredicates = FxHashMap::default();

    // Now compile all predicates in topological order
    for idx in &sorted_nodes {
        let predicate = indices_to_predicates[idx];

        if let Ok((compiled_predicate, programs, c_nodes, v_nodes)) = handler
            .scope(|handler| compile_predicate(handler, contract, &compiled_predicates, predicate))
        {
            let compiled_predicate_address = essential_hash::content_addr(&compiled_predicate);
            compiled_predicates.insert(
                predicate.name.name.clone(),
                (
                    compiled_predicate,
                    compiled_predicate_address,
                    programs,
                    c_nodes,
                    v_nodes,
                    predicate.span.clone(),
                ),
            );
        }
    }

    // Now check for duplicate predicates
    let mut unique_addresses: FxHashSet<&ContentAddress> = FxHashSet::default();
    let mut original_predicates: FxHashMap<&ContentAddress, (&String, &Span)> =
        FxHashMap::default();

    let mut compiled_predicates_vec: Vec<_> = compiled_predicates.iter().collect();
    compiled_predicates_vec.reverse(); // Guarantees the error message refers to the first declaration of the predicate

    for (string, (_, content_address, _, _, _, span)) in &compiled_predicates_vec {
        if !unique_addresses.insert(content_address) {
            let original_predicate = original_predicates
                .get(content_address)
                .expect("predicate name guaranteed to exist");
            let mut original_span = original_predicate.1;
            let mut span = span;

            // Ensure the error label appears before the hint label
            if span.start() < original_span.start() {
                (original_span, span) = (span, original_span);
            }

            handler.emit_err(Error::Compile {
                error: CompileError::IdenticalPredicates {
                    original_name: original_predicate.0.to_string(),
                    duplicate_name: string.to_string(),
                    original_span: original_span.clone(),
                    span: span.clone(),
                },
            });
        } else {
            original_predicates.insert(content_address, (string, span));
        }
    }

    // Now, produce the two vectors needed for `CompiledContract`: A vector of all the predicate
    // names and a vector of all the compiled predicates. Note that the order here must match the
    // original order in `contract.preds`.
    let mut combined_programs = BTreeSet::new();
    let (predicate_metadata, predicates) = contract
        .preds
        .iter()
        .map(|(_, pred)| {
            compiled_predicates
                .remove(&pred.name.name)
                .map(|(compiled_predicate, _, programs, c_nodes, v_nodes, _)| {
                    combined_programs.extend(programs);
                    (
                        (pred.name.name.clone(), c_nodes, v_nodes),
                        compiled_predicate,
                    )
                })
                .ok_or_else(|| {
                    handler.emit_internal_err(
                        "predicate must exist in the compiled_predicates map",
                        empty_span(),
                    )
                })
        })
        .collect::<Result<_, _>>()?;

    if handler.has_errors() {
        Err(handler.cancel())
    } else {
        Ok(CompiledContract {
            predicate_metadata,
            contract: essential_types::contract::Contract { predicates, salt },
            programs: combined_programs,
        })
    }
}

type CompiledPredicates = FxHashMap<
    String,
    (
        CompiledPredicate,
        ContentAddress,
        BTreeSet<Program>,
        Vec<usize>,
        Vec<usize>,
        Span,
    ),
>;

#[derive(Clone, Debug)]
enum NodeType {
    Temp {
        expr: ExprKey,
        reads: Reads,
    },
    NonLeaf {
        index: usize,
        var: Variable,
        reads: Reads,
    },
    Leaf {
        index: usize,
        expr: ExprKey,
        reads: Reads,
    },
}

impl NodeType {
    fn is_leaf(&self) -> bool {
        matches!(self, NodeType::Leaf { .. })
    }

    fn expr(&self) -> ExprKey {
        match self {
            Self::NonLeaf { var, .. } => var.expr,
            Self::Leaf { expr, .. } | Self::Temp { expr, .. } => *expr,
        }
    }

    fn span(&self, contract: &Contract) -> Span {
        match self {
            Self::NonLeaf { var, .. } => var.span.clone(),
            Self::Leaf { expr, .. } | Self::Temp { expr, .. } => expr.get(contract).span().clone(),
        }
    }
}

type CompilePredicateResult =
    Result<(CompiledPredicate, BTreeSet<Program>, Vec<usize>, Vec<usize>), ErrorEmitted>;

/// Converts a `crate::Predicate` into a `CompiledPredicate` which
/// includes generating assembly for the constraints and for variable reads.
pub fn compile_predicate(
    handler: &Handler,
    contract: &Contract,
    compiled_predicates: &CompiledPredicates,
    pred: &Predicate,
) -> CompilePredicateResult {
    let mut data_flow_graph = Graph::<NodeType, ()>::new();
    let mut vars_to_nodes = FxHashMap::<(String, Reads), NodeIndex>::default();

    for (index, (_, variable)) in pred.variables().enumerate() {
        vars_to_nodes.insert(
            (variable.name.clone(), Reads::Pre),
            data_flow_graph.add_node(NodeType::NonLeaf {
                index,
                var: variable.clone(),
                reads: Reads::Pre,
            }),
        );
        vars_to_nodes.insert(
            (variable.name.clone(), Reads::Post),
            data_flow_graph.add_node(NodeType::NonLeaf {
                index,
                var: variable.clone(),
                reads: Reads::Post,
            }),
        );
    }

    for (_, variable) in pred.variables() {
        for (var_name, reads) in variable.expr.collect_path_to_var_exprs(contract, pred) {
            data_flow_graph.add_edge(
                vars_to_nodes[&(var_name.clone(), reads)],
                vars_to_nodes[&(variable.name.clone(), Reads::Pre)],
                (),
            );
            data_flow_graph.add_edge(
                vars_to_nodes[&(var_name.clone(), reads)],
                vars_to_nodes[&(variable.name.clone(), Reads::Post)],
                (),
            );
        }
    }

    for (index, ConstraintDecl { expr, .. }) in pred.constraints.iter().enumerate() {
        let storage_accesses = expr.collect_storage_accesses(contract);

        let (post_accesses, pre_accesses): (Vec<_>, Vec<_>) = storage_accesses
            .into_iter()
            .partition::<Vec<_>, _>(|access| {
                matches!(
                    access.get(contract),
                    Expr::IntrinsicCall {
                        kind: (
                            IntrinsicKind::Internal(
                                InternalIntrinsic::PostState | InternalIntrinsic::PostStateExtern
                            ),
                            _
                        ),
                        ..
                    }
                )
            });

        let constraint_node = data_flow_graph.add_node(NodeType::Leaf {
            index,
            expr: *expr,
            reads: if !pre_accesses.is_empty() && post_accesses.is_empty() {
                Reads::Pre
            } else if pre_accesses.is_empty() && !post_accesses.is_empty() {
                Reads::Post
            } else {
                Reads::Pre
            },
        });

        if !pre_accesses.is_empty() && !post_accesses.is_empty() {
            for access in post_accesses {
                // create temporary nodes
                let temp_node = data_flow_graph.add_node(NodeType::Temp {
                    expr: access,
                    reads: Reads::Post,
                });
                data_flow_graph.add_edge(temp_node, constraint_node, ());
            }
        }

        for (var_name, reads) in expr.collect_path_to_var_exprs(contract, pred) {
            data_flow_graph.add_edge(vars_to_nodes[&(var_name, reads)], constraint_node, ());
        }
    }

    // Remove all non-leaf nodes that have no children. That is, these are dead internal nodes that
    // are not constraints.
    data_flow_graph
        .retain_nodes(|graph, node| graph[node].is_leaf() || graph.edges(node).next().is_some());

    // Detect dependency cycles between nodes.
    // TODO: move this check to semantic analysis and only emit an internal error here if
    // the topological sort later fails.
    let sccs = petgraph::algo::kosaraju_scc(&data_flow_graph);
    for scc in &sccs {
        // Bad components are ones with more than 1 node. These contain cycles.
        if scc.len() > 1 {
            handler.emit_err(Error::Compile {
                error: CompileError::VarsDependencyCycle {
                    spans: scc
                        .iter()
                        .map(|idx| data_flow_graph[*idx].span(contract))
                        .collect(),
                },
            });
        }
    }

    // Topologically sort the graph
    let Ok(mut sorted_nodes) = petgraph::algo::toposort(&data_flow_graph, None) else {
        // TODO: turn into an actual error
        return Err(handler.emit_internal_err(
            "dependency cycle detected between program nodes",
            empty_span(),
        ));
    };

    // Now move all the "sink" nodes (i.e. constraints) to the end. This is important for
    // correctness of the final compute graph
    sorted_nodes = {
        let (mut non_sinks, sinks): (Vec<_>, Vec<_>) =
            sorted_nodes.into_iter().partition(|&node| {
                data_flow_graph
                    .neighbors_directed(node, petgraph::Outgoing)
                    .next()
                    .is_some()
            });
        non_sinks.extend(sinks);
        non_sinks
    };

    let no_span_predicates: FxHashMap<String, (CompiledPredicate, ContentAddress)> =
        compiled_predicates
            .iter()
            .map(
                |(k, (compiled_predicate, content_address, _, _, _, _span))| {
                    (
                        k.clone(),
                        (compiled_predicate.clone(), content_address.clone()),
                    )
                },
            )
            .collect();

    // Final compiled predicate
    let mut compiled_predicate = CompiledPredicate {
        nodes: vec![],
        edges: vec![],
    };

    let mut edge_start = 0;
    let mut programs = BTreeSet::new();

    // c_nodes[i] contains the index of the compute graph node corresponding to constraint i
    let mut c_nodes = vec![0; pred.constraints.len()];

    // v_nodes[i] contains the index of the compute graph node corresponding to variable i
    let mut v_nodes = vec![0; pred.variables().count()];

    for node in &sorted_nodes {
        let mut builder = AsmBuilder::new(&no_span_predicates);
        // Collect all the parents of this node and sort them according to their order in
        // `sorted_nodes`
        let mut parents = data_flow_graph
            .neighbors_directed(*node, petgraph::Direction::Incoming)
            .collect::<Vec<_>>();
        parents.sort_by_key(|node| sorted_nodes.iter().position(|&n| n == *node));

        // Produce a list of the input variables to this node
        let node_inputs = parents
            .iter()
            .map(|node| data_flow_graph[*node].clone())
            .collect::<Vec<_>>();

        let asm = builder.compile_compute_node(
            handler,
            &data_flow_graph[*node],
            &node_inputs,
            contract,
            pred,
        )?;

        let address = essential_hash::content_addr(&Program(
            essential_asm::to_bytes(asm.iter().copied()).collect(),
        ));
        programs.insert(Program(
            essential_asm::to_bytes(asm.iter().copied()).collect(),
        ));

        match &data_flow_graph[*node] {
            NodeType::Temp { reads, .. } => {
                // This is basically a temp variable
                compiled_predicate.nodes.push(Node {
                    program_address: address,
                    edge_start,
                    reads: *reads,
                });
                // v_nodes[*index] = compiled_predicate.nodes.len() - 1;

                // Handle the edges out of this node
                for edge in data_flow_graph.edges(*node) {
                    let to = edge.target();
                    compiled_predicate
                        .edges
                        .push(sorted_nodes.iter().position(|n| *n == to).unwrap() as u16);
                }
                edge_start += data_flow_graph.edges(*node).count() as u16;
            }
            NodeType::Leaf { index, reads, .. } => {
                // This is basically a constraint
                // No edges expected out of this node

                compiled_predicate.nodes.push(Node {
                    program_address: address,
                    edge_start: Edge::MAX,
                    reads: *reads,
                });
                c_nodes[*index] = compiled_predicate.nodes.len() - 1;
            }
            NodeType::NonLeaf { index, reads, .. } => {
                compiled_predicate.nodes.push(Node {
                    program_address: address,
                    edge_start,
                    reads: *reads,
                });
                v_nodes[*index] = compiled_predicate.nodes.len() - 1;

                // Handle the edges out of this node
                for edge in data_flow_graph.edges(*node) {
                    let to = edge.target();
                    compiled_predicate
                        .edges
                        .push(sorted_nodes.iter().position(|n| *n == to).unwrap() as u16);
                }
                edge_start += data_flow_graph.edges(*node).count() as u16;
            }
        }
    }

    if handler.has_errors() {
        return Err(handler.cancel());
    }

    Ok((compiled_predicate, programs, c_nodes, v_nodes))
}
