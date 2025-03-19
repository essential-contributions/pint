use crate::{
    error::{CompileError, Error, ErrorEmitted, Handler},
    expr::{Expr, ExternalIntrinsic, Immediate, IntrinsicKind},
    predicate::{ConstraintDecl, Contract, ExprKey, Predicate, Variable},
    span::{empty_span, Span, Spanned},
};

use asm_builder::AsmBuilder;
use essential_types::{
    predicate::{Edge, Node, Predicate as CompiledPredicate, Program, Reads},
    ContentAddress,
};
use fxhash::{FxHashMap, FxHashSet};
use petgraph::{graph::NodeIndex, visit::EdgeRef, Graph};
use std::collections::BTreeSet;

mod asm_builder;
mod display;
#[cfg(test)]
mod tests;

#[derive(Debug, Default, Clone)]
pub struct CompiledContract {
    pub names: Vec<String>,
    pub contract: essential_types::contract::Contract,
    pub programs: BTreeSet<Program>,
}

type CompiledPredicates =
    FxHashMap<String, (CompiledPredicate, ContentAddress, BTreeSet<Program>, Span)>;

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

        if let Ok((compiled_predicate, programs)) = handler
            .scope(|handler| compile_predicate(handler, contract, &compiled_predicates, predicate))
        {
            let compiled_predicate_address = essential_hash::content_addr(&compiled_predicate);
            compiled_predicates.insert(
                predicate.name.name.clone(),
                (
                    compiled_predicate,
                    compiled_predicate_address,
                    programs,
                    predicate.span.clone(),
                ),
            );
        } else {
            // This predicate failed to compile. Just exit for now.
            return Err(handler.cancel());
        }
    }

    // Now check for duplicate predicates
    let mut unique_addresses: FxHashSet<&ContentAddress> = FxHashSet::default();
    let mut original_predicates: FxHashMap<&ContentAddress, (&String, &Span)> =
        FxHashMap::default();

    let mut compiled_predicates_vec: Vec<_> = compiled_predicates.iter().collect();
    compiled_predicates_vec.reverse(); // Guarantees the error message refers to the first declaration of the predicate

    for (string, (_, content_address, _, span)) in &compiled_predicates_vec {
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
    let (names, predicates) = contract
        .preds
        .iter()
        .map(|(_, pred)| {
            compiled_predicates
                .remove(&pred.name.name)
                .map(|(compiled_predicate, _, programs, _)| {
                    combined_programs.extend(programs);
                    (pred.name.name.clone(), compiled_predicate)
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
            names,
            contract: essential_types::contract::Contract { predicates, salt },
            programs: combined_programs,
        })
    }
}

/// This a node in the compute graph. Keeps track of which item it represents. There are three
/// types of nodes:
/// 1. `Var` nodes represent local variables. They are internal nodes (non-leaves).
/// 2. `Constraint` nodes represent constraints. They are leaves.
/// 3. `Expr` nodes represent specific expressions that asm gen decided to use a separate node for.
///
#[derive(Clone, Debug)]
enum ComputeNode {
    Var { var: Variable, reads: Reads },
    Constraint { expr: ExprKey, reads: Reads },
    Expr { expr: ExprKey, reads: Reads },
}

impl ComputeNode {
    fn is_leaf(&self) -> bool {
        matches!(self, ComputeNode::Constraint { .. })
    }

    fn expr(&self) -> ExprKey {
        match self {
            Self::Var { var, .. } => var.expr,
            Self::Constraint { expr, .. } | Self::Expr { expr, .. } => *expr,
        }
    }

    fn span(&self, contract: &Contract) -> Span {
        match self {
            Self::Var { var, .. } => var.span.clone(),
            Self::Constraint { expr, .. } | Self::Expr { expr, .. } => {
                expr.get(contract).span().clone()
            }
        }
    }
}

/// Converts a `crate::Predicate` into a `CompiledPredicate` which
/// includes generating assembly for the constraints and for variable reads.
pub fn compile_predicate(
    handler: &Handler,
    contract: &Contract,
    compiled_predicates: &CompiledPredicates,
    pred: &Predicate,
) -> Result<(CompiledPredicate, BTreeSet<Program>), ErrorEmitted> {
    let mut data_flow_graph = Graph::<ComputeNode, ()>::new();
    let mut vars_to_nodes = FxHashMap::<(String, Reads), NodeIndex>::default();
    let mut asm_args_to_nodes = FxHashMap::<ExprKey, NodeIndex>::default();

    // Insert non leaf nodes into the compute graph
    for (_, variable) in pred.variables() {
        let var_node_pre = data_flow_graph.add_node(ComputeNode::Var {
            var: variable.clone(),
            reads: Reads::Pre,
        });
        vars_to_nodes.insert((variable.name.clone(), Reads::Pre), var_node_pre);

        // Variables that are initialized with an asm block depend on the arguments of the asm
        // block. Pull those arguments into their own and add the appropriate dependencies.
        if let Expr::AsmBlock { args, .. } = variable.expr.get(contract) {
            let arg_nodes: Vec<_> = args
                .iter()
                .map(|arg| {
                    // This is the node for the argument
                    let arg_node = data_flow_graph.add_node(ComputeNode::Expr {
                        expr: *arg,
                        reads: Reads::Pre,
                    });

                    asm_args_to_nodes.insert(*arg, arg_node);

                    // Finally, the var nodes themselves depend on the arg node
                    data_flow_graph.add_edge(arg_node, var_node_pre, ());

                    arg_node
                })
                .collect();

            // This is a bit cheeky. Basically we're trying to force a _fake_ dependence between
            // the args in the order in which they appear in the arg list, so that they end up in
            // the same order in the precomputed memory they will populate for this variable.
            //
            // Maybe one day we'll allow direct access to the arguments inside the asm block.
            arg_nodes.windows(2).for_each(|window| {
                data_flow_graph.add_edge(window[0], window[1], ());
            });
        }
    }

    // Insert edges between non leaf nodes
    for (_, variable) in pred.variables() {
        if let Expr::AsmBlock { args, .. } = variable.expr.get(contract) {
            // For asm blocks, insert edges from the vars we depend on to the asm args
            for arg in args {
                for (var_name, reads) in arg.collect_path_to_var_exprs(contract, pred) {
                    data_flow_graph.add_edge(
                        vars_to_nodes[&(var_name, reads)],
                        asm_args_to_nodes[arg],
                        (),
                    );
                }
            }
        } else {
            // Otherwise, insert edges from the vars we depend on to this var directly
            for (var_name, reads) in variable.expr.collect_path_to_var_exprs(contract, pred) {
                data_flow_graph.add_edge(
                    vars_to_nodes[&(var_name.clone(), reads)],
                    vars_to_nodes[&(variable.name.clone(), Reads::Pre)],
                    (),
                );
            }
        }
    }

    // Insert leaf nodes and edges to them
    for ConstraintDecl { expr, .. } in pred.constraints.iter() {
        let constraint_node = data_flow_graph.add_node(ComputeNode::Constraint {
            expr: *expr,
            reads: Reads::Pre,
        });

        for (var_name, reads) in expr.collect_path_to_var_exprs(contract, pred) {
            data_flow_graph.add_edge(vars_to_nodes[&(var_name, reads)], constraint_node, ());
        }
    }

    // Remove all non-leaf nodes that have no children. That is, these are dead internal nodes that
    // **are not constraints**. These can be ignored and should not be compiled.
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
        return Err(handler.emit_internal_err(
            "dependency cycle detected between program nodes. should have been caught above",
            empty_span(),
        ));
    };

    // Now move all the "sink" nodes (i.e. constraints) to the end. This is important for
    // correctness of the final compute graph in the `essential_types::predicate::Predicate` object
    // where its assumed that all the leaf nodes are at the end
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
            .map(|(k, (compiled_predicate, content_address, _, _span))| {
                (
                    k.clone(),
                    (compiled_predicate.clone(), content_address.clone()),
                )
            })
            .collect();

    // Final compiled predicate which we will slowly build
    let mut compiled_predicate = CompiledPredicate {
        nodes: vec![],
        edges: vec![],
    };

    let mut edge_start = 0;
    let mut programs = BTreeSet::new();

    for node in &sorted_nodes {
        let mut builder = AsmBuilder::new(&no_span_predicates);
        // Collect all the parents of this node and sort them according to their order in
        // `sorted_nodes`
        let mut parents = data_flow_graph
            .neighbors_directed(*node, petgraph::Direction::Incoming)
            .collect::<Vec<_>>();
        parents.sort_by_key(|node| sorted_nodes.iter().position(|&n| n == *node));

        // Produce a list of the inputs to this node from its parents
        let parents = parents
            .iter()
            .map(|node| data_flow_graph[*node].clone())
            .collect::<Vec<_>>();

        // Compile each node separately while keeping in mind which `parents` it has.
        let asm = builder.compile_compute_node(
            handler,
            &data_flow_graph[*node],
            &parents,
            contract,
            pred,
        )?;

        let address = essential_hash::content_addr(&Program(
            essential_asm::to_bytes(asm.iter().copied()).collect(),
        ));

        programs.insert(Program(
            essential_asm::to_bytes(asm.iter().copied()).collect(),
        ));

        // Now push a node into the final `compiled_predicate`
        match &data_flow_graph[*node] {
            ComputeNode::Constraint { reads, .. } => {
                compiled_predicate.nodes.push(Node {
                    program_address: address,
                    edge_start: Edge::MAX,
                    // Constraints don't access state directly yet, so default to `Reads::Pre` here
                    // for now
                    reads: *reads,
                });
            }
            ComputeNode::Var { reads, .. } | ComputeNode::Expr { reads, .. } => {
                compiled_predicate.nodes.push(Node {
                    program_address: address,
                    edge_start,
                    reads: *reads,
                });

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

    Ok((compiled_predicate, programs))
}
