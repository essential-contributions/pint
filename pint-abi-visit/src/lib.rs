//! Items to assist traversal of Pint ABI Keyed Vars.
//!
//! This assists in traversal of the `storage` and `pub_vars` sections of the Pint ABI.

use essential_types::Word;
use petgraph::visit::EdgeRef;
use pint_abi_types::{TupleField, TypeABI, VarABI};

/// The [`TypeABI`] rose tree represented as a graph.
///
/// By flattening the [`KeyedTypeVar`]s into an indexable tree type, we gain
/// more flexibility around inspecting both parent and child nodes during
/// traversal.
///
/// This is particularly useful for generating both types and implementations
/// (that may refer to child nodes) when visiting a single node.
pub struct KeyedVarTree<'a> {
    graph: KeyedVarGraph<'a>,
    /// The root keyed var variables.
    roots: Vec<NodeIx>,
}

/// The graph structure used to represent the [`KeyedVarTree`] internally.
///
/// The edge `a -> b` implies that `b` is nested within `a`.
pub type KeyedVarGraph<'a> = petgraph::Graph<Keyed<'a>, (), petgraph::Directed, TreeIx>;

/// Index size used for the graph.
type TreeIx = usize;
/// An index into a `KeyedVarGraph` edge.
pub type EdgeIx = petgraph::graph::EdgeIndex<TreeIx>;
/// An index into a `KeyedVarGraph` node.
pub type NodeIx = petgraph::graph::NodeIndex<TreeIx>;

/// Represents a visited keyed var along with the context in which its nested.
pub struct Keyed<'a> {
    /// The name of the var if it is named.
    pub name: Option<&'a str>,
    /// The type of the var.
    pub ty: &'a TypeABI,
    /// Describes how the keyed var is nested within `storage` or `pub vars`.
    ///
    /// The first element is always a `Var` representing the keyed var's
    /// top-level index within `storage` or pub vars.
    pub nesting: Nesting,
}

/// Describes how a [`TypeABI`] is nested within the tree.
#[derive(Clone, Debug)]
pub enum Nesting {
    /// A top-level storage field of pub var.
    ///
    /// This is always the first element in a `Keyed`'s `nesting: [Nesting]` field.
    Var {
        /// Represents the index of the storage field or pub var.
        ix: usize,
    },
    /// The `Keyed` var is within a tuple field.
    TupleField {
        /// The field index of the var within the tuple (not flattened).
        ix: usize,
        /// The flattened index of the var within a tree of directly nested tuples.
        ///
        /// This is required to match the way that pintc flattens tuples, which
        /// influences how their keys are constructed.
        flat_ix: usize,
    },
    /// An entry within a map.
    MapEntry {
        /// The number of words used to represent the key.
        key_size: usize,
    },
    /// An element within an array.
    ArrayElem {
        /// The total length of the array.
        array_len: usize,
    },
}

impl<'a> KeyedVarTree<'a> {
    /// Construct a new tree from the given list of `VarABI`s from a
    /// `storage` or `pub_vars`instance.
    pub fn from_keyed_vars(vars: &'a [VarABI]) -> Self {
        // Construct the graph.
        let mut graph = KeyedVarGraph::default();

        // Add each root and its children.
        let mut roots = vec![];
        for (ix, var) in vars.iter().enumerate() {
            let ty = &var.ty;
            let name = Some(var.name.as_str());
            let nesting = Nesting::Var { ix };
            let node = Keyed { ty, name, nesting };
            let n = graph.add_node(node);
            add_children(&mut graph, n, &var.ty);
            roots.push(n);
        }

        KeyedVarTree { graph, roots }
    }

    /// Visit all keyed types within the tree in depth-first order.
    pub fn dfs(&self, mut visit: impl FnMut(NodeIx)) {
        for &root in &self.roots {
            let mut dfs = petgraph::visit::Dfs::new(&self.graph, root);
            while let Some(n) = dfs.next(&self.graph) {
                visit(n);
            }
        }
    }

    /// Return the node index of the parent node within the tree.
    pub fn parent(&self, n: NodeIx) -> Option<NodeIx> {
        parent(&self.graph, n)
    }

    /// An iterator yielding the immediate children of this node.
    pub fn children(&self, n: NodeIx) -> Vec<NodeIx> {
        let mut children: Vec<_> = self
            .graph
            .edges_directed(n, petgraph::Direction::Outgoing)
            .map(|e| e.target())
            .collect();
        // The graph edges iterator yields edges in the reverse order of which they were added.
        // Reverse the order here to ensure children are yielded in the more intuitive order.
        children.reverse();
        children
    }

    /// Returns a description of how the node at `n` is nested from the root.
    pub fn nesting(&self, n: NodeIx) -> Vec<Nesting> {
        nesting(&self.graph, n)
    }

    /// The root nodes, representing the top-level storage or pub vars.
    pub fn roots(&self) -> &[NodeIx] {
        &self.roots
    }
}

impl<'a> core::ops::Deref for KeyedVarTree<'a> {
    type Target = KeyedVarGraph<'a>;
    fn deref(&self) -> &Self::Target {
        &self.graph
    }
}

/// Given a type nesting, returns a partial key to the nested value.
///
/// Words that are provided dynamically (via map key or array index) are
/// represented with `None`.
pub fn partial_key_from_nesting(nesting: &[Nesting]) -> Vec<Option<Word>> {
    let mut opts = vec![];
    let mut iter = nesting.iter().peekable();
    while let Some(nesting) = iter.next() {
        match nesting {
            Nesting::Var { ix } => {
                let word = Word::try_from(*ix).expect("out of Word range");
                opts.push(Some(word));
            }
            Nesting::TupleField { ix: _, flat_ix } => {
                let mut deepest_flat_ix = *flat_ix;
                while let Some(Nesting::TupleField { ix: _, flat_ix }) = iter.peek() {
                    deepest_flat_ix = *flat_ix;
                    iter.next();
                }
                let word = Word::try_from(deepest_flat_ix).expect("out of Word range");
                opts.push(Some(word));
            }
            Nesting::MapEntry { key_size } => {
                opts.resize(opts.len() + key_size, None);
            }
            Nesting::ArrayElem { array_len: _ } => {
                while let Some(Nesting::ArrayElem { array_len: _ }) = iter.peek() {
                    iter.next();
                }
                opts.push(None);
            }
        }
    }
    opts
}

/// The number of words used to represent an ABI type in key form.
pub fn ty_size(ty: &TypeABI) -> usize {
    match ty {
        TypeABI::Bool | TypeABI::Int | TypeABI::Real => 1,
        TypeABI::B256 => 4,
        TypeABI::String => panic!("unknown size of type `string`"),
        TypeABI::Tuple(fields) => fields.iter().map(|f| ty_size(&f.ty)).sum(),
        TypeABI::Array { ty, size } => {
            ty_size(ty) * usize::try_from(*size).expect("size out of range")
        }
        TypeABI::Map { .. } => 1,
    }
}

/// Add all child nodes of the given node `a` with type `ty`, by recursively
/// traversing the given `TypeABI`.
fn add_children<'a>(graph: &mut KeyedVarGraph<'a>, a: NodeIx, ty: &'a TypeABI) {
    match ty {
        TypeABI::Bool | TypeABI::Int | TypeABI::Real | TypeABI::String | TypeABI::B256 => {}

        // Recurse for nested tuple types.
        TypeABI::Tuple(fields) => {
            for (ix, field) in fields.iter().enumerate() {
                let flat_ix = {
                    let start_flat_ix = match &graph[a].nesting {
                        Nesting::TupleField { flat_ix, .. } => *flat_ix,
                        _ => 0,
                    };
                    flattened_ix(fields, ix, start_flat_ix)
                };
                let name = field.name.as_deref();
                let ty = &field.ty;
                let nesting = Nesting::TupleField { ix, flat_ix };
                let node = Keyed { ty, name, nesting };
                let b = graph.add_node(node);
                graph.add_edge(a, b, ());
                add_children(graph, b, &field.ty);
            }
        }

        // Recurse for nested array element types.
        TypeABI::Array { ty, size } => {
            let array_len = usize::try_from(*size).expect("size out of range");
            let nesting = Nesting::ArrayElem { array_len };
            let name = None;
            let node = Keyed { ty, name, nesting };
            let b = graph.add_node(node);
            graph.add_edge(a, b, ());
            add_children(graph, b, ty);
        }

        // Recurse for nested map element types.
        TypeABI::Map { ty_from, ty_to } => {
            let key_size = ty_size(ty_from);
            let nesting = Nesting::MapEntry { key_size };
            let name = None;
            let ty = ty_to;
            let node = Keyed { ty, name, nesting };
            let b = graph.add_node(node);
            graph.add_edge(a, b, ());
            add_children(graph, b, ty_to);
        }
    }
}

/// Return the node index of the parent node within the graph.
fn parent(graph: &KeyedVarGraph, n: NodeIx) -> Option<NodeIx> {
    graph
        .edges_directed(n, petgraph::Direction::Incoming)
        .next()
        .map(|e| e.source())
}

/// Return the full nesting of the node at the given index.
fn nesting(graph: &KeyedVarGraph, mut n: NodeIx) -> Vec<Nesting> {
    // First, collect the path to the root.
    let mut path = vec![n];
    while let Some(p) = parent(graph, n) {
        path.push(p);
        n = p;
    }
    // Collect the nestings starting from the root.
    let mut nestings = vec![];
    while let Some(n) = path.pop() {
        nestings.push(graph[n].nesting.clone());
    }
    nestings
}

/// Determine the flattened index of the tuple field at the given field index
/// within the given `fields`.
///
/// The `start_flat_ix` represents the flat index of the enclosing tuple.
fn flattened_ix(fields: &[TupleField], field_ix: usize, start_flat_ix: usize) -> usize {
    // Given a slice of tuple fields, determine the total number of leaf fields
    // within the directly nested tree of tuples.
    fn count_flattened_leaf_fields(fields: &[TupleField]) -> usize {
        fields
            .iter()
            .map(|field| match field.ty {
                TypeABI::Tuple(ref fields) => count_flattened_leaf_fields(fields),
                _ => 1,
            })
            .sum()
    }
    start_flat_ix + count_flattened_leaf_fields(&fields[0..field_ix])
}
