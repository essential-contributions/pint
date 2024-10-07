//! Items to assist traversal of Pint ABI Keyed Vars.
//!
//! This assists in traversal of the `storage` and `pub_vars` sections of the Pint ABI.

use essential_types::Word;
use petgraph::visit::EdgeRef;
use pint_abi_types::{TupleField, TypeABI, VarABI};

/// The [`TypeABI`] rose tree represented as a graph.
///
/// By flattening the [`VarABI`]s into an indexable tree type, we gain
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
        /// The flattened index of the field within the directly enclosing tuple.
        ///
        /// If this is the first field in the tuple, it will always be `0`.
        ///
        /// For all other fields, this will be the sum of the
        /// `flattened_key_count` of all prior fields.
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
        /// The size of a single element's key in words.
        ///
        /// E.g. for `{ int, int }[10]`, the array's `elem_len` is `2`.
        elem_len: usize,
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
            // The first word is always the Var index.
            Nesting::Var { ix } => {
                let word = Word::try_from(*ix).expect("out of Word range");
                opts.push(Some(word));
            }
            // Map entries are keyed by a dynamically provided map key.
            Nesting::MapEntry { key_size } => {
                opts.resize(opts.len() + key_size, None);
            }
            // Tuples are flattened into arrays and other tuples.
            // If there's an array within this nesting, we can't know the index.
            Nesting::TupleField { ix: _, flat_ix } => {
                opts.push(flattened_opt_word(Some(*flat_ix), &mut iter));
            }
            // Arrays are flattened into other arrays and tuples.
            Nesting::ArrayElem { array_len: _, .. } => {
                opts.push(flattened_opt_word(None, &mut iter));
            }
        }
    }
    opts
}

/// Flattens directly nested tuple and array nestings into a single word.
///
/// In the case that an array appears in the nesting, `None` is returned as the
/// precise word cannot be known until an array index is provided dynamically.
///
/// The provided iterator's `next` method will only be called for each
/// directly nested array and tuple.
fn flattened_opt_word<'a>(
    mut tuple_flattened_ix: Option<usize>,
    iter: &mut std::iter::Peekable<impl Iterator<Item = &'a Nesting>>,
) -> Option<Word> {
    while let Some(nesting) = iter.peek() {
        match nesting {
            Nesting::ArrayElem { .. } => tuple_flattened_ix = None,
            Nesting::TupleField { flat_ix, .. } => {
                if let Some(ref mut ix) = tuple_flattened_ix {
                    *ix += *flat_ix;
                }
            }
            _ => break,
        }
        iter.next();
    }
    tuple_flattened_ix.map(|ix| Word::try_from(ix).expect("out of `Word` range"))
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
        TypeABI::Union { variants, .. } => {
            // 1 word for the tag + max variant type size
            1 + variants
                .iter()
                .filter_map(|variant| variant.ty.as_ref())
                .map(ty_size)
                .max()
                .unwrap_or_default()
        }
        TypeABI::Map { .. } => 1,
    }
}

/// Add all child nodes of the given node `a` with type `ty`, by recursively
/// traversing the given `TypeABI`.
fn add_children<'a>(graph: &mut KeyedVarGraph<'a>, a: NodeIx, ty: &'a TypeABI) {
    match ty {
        // Leaf types have no further nesting.
        TypeABI::Bool
        | TypeABI::Int
        | TypeABI::Real
        | TypeABI::String
        | TypeABI::B256
        | TypeABI::Union { .. } => {}

        // Recurse for nested tuple types.
        TypeABI::Tuple(fields) => {
            for (ix, field) in fields.iter().enumerate() {
                let flat_ix = flattened_tuple_key_count(&fields[0..ix]);
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
            let elem_len = flattened_key_count(ty);
            let nesting = Nesting::ArrayElem {
                array_len,
                elem_len,
            };
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

/// Determine the total number of keys within the directly nested flattened types.
///
/// Types that are flattened together include arrays and tuples.
fn flattened_key_count(ty: &TypeABI) -> usize {
    match ty {
        TypeABI::Tuple(fields) => flattened_tuple_key_count(fields),
        TypeABI::Array { ty, size } => {
            let size = usize::try_from(*size).expect("size out of usize range");
            size * flattened_key_count(ty)
        }
        // The keys for the following are not flattened any further.
        TypeABI::Bool
        | TypeABI::Real
        | TypeABI::Int
        | TypeABI::String
        | TypeABI::B256
        | TypeABI::Union { .. }
        | TypeABI::Map { .. } => 1,
    }
}

// Given a slice of tuple fields, determine the total number of keys within the
// directly nested flattened types.
fn flattened_tuple_key_count(fields: &[TupleField]) -> usize {
    fields
        .iter()
        .map(|field| flattened_key_count(&field.ty))
        .sum()
}
