//! Package graph implementation and dependency resolution.

use crate::{
    manifest::{self, Dependency, ManifestFile},
    source::{self, Source},
};
use petgraph::Directed;
use serde::{Deserialize, Serialize};
use std::{
    collections::{hash_map, BTreeMap, HashMap, HashSet},
    hash::{Hash, Hasher},
    path::Path,
};
use thiserror::Error;

/// The type used to index into the graph.
type GraphIx = u32;
/// The package graph type, where the edge *a* -> *b* means that *a* depends on *b*.
pub type Graph = petgraph::stable_graph::StableGraph<Pinned, Dep, Directed, GraphIx>;
/// The package graph's edge index type.
pub type EdgeIx = petgraph::graph::EdgeIndex<GraphIx>;
/// The package graph's node index type.
pub type NodeIx = petgraph::graph::NodeIndex<GraphIx>;

/// A dependency edge.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Dep {
    /// The kind of dependency represented by the edge.
    pub kind: DepKind,
    /// The dependency name used to refer to the package.
    ///
    /// This is the name on the left hand side of the `=` in a dependency.
    ///
    /// Note that this might differ from the package name in the case that the
    /// dependency specifies a `package` field.
    pub name: String,
}

/// The kind of dependency represented by an edge.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DepKind {
    /// A library dependency, sharing items like types, constants and macros.
    Library,
    /// A contract dependency, providing the content address of the contract.
    Contract,
}

/// An unpinned package.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
pub struct Pkg {
    /// The name declared in the package manifest.
    pub name: String,
    /// The unpinned source of the package.
    pub source: Source,
}

/// A pinned package.
#[derive(Clone, Debug, Eq, Hash, PartialEq, Deserialize, Serialize)]
pub struct Pinned {
    /// The name declared in the package manifest.
    pub name: String,
    /// The pinned source of the package.
    pub source: source::Pinned,
}

/// The ID of a pinned package.
///
/// Determined by hashing the package's name with its [`source::Pinned`].
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq, Deserialize, Serialize)]
pub struct PinnedId(u64);

/// A set of manifests that have been pinned during construction of the compilation plan.
pub type PinnedManifests = HashMap<PinnedId, ManifestFile>;

/// Manifests of the root packages being built.
///
/// This enables building more than one output package simultaneously, improving
/// compilation times for cases where packages may share multiple dependencies.
pub type MemberManifests = BTreeMap<String, ManifestFile>;

/// A mapping from fetched packages to their location within the graph.
type FetchedPkgs = HashMap<Pkg, NodeIx>;

/// A compilation plan generated for one or more package manifests.
struct Plan {
    /// The dependency graph of all packages.
    graph: Graph,
    /// A mapping from each package's pinned ID to its associated manifest.
    manifests: PinnedManifests,
    /// The topological order in which nodes should be built.
    compilation_order: Vec<NodeIx>,
}

#[derive(Debug, Error)]
pub enum PlanError {
    #[error("failed to fetch the package graph: {0}")]
    FetchGraph(#[from] FetchGraphError),
}

#[derive(Debug, Error)]
#[error("failed to fetch graph")]
pub enum FetchGraphError {
    #[error("{0}")]
    MemberName(#[from] MemberNameNotUnique),
    #[error("failed to source dependency package {0:?}: {1}")]
    Source(String, source::SourceError),
    #[error("failed to pin/fetch dependency package {0:?}: {1}")]
    PinAndFetch(String, source::PinAndFetchError),
    #[error("package {0:?}'s dependency named {1:?} failed the depenency manifest check: {2}")]
    DepManifest(String, String, DepManifestError),
}

/// Error indicating that more than one member package was found with the same name.
#[derive(Debug, Error)]
#[error("more than one member package with name {0:?}")]
pub struct MemberNameNotUnique(pub String);

/// Errors produced by dependency manifest validation.
#[derive(Debug, Error)]
pub enum DepManifestError {
    /// Expected a dependency of some kind (i.e. Library, Contract) but found some other kind.
    #[error("declared as a {0:?} dependency, but is actually a {1:?}")]
    UnexpectedKind(DepKind, manifest::PackageKind),
    // A mismatch occurred between the dependency package name and the actual package name.
    #[error(
        "dependency name {0:?} must match the manifest project name {1:?} \
        unless `package = {1:?}` is specified in the dependency declaration"
    )]
    UnexpectedName(String, String),
}

impl Dep {
    fn new(name: String, kind: DepKind) -> Self {
        Self { name, kind }
    }
}

impl Pinned {
    /// A unique ID for the pinned package.
    ///
    /// The internal value is produced by hashing the package's name and `source::Pinned`.
    pub fn id(&self) -> PinnedId {
        PinnedId::new(&self.name, &self.source)
    }

    /// Retrieve the unpinned version of this source.
    pub fn unpinned(&self, path: &Path) -> Pkg {
        let source = self.source.unpinned(path);
        let name = self.name.clone();
        Pkg { name, source }
    }
}

impl PinnedId {
    /// Determine the sha256 hash of the given name and pinned source to produce
    /// a unique pinned package ID.
    pub fn new(name: &str, source: &source::Pinned) -> Self {
        let mut hasher = hash_map::DefaultHasher::default();
        name.hash(&mut hasher);
        source.hash(&mut hasher);
        Self(hasher.finish())
    }
}

/// Construct a compilation plan from the given package manifests that we wish to build.
pub fn plan_from_manifests(member_manifests: &MemberManifests) -> Result<Plan, PlanError> {
    // Fetch the graph and populate the pinned manifests.
    let mut graph = Graph::default();
    let mut pinned_manifests = PinnedManifests::default();
    fetch_graph(member_manifests, &mut graph, &mut pinned_manifests)?;

    // Validate
    // validate_graph(&graph, manifests)?;
    // let compilation_order = compilation_order(&graph)?;
    todo!()
}

/// Complete the given package graph (and associated `PinnedManifests` map) for the `root_manifest`.
fn fetch_graph(
    member_manifests: &MemberManifests,
    graph: &mut Graph,
    pinned_manifests: &mut PinnedManifests,
) -> Result<HashSet<NodeIx>, FetchGraphError> {
    let mut added_nodes = HashSet::default();
    for name in member_manifests.keys() {
        let added = fetch_graph_from_member(member_manifests, name, graph, pinned_manifests)?;
        added_nodes.extend(added);
    }
    // validate_contract_deps(graph)?;
    Ok(added_nodes)
}

/// Complete the given package graph (and associated `PinnedManifests` map) for
/// the member.
///
/// First finds the root node associated with the given `root_manifest` within
/// the graph, adding it if it does not already exist.
///
/// Next, recursively traverses the dependencies, fetching and pinning each
/// package that does not exist within the graph.
///
/// Returns the set of nodes that were newly added.
fn fetch_graph_from_member(
    member_manifests: &MemberManifests,
    member_name: &str,
    graph: &mut Graph,
    pinned_manifests: &mut PinnedManifests,
) -> Result<HashSet<NodeIx>, FetchGraphError> {
    // The manifest associated with the given member name.
    let member_manifest = &member_manifests[member_name];

    // Retrieve the member node, or create one if it does not exist.
    let member_node = find_member_node(graph, &member_manifest.pkg.name)?
        .unwrap_or_else(|| insert_member_pkg(member_manifest.clone(), graph, pinned_manifests));

    // Traverse the rest of the graph from the root.
    let fetch_ts = std::time::Instant::now();
    let fetch_id = source::fetch_graph_id(member_manifest.dir(), fetch_ts);
    let path_root = graph[member_node].id();

    // Track already fetched packages and visited nodes.
    let mut fetched = fetched_pkgs(graph, pinned_manifests);
    let mut visited: HashSet<NodeIx> = HashSet::default();

    fetch_deps(
        member_manifests,
        fetch_id,
        member_node,
        path_root,
        graph,
        pinned_manifests,
        &mut fetched,
        &mut visited,
    )
}

/// Insert a member package into the graph and pinned manifest map.
fn insert_member_pkg(
    member_manifest: ManifestFile,
    graph: &mut Graph,
    pinned_manifests: &mut PinnedManifests,
) -> NodeIx {
    let name = member_manifest.pkg.name.clone();
    let source = source::Pinned::MEMBER;
    let pkg = Pinned { name, source };
    let pkg_id = pkg.id();
    pinned_manifests.insert(pkg_id, member_manifest);
    graph.add_node(pkg)
}

/// Create a mapping from already fetched packages to their location within the graph.
///
/// Assumes `pinned_manifests` contains an entry for every pinned package within
/// the graph and `panic!`s otherwise.
fn fetched_pkgs(graph: &Graph, pinned_manifests: &PinnedManifests) -> FetchedPkgs {
    graph
        .node_indices()
        .map(|n| {
            let pinned = &graph[n];
            let manifest = &pinned_manifests[&pinned.id()];
            let pkg = pinned.unpinned(manifest.dir());
            (pkg, n)
        })
        .collect()
}

/// Given a manifest, produces iterator yielding all named dependencies alongside their kind.
fn manifest_deps(
    manifest: &ManifestFile,
) -> impl '_ + Iterator<Item = (String, Dependency, DepKind)> {
    let deps = manifest
        .deps
        .iter()
        .map(|(name, dep)| (name.clone(), dep.clone(), DepKind::Library));
    let contract_deps = manifest
        .contract_deps
        .iter()
        .map(|(name, dep)| (name.clone(), dep.clone(), DepKind::Contract));
    deps.chain(contract_deps)
}

/// Traverse, fetch and pin unfetched, unpinned dependencies as required to complete the graph.
fn fetch_deps(
    member_manifests: &MemberManifests,
    fetch_id: u64,
    node: NodeIx,
    path_root: PinnedId,
    graph: &mut Graph,
    pinned_manifests: &mut PinnedManifests,
    fetched: &mut FetchedPkgs,
    visited: &mut HashSet<NodeIx>,
) -> Result<HashSet<NodeIx>, FetchGraphError> {
    let mut added = HashSet::default();
    let parent_id = graph[node].id();
    let deps: Vec<_> = manifest_deps(&pinned_manifests[&parent_id]).collect();
    for (dep_name, dep, dep_kind) in deps {
        let pkg_name = dep.package.as_ref().unwrap_or(&dep_name);
        let parent_manifest_dir = pinned_manifests[&parent_id].dir();
        let source =
            Source::from_manifest_dep(parent_manifest_dir, &dep, member_manifests.values())
                .map_err(|e| FetchGraphError::Source(pkg_name.to_string(), e))?;

        // If we haven't yet fetched this dependency, fetch it, pin it and add it to the graph.
        let dep_pkg = Pkg {
            name: pkg_name.to_string(),
            source,
        };
        let dep_node = match fetched.entry(dep_pkg) {
            hash_map::Entry::Occupied(entry) => *entry.get(),
            hash_map::Entry::Vacant(entry) => {
                let pkg = entry.key();
                let ctx = source::PinCtx {
                    fetch_id,
                    path_root,
                    pkg_name: &pkg.name,
                };
                let source = pkg
                    .source
                    .pin_and_fetch(ctx, pinned_manifests)
                    .map_err(|e| FetchGraphError::PinAndFetch(pkg.name.clone(), e))?;
                let name = pkg.name.clone();
                let dep_pinned = Pinned { name, source };
                let dep_node = graph.add_node(dep_pinned);
                added.insert(dep_node);
                *entry.insert(dep_node)
            }
        };

        let dep_edge = Dep::new(dep_name.to_string(), dep_kind.clone());
        // Ensure we have an edge to the dependency.
        graph.update_edge(node, dep_node, dep_edge.clone());

        // If we've visited this node during this traversal already, no need to traverse it again.
        if !visited.insert(dep_node) {
            continue;
        }

        // Check the dependency's manifest.
        let dep_pinned = &graph[dep_node];
        let dep_pkg_id = dep_pinned.id();
        check_dep_manifest(dep_pinned, &pinned_manifests[&dep_pkg_id], &dep_edge)
            .map_err(|e| FetchGraphError::DepManifest(graph[node].name.clone(), dep_name, e))?;

        let path_root = match dep_pinned.source {
            source::Pinned::Member(_) => dep_pkg_id,
            source::Pinned::Path(_) => path_root,
        };

        // Recursively fetch this dependency's dependencies.
        added.extend(fetch_deps(
            member_manifests,
            fetch_id,
            dep_node,
            path_root,
            graph,
            pinned_manifests,
            fetched,
            visited,
        )?);
    }
    Ok(added)
}

/// Find the member node with the given package name.
///
/// Returns an `Err` if more than one member node with the given name exists.
fn find_member_node(g: &Graph, pkg_name: &str) -> Result<Option<NodeIx>, MemberNameNotUnique> {
    let mut matching = member_nodes_with_name(g, pkg_name);
    let node_opt = matching.next();
    match matching.next() {
        None => Ok(node_opt),
        Some(_) => Err(MemberNameNotUnique(pkg_name.to_string())),
    }
}

/// All member nodes within the graph.
fn member_nodes(g: &Graph) -> impl '_ + Iterator<Item = NodeIx> {
    g.node_indices()
        .filter(|&n| g[n].source == source::Pinned::MEMBER)
}

/// All member nodes with the given name.
fn member_nodes_with_name<'a>(
    g: &'a Graph,
    pkg_name: &'a str,
) -> impl 'a + Iterator<Item = NodeIx> {
    member_nodes(g).filter(move |&n| g[n].name == pkg_name)
}

/// Validate the manifest of a depenency.
fn check_dep_manifest(
    dep: &Pinned,
    dep_manifest: &ManifestFile,
    dep_edge: &Dep,
) -> Result<(), DepManifestError> {
    // Ensure the dependency kind matches the expected kind.
    match (&dep_edge.kind, &dep_manifest.pkg.kind) {
        (DepKind::Contract, manifest::PackageKind::Contract)
        | (DepKind::Library, manifest::PackageKind::Library) => (),
        _ => {
            let pkg_kind = dep_manifest.pkg.kind.clone();
            return Err(DepManifestError::UnexpectedKind(
                dep_edge.kind.clone(),
                pkg_kind,
            ));
        }
    }

    // Ensure the name matches the manifest project name.
    if dep.name != dep_manifest.pkg.name {
        let pkg_name = dep_manifest.pkg.name.clone();
        return Err(DepManifestError::UnexpectedName(dep.name.clone(), pkg_name));
    }

    Ok(())
}
