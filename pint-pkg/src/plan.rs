//! Items related to construction of the compilation [`Plan`].

use crate::{
    manifest::{self, Dependency, ManifestFile},
    source::{self, Source},
};
use manifest::ManifestFileError;
use petgraph::{visit::EdgeRef, Directed, Direction};
use serde::{Deserialize, Serialize};
use std::{
    collections::{hash_map, BTreeMap, HashMap, HashSet},
    fmt,
    hash::{Hash, Hasher},
    path::{Path, PathBuf},
    str,
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
/// Shorthand for the graph's edge reference type.
type EdgeReference<'a> = petgraph::stable_graph::EdgeReference<'a, Dep, GraphIx>;

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

/// The set of invalid deps discovered during a `check_graph` traversal.
pub type InvalidDeps = BTreeMap<EdgeIx, InvalidDepCause>;

/// A mapping from fetched packages to their location within the graph.
type FetchedPkgs = HashMap<Pkg, NodeIx>;

/// A compilation plan generated for one or more member package manifests.
#[derive(Debug)]
pub struct Plan {
    /// The dependency graph of all packages.
    graph: Graph,
    /// A mapping from each package's pinned ID to its associated manifest.
    manifests: PinnedManifests,
    /// The topological order in which nodes should be built.
    compilation_order: Vec<NodeIx>,
}

/// Failed to construct a compilation plan.
#[derive(Debug, Error)]
pub enum PlanError {
    /// Failed to fetch the package graph.
    #[error("failed to fetch the package graph: {0}")]
    FetchGraph(#[from] FetchGraphError),
    /// A cycle was detected in the package graph.
    #[error("{0}")]
    DependencyCycle(#[from] DependencyCycle),
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

/// Failed to parse a pinned ID from its hex string representation.
#[derive(Debug, Error)]
#[error("failed to parse `PinnedId`: expected 16-char hex string, found {0:?}")]
pub struct PinnedIdFromStrError(String);

/// The reason the dependency was invalidated.
#[derive(Debug, Error)]
pub enum InvalidDepCause {
    /// No members found in the graph for the given set.
    #[error("graph contains none of the given members")]
    NoMembersFound,
    #[error("{0}")]
    Check(#[from] CheckDepError),
}

/// Represents an invalid dependency.
#[derive(Debug, Error)]
pub enum CheckDepError {
    /// Failed to resolve the dependency's path.
    #[error("{0}")]
    PathError(#[from] DepPathError),
    /// Failed to load a dependency manifest.
    #[error("{0}")]
    DepManifestFileError(#[from] ManifestFileError),
    /// The dependency has no entry in the parent manifest.
    #[error("no entry in parent manifest")]
    NoEntryInManifest,
    /// Failed to construct the source.
    #[error("{0}")]
    Source(#[from] source::SourceError),
    /// A source mismatch
    #[error("the graph source {0:?} does not match the manifest entry {1:?}")]
    SourceMismatch(Source, Source),
    /// The dependency's manifest failed the dependency manifest check.
    #[error("{0}")]
    DepManifestError(#[from] DepManifestError),
}

/// Failed to find a path root for a given path node.
///
/// All path nodes must have some root in order to resolve their potentially relative path.
#[derive(Debug, Error)]
#[error("failed to find path root: `path` dependency {0:?} has no parent")]
pub struct FindPathRootError(String);

/// An error produced by a failed path root check.
#[derive(Debug, Error)]
pub enum CheckPathRootError {
    #[error("{0}")]
    NotFound(#[from] FindPathRootError),
    #[error("invalid path root for dependency {0:?}: expected {1}, found {2}")]
    Invalid(String, PinnedId, PinnedId),
}

/// Failed to resolve a dependency's path.
#[derive(Debug, Error)]
pub enum DepPathError {
    /// Some source-specific error occurred.
    #[error("{0}")]
    Source(#[from] source::DepPathError),
    /// The path root was not found or was invalid.
    #[error("{0}")]
    CheckPathRoot(#[from] CheckPathRootError),
    /// The dependency has no entry in the parent manifest.
    #[error("dependency named {0:?} does not appear in manifest of {1:?}")]
    NoEntryInManifest(String, String),
    /// A dependency declared as a member was not found in the member manifests.
    #[error("supposed member dependency {0:?} not found in member manifests")]
    MemberNotFound(String),
    /// The dependency was specified as a path that does not exist.
    #[error("dependency named {0:?} specified a path {0:?} that does not exist")]
    PathDoesNotExist(String, PathBuf),
}

/// A cycle was detected while attempting to determine compilation order.
#[derive(Debug, Error)]
#[error("cycle detected between the following packages: {0:?}")]
pub struct DependencyCycle(pub Vec<String>);

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

impl Plan {
    /// The full package dependency graph.
    pub fn graph(&self) -> &Graph {
        &self.graph
    }

    /// The manifest (both in-memory and local file) for every package in the graph.
    pub fn manifests(&self) -> &PinnedManifests {
        &self.manifests
    }

    /// An order in which packages may be compiled so that all dependencies are
    /// built before all dependents.
    pub fn compilation_order(&self) -> &[NodeIx] {
        &self.compilation_order
    }
}

impl fmt::Display for PinnedId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:016x}", self.0)
    }
}

impl str::FromStr for PinnedId {
    type Err = PinnedIdFromStrError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let res = u64::from_str_radix(s, 16);
        Ok(Self(res.map_err(|_| PinnedIdFromStrError(s.to_string()))?))
    }
}

/// Construct a compilation plan from the given member manifests that we wish to build.
///
/// Fetches and pins all packages as a part of constructing the full compilation plan.
pub fn from_members(members: &MemberManifests) -> Result<Plan, PlanError> {
    // Fetch the graph and populate the pinned manifests.
    let mut graph = Graph::default();
    let mut pinned_manifests = PinnedManifests::default();
    fetch_graph(members, &mut graph, &mut pinned_manifests)?;

    // TODO: Remove this block, just a sanity check.
    {
        let (pinned_manifests2, invalid_deps) = check_graph(&graph, members);
        assert!(invalid_deps.is_empty(), "{invalid_deps:?}");
        assert_eq!(pinned_manifests, pinned_manifests2);
    }

    // Determine the order in which packages should be compiled.
    // TODO: Remove this when enabling concurrent builds.
    let compilation_order = compilation_order(&graph)?;

    Ok(Plan {
        graph,
        manifests: pinned_manifests,
        compilation_order,
    })
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
                    _fetch_id: fetch_id,
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

/// Validate the graph against the given member manifests.
///
/// Returns a map of `PinnedManifests` for all valid nodes within the graph,
/// alongside of all invalid dependencies as a set of edges.
fn check_graph(graph: &Graph, members: &MemberManifests) -> (PinnedManifests, InvalidDeps) {
    // Add all existing member manifests to the pinned manifests map.
    let mut pinned_manifests = PinnedManifests::default();
    let mut invalid_deps = InvalidDeps::default();
    for n in member_nodes(graph) {
        let pinned = &graph[n];
        let name = &pinned.name;
        let Some(manifest) = members.get(name) else {
            continue;
        };
        pinned_manifests.insert(pinned.id(), manifest.clone());
        invalid_deps.extend(check_deps(graph, members, n, &mut pinned_manifests));
    }

    // If no member nodes, we need to reconstruct the whole graph.
    if pinned_manifests.is_empty() {
        invalid_deps.extend(
            graph
                .edge_indices()
                .map(|e| (e, InvalidDepCause::NoMembersFound)),
        );
    }

    (pinned_manifests, invalid_deps)
}

/// Recursively validate all dependencies starting from the given node.
///
/// `pinned_manifests` must at least already contain the manifest of the given
/// `node` whose dependencies are being checked.
fn check_deps(
    graph: &Graph,
    members: &MemberManifests,
    node: NodeIx,
    pinned_manifests: &mut PinnedManifests,
) -> InvalidDeps {
    let mut remove = InvalidDeps::default();
    for edge in graph.edges_directed(node, Direction::Outgoing) {
        let dep_manifest = match check_dep(graph, members, edge, pinned_manifests) {
            Ok(manifest) => manifest,
            Err(err) => {
                remove.insert(edge.id(), InvalidDepCause::Check(err));
                continue;
            }
        };
        let dep_node = edge.target();
        let dep_id = graph[dep_node].id();
        if pinned_manifests.insert(dep_id, dep_manifest).is_none() {
            let rm = check_deps(graph, members, dep_node, pinned_manifests);
            remove.extend(rm);
        }
    }
    remove
}

/// Validate a dependency within the given graph.
///
/// Assumes that `PinnedManifests` contains the manifest for the `edge.source()`
/// node and panics otherwise.
fn check_dep(
    graph: &Graph,
    members: &MemberManifests,
    edge: EdgeReference,
    pinned_manifests: &PinnedManifests,
) -> Result<ManifestFile, CheckDepError> {
    let node = edge.source();
    let dep_node = edge.target();
    let dep = edge.weight();
    let node_manifest = &pinned_manifests[&graph[node].id()];
    let dep_path = dep_path(graph, members, node_manifest, dep_node)?;
    let dep_manifest_path = dep_path.join(ManifestFile::FILE_NAME);
    let dep_manifest = ManifestFile::from_path(&dep_manifest_path)?;
    let dep_entry = node_manifest
        .dep(&dep.name)
        .ok_or(CheckDepError::NoEntryInManifest)?;
    let dep_source = Source::from_manifest_dep(node_manifest.dir(), dep_entry, members.values())?;
    let dep_pkg = graph[dep_node].unpinned(&dep_path);
    if dep_pkg.source != dep_source {
        return Err(CheckDepError::SourceMismatch(dep_pkg.source, dep_source));
    }
    check_dep_manifest(&graph[dep_node], &dep_manifest, dep)?;
    Ok(dep_manifest)
}

/// Given a manifest and a node associated with one of its dependencies, returns
/// the canonical local path to the directory containing the dependency's
/// manifest.
fn dep_path(
    graph: &Graph,
    members: &MemberManifests,
    node_manifest: &ManifestFile,
    dep_node: NodeIx,
) -> Result<PathBuf, DepPathError> {
    let dep = &graph[dep_node];
    let dep_name = &dep.name;
    match dep.source.dep_path(&dep.name)? {
        source::DependencyPath::ManifestPath(path) => Ok(path),
        source::DependencyPath::Member => members
            .values()
            .find(|m| m.pkg.name == *dep_name)
            .map(|m| m.dir().to_path_buf())
            .ok_or_else(|| DepPathError::MemberNotFound(dep_name.to_string())),
        source::DependencyPath::Root(path_root) => {
            check_path_root(graph, dep_node, path_root)?;
            // Check if the path is directly from the dependency.
            match node_manifest.dep_path(dep_name) {
                None => Err(DepPathError::NoEntryInManifest(
                    dep_name.to_string(),
                    node_manifest.pkg.name.to_string(),
                )),
                Some(path) if !path.exists() => {
                    Err(DepPathError::PathDoesNotExist(dep_name.to_string(), path))
                }
                Some(path) => Ok(path),
            }
        }
    }
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

/// Check that the given `path_root` is actually the path root of the given `path_dep`.
fn check_path_root(
    graph: &Graph,
    path_dep: NodeIx,
    path_root: PinnedId,
) -> Result<(), CheckPathRootError> {
    let path_root_node = find_path_root(graph, path_dep)?;
    if graph[path_root_node].id() != path_root {
        return Err(CheckPathRootError::Invalid(
            graph[path_dep].name.to_string(),
            path_root,
            graph[path_root_node].id(),
        ));
    }
    Ok(())
}

/// Find the node that is the path root for the given node.
///
/// Returns an `Err` in the case that the path root could not be resolved.
fn find_path_root(graph: &Graph, mut node: NodeIx) -> Result<NodeIx, FindPathRootError> {
    loop {
        let pkg = &graph[node];
        match pkg.source {
            source::Pinned::Member(_) => return Ok(node),
            source::Pinned::Path(ref src) => {
                let parent = graph
                    .edges_directed(node, Direction::Incoming)
                    .next()
                    .map(|edge| edge.source())
                    .ok_or_else(|| FindPathRootError(format!("{src}")))?;
                node = parent;
            }
        }
    }
}

/// Perform a toposort on the reversed weights to determine compilation order.
///
/// This ensures all dependencies are compiled prior to their dependents.
fn compilation_order(graph: &Graph) -> Result<Vec<NodeIx>, DependencyCycle> {
    let rev_graph = petgraph::visit::Reversed(&graph);
    petgraph::algo::toposort(rev_graph, None).map_err(|_cycle| {
        let sccs = petgraph::algo::kosaraju_scc(graph);
        let cycle = sccs
            .into_iter()
            .find(|path| path.len() > 1)
            .expect("one cycle must exist")
            .into_iter()
            .map(|n| graph[n].name.to_string())
            .collect();
        DependencyCycle(cycle)
    })
}
