//! Implementations for the different kinds of dependency sources.

use crate::{
    manifest::{self, ManifestFile},
    plan::{PinnedId, PinnedManifests},
};
use serde::{Deserialize, Serialize};
use std::{
    collections::hash_map,
    fmt,
    hash::{Hash, Hasher},
    path::{Path, PathBuf},
};
use thiserror::Error;

mod member;
mod path;

/// Pin this source at a specific "version", return the local directory to fetch into.
trait Pin {
    type Pinned: Fetch + Hash;
    type Error: fmt::Debug + fmt::Display;
    fn pin(&self, ctx: PinCtx) -> Result<(Self::Pinned, PathBuf), Self::Error>;
}

/// Fetch (and optionally cache) a pinned instance of this source to the given path.
trait Fetch {
    type Error: fmt::Debug + fmt::Display;
    fn fetch(&self, ctx: PinCtx, local: &Path) -> Result<ManifestFile, Self::Error>;
}

/// The canonical, local path for this source as a dependency.
trait DepPath {
    type Error: fmt::Debug + fmt::Display;
    fn dep_path(&self, name: &str) -> Result<DependencyPath, Self::Error>;
}

/// Represents the source for a package.
///
/// The `Source` type does not specify a speccific, pinned version, but does
/// specify a method of retrieving the pinned version.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
pub enum Source {
    /// Used to refer to one of the workspace members.
    Member(member::Source),
    /// A path to a directory with a `pint.toml` manifest at its root.
    Path(path::Source),
}

// The pinned form of a package source.
#[derive(Clone, Debug, Eq, Hash, PartialEq, Deserialize, Serialize)]
pub enum Pinned {
    Member(member::Pinned),
    Path(path::Pinned),
}

/// The context provided to the pinning and fetching of a source type.
#[derive(Clone)]
pub(crate) struct PinCtx<'a> {
    /// A unique ID associated with the current fetch.
    // TODO: Use this for remote deps.
    pub(crate) _fetch_id: FetchId,
    /// The current package graph path root.
    pub(crate) path_root: PinnedId,
    /// The name of the package being pinned.
    pub(crate) pkg_name: &'a str,
}

pub(crate) enum DependencyPath {
    /// The dependency is another member of the workspace.
    Member,
    /// The dependency is located at this specific path.
    // TODO: Rm this `allow` when introducing remote deps.
    #[allow(dead_code)]
    ManifestPath(PathBuf),
    /// Path is pinned via manifest, relative to the given root node.
    Root(PinnedId),
}

/// Error indicating failure to construct `Source` from manifest dependency.
#[derive(Debug, Error)]
pub enum SourceError {
    #[error("failed to canonicalize path {0:?}: {1}")]
    FailedToCanonicalizePath(std::path::PathBuf, std::io::Error),
}

/// Failed to pin or fetch the source of a dependency.
#[derive(Debug, Error)]
pub enum PinAndFetchError {
    /// Failed to pin and fetch a path dependency.
    #[error("{0}")]
    Path(#[from] PinAndFetchErrorKind<path::Source>),
    /// Failed to pin and fetch a member dependency.
    #[error("{0}")]
    Member(#[from] PinAndFetchErrorKind<member::Source>),
}

/// Failed to pin or fetch the source of a particular dependency source type.
#[derive(Debug, Error)]
#[allow(private_bounds, private_interfaces)]
pub enum PinAndFetchErrorKind<T: Pin> {
    #[error("failed to pin dependency source: {0}")]
    Pin(T::Error),
    #[error("failed to fetch dependency source: {0}")]
    Fetch(<T::Pinned as Fetch>::Error),
}

/// Failed to resolve the dependency's path.
#[derive(Debug, Error)]
#[error("failed to resolve the path to the dependency's local source")]
pub enum DepPathError {}

pub type FetchId = u64;

impl Source {
    /// Constructs either a `Path` or a `Member` source from the given relative path.
    ///
    /// The returned `Source` is a `Member` in the case that the canonical path
    /// matches one of the manifest paths in the given `member_manifests` set.
    fn from_relative_path<'a>(
        manifest_dir: &Path,
        relative_path: &Path,
        member_manifests: impl IntoIterator<Item = &'a ManifestFile>,
    ) -> Result<Self, SourceError> {
        let path = manifest_dir.join(relative_path);
        let canonical_path = path
            .canonicalize()
            .map_err(|e| SourceError::FailedToCanonicalizePath(path, e))?;
        let is_member = member_manifests
            .into_iter()
            .any(|pkg_manifest| pkg_manifest.dir() == canonical_path);
        if is_member {
            Ok(Source::Member(member::Source(canonical_path)))
        } else {
            Ok(Source::Path(canonical_path))
        }
    }

    /// Construct a `Source` from the given manifest depenency.
    pub fn from_manifest_dep<'a>(
        manifest_dir: &Path,
        dep: &manifest::Dependency,
        member_manifests: impl IntoIterator<Item = &'a ManifestFile>,
    ) -> Result<Self, SourceError> {
        match &dep.source {
            manifest::dependency::Source::Path(path) => {
                Self::from_relative_path(manifest_dir, &path.path, member_manifests)
            }
        }
    }

    /// Determine the pinned version of a dependency from its source.
    ///
    /// If a manifest does not yet exist for the pinned version, fetch the
    /// dependency into its expected location and add its manifest to the pinned
    /// manifests map.
    pub(crate) fn pin_and_fetch(
        &self,
        ctx: PinCtx,
        manifests: &mut PinnedManifests,
    ) -> Result<Pinned, PinAndFetchError> {
        match self {
            Source::Member(source) => Ok(Pinned::Member(pin_and_fetch(source, ctx, manifests)?)),
            Source::Path(source) => Ok(Pinned::Path(pin_and_fetch(source, ctx, manifests)?)),
        }
    }
}

impl Pinned {
    /// Short-hand for `Pinned::Member(member::Pinned)`.
    pub const MEMBER: Self = Self::Member(member::Pinned);

    /// Retrieve the unpinned instance of this source.
    pub fn unpinned(&self, path: &Path) -> Source {
        match self {
            Self::Member(_) => Source::Member(member::Source(path.to_owned())),
            Self::Path(_) => Source::Path(path.to_owned()),
        }
    }

    /// Return how the pinned source for a dependency can be found on the local file system.
    pub(crate) fn dep_path(&self, name: &str) -> Result<DependencyPath, DepPathError> {
        match self {
            Self::Member(pinned) => Ok(pinned.dep_path(name).expect("infallible")),
            Self::Path(pinned) => Ok(pinned.dep_path(name).expect("infallible")),
        }
    }
}

fn pin_and_fetch<T>(
    source: &T,
    ctx: PinCtx,
    manifests: &mut PinnedManifests,
) -> Result<T::Pinned, PinAndFetchErrorKind<T>>
where
    T: Pin,
    T::Pinned: Clone,
    Pinned: From<T::Pinned>,
{
    let (pinned, fetch_path) = source.pin(ctx.clone()).map_err(PinAndFetchErrorKind::Pin)?;
    let id = PinnedId::new(ctx.pkg_name, &Pinned::from(pinned.clone()));
    if let hash_map::Entry::Vacant(entry) = manifests.entry(id) {
        let res = pinned.fetch(ctx, &fetch_path);
        let manifest = res.map_err(PinAndFetchErrorKind::Fetch)?;
        entry.insert(manifest);
    }
    Ok(pinned)
}

/// A unique ID for a fetch pass.
pub fn fetch_graph_id(path: &Path, timestamp: std::time::Instant) -> FetchId {
    let mut hasher = hash_map::DefaultHasher::new();
    path.hash(&mut hasher);
    timestamp.hash(&mut hasher);
    hasher.finish()
}
