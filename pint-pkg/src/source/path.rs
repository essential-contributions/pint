//! Path source implementation.

use crate::{
    manifest::{ManifestFile, ManifestFileError},
    pkg::PinnedId,
    source,
};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

/// A path to a `pint` package directory.
pub type Source = PathBuf;

/// A pinned instance of a path source.
#[derive(Clone, Debug, Eq, Hash, PartialEq, Deserialize, Serialize)]
pub struct Pinned {
    /// Both workspace member packages and remote packages may specify path
    /// dependencies. This `path_root` indicates the member or remote pinned
    /// package that is the root of the subgraph of path dependencies that this
    /// package is a part of.
    ///
    /// Tracking the `path_root` helps to distinguish between different path
    /// dependencies that share the same name.
    pub path_root: PinnedId,
}

impl source::Pin for Source {
    type Pinned = Pinned;
    type Error = core::convert::Infallible;
    fn pin(&self, ctx: source::PinCtx) -> Result<(Self::Pinned, PathBuf), Self::Error> {
        let path_root = ctx.path_root;
        let pinned = Pinned { path_root };
        Ok((pinned, self.clone()))
    }
}

impl source::Fetch for Pinned {
    type Error = ManifestFileError;
    fn fetch(&self, _ctx: source::PinCtx, local: &Path) -> Result<ManifestFile, Self::Error> {
        let manifest_path = local.join(ManifestFile::FILE_NAME);
        let manifest = ManifestFile::from_path(&manifest_path)?;
        Ok(manifest)
    }
}

impl From<Pinned> for source::Pinned {
    fn from(p: Pinned) -> Self {
        source::Pinned::Path(p)
    }
}
