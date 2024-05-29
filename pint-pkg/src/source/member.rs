//! Member source implementation.

use crate::{
    manifest::{ManifestFile, ManifestFileError},
    source,
};
use serde::{Deserialize, Serialize};
use std::{
    fmt,
    path::{Path, PathBuf},
};

/// The member source location as a canonical path.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, Deserialize, Serialize)]
pub struct Source(pub(super) PathBuf);

/// A pinned instance of a workspace member package.
#[derive(Clone, Debug, Eq, Hash, PartialEq, Deserialize, Serialize)]
pub struct Pinned;

impl source::Pin for Source {
    type Pinned = Pinned;
    type Error = core::convert::Infallible;
    fn pin(&self, _ctx: source::PinCtx) -> Result<(Self::Pinned, PathBuf), Self::Error> {
        Ok((Pinned, self.0.clone()))
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

impl fmt::Display for Pinned {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "member")
    }
}

impl From<Pinned> for source::Pinned {
    fn from(p: Pinned) -> Self {
        source::Pinned::Member(p)
    }
}
