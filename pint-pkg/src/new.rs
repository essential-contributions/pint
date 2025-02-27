//! Items related to the creation of new packages.

use crate::manifest::{self, Manifest, ManifestFile};
use std::{
    fs,
    io::{self, Write},
    path::{Path, PathBuf},
};
use thiserror::Error;

/// Options for the `new_pkg` function.
#[derive(Debug, Default)]
pub struct Options {
    /// A name for the package.
    ///
    /// If `None`, the name of the directory is used.
    pub name: Option<String>,
    /// Optionally specify a package kind.
    ///
    /// If `None`, `Contract` is used.
    pub kind: Option<manifest::PackageKind>,
}

#[derive(Debug, Error)]
pub enum NewPkgError {
    /// The given path already exists and is not a directory.
    #[error("the path {0:?} already exists and is not a directory")]
    ExistingPathNotDirectory(PathBuf),
    /// An I/O error occurred.
    #[error("an I/O error occurred: {0}")]
    Io(#[from] io::Error),
    /// The given directory already contains a pint manifest.
    #[error("the given directory already contains a pint manifest: {0:?}")]
    ManifestExists(PathBuf),
    /// Failed to retrieve a package name from the given path.
    #[error("failed to retrieve package name from given path: {0:?}")]
    NameFromPath(PathBuf),
    /// The package name is invalid.
    #[error("package name {0:?} is invalid: {1}")]
    InvalidPkgName(String, manifest::InvalidName),
}

/// Create a new package at the given path.
///
/// If the directory does not yet exist, it will be created.
///
/// On success, returns the path to the package's manifest.
pub fn new_pkg(path: &Path, opts: Options) -> Result<PathBuf, NewPkgError> {
    let manifest_path = path.join(ManifestFile::FILE_NAME);
    let src_path = path.join("src");

    // Check that we wouldn't overwrite something, and that the dir exists.
    if path.exists() {
        if !path.is_dir() {
            return Err(NewPkgError::ExistingPathNotDirectory(path.to_path_buf()));
        }
        if manifest_path.exists() {
            return Err(NewPkgError::ManifestExists(manifest_path));
        }
    } else {
        fs::create_dir_all(path)?;
    }

    // Now that we know the dir exists, we can canonicalise the path.
    let path = path.canonicalize()?;

    // Determine the kind and pkg name.
    let kind = opts.kind.unwrap_or_default();
    let name = match opts.name {
        Some(name) => name,
        None => path
            .file_stem()
            .ok_or_else(|| NewPkgError::NameFromPath(path.to_path_buf()))?
            .to_string_lossy()
            .to_string(),
    };

    // Validate the pkg name.
    manifest::check_name(&name).map_err(|e| NewPkgError::InvalidPkgName(name.to_string(), e))?;

    // Create the `src` dir.
    fs::create_dir_all(&src_path)?;

    // Create the manifest file.
    let manifest_string = new_manifest_string(&name, &kind);
    fs::write(&manifest_path, &manifest_string)?;

    // Create the default pint file.
    let manifest: Manifest = manifest_string.parse().expect("checked in unit testing");
    let pnt_path = src_path.join(manifest.entry_point_str());
    if !pnt_path.exists() {
        let pnt_string = default_pnt_str(&kind);
        fs::write(pnt_path, pnt_string)?;
    }

    // Create or append to .gitignore file.
    let gitignore_path = path.join(".gitignore");
    let mut gitignore_file = fs::OpenOptions::new()
        .append(true)
        .create(true)
        .open(gitignore_path)?;
    gitignore_file.write_all(GITIGNORE.as_bytes())?;

    Ok(manifest_path)
}

fn default_pnt_str(kind: &manifest::PackageKind) -> &'static str {
    match kind {
        manifest::PackageKind::Contract => DEFAULT_CONTRACT_PNT,
        manifest::PackageKind::Library => DEFAULT_LIBRARY_PNT,
    }
}

fn new_manifest_string(name: &str, kind: &manifest::PackageKind) -> String {
    format!(
        r#"[package]
name = "{name}"
kind = "{kind}"

[dependencies]
# Library dependencies go here.

[contract-dependencies]
# Contract dependencies go here.
"#
    )
}

const DEFAULT_CONTRACT_PNT: &str = r#"storage {
    counter: int,
}

predicate Increment() {
    let counter: int? = mut storage::counter;
    constraint (counter == nil && counter'! == 1) || counter'! == counter! + 1;
}
"#;

const DEFAULT_LIBRARY_PNT: &str = r#"union Animal = Cat | Dog;

type Person = {
    address: b256,
    pet: Animal,
};
"#;

const GITIGNORE: &str = r#"out"#;
