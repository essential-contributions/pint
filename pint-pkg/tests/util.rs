//! Items shared between pint-pkg tests.

#![allow(dead_code)]

use pint_pkg::{
    manifest::{self, Manifest, ManifestFile},
    new::new_pkg,
};
use std::{fs, panic::UnwindSafe, path::Path};

/// Create a temporary directory with a random hash based on current timestamp
/// and call the given function with access to it.
///
/// Cleans up the temporary directory after the given function completes.
pub(crate) fn with_temp_dir<F>(f: F)
where
    F: FnOnce(&Path) + UnwindSafe,
{
    use std::hash::{Hash, Hasher};
    let temp_dir = std::env::temp_dir();
    let ts = std::time::Instant::now();
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    ts.hash(&mut hasher);
    let hash = hasher.finish();
    let dirname = format!("{hash:16x}");
    let dir = temp_dir.join(dirname);
    std::fs::create_dir_all(&dir).expect("failed to create temp dir");
    let res = std::panic::catch_unwind(|| f(&dir));
    std::fs::remove_dir_all(&dir).unwrap();
    res.unwrap();
}

/// Create a new library package with default options.
pub(crate) fn new_lib_pkg(path: &Path) -> ManifestFile {
    let opts = pint_pkg::new::Options {
        kind: Some("library".parse().unwrap()),
        ..Default::default()
    };
    new_pkg(path, opts).unwrap();
    let manifest_path = path.join(ManifestFile::FILE_NAME);
    ManifestFile::from_path(&manifest_path).unwrap()
}

/// Load the manifest from path, edit it with the given function, then write it
/// back to the original file.
pub(crate) fn edit_manifest<F>(manifest_file: &mut ManifestFile, edit: F)
where
    F: FnOnce(&mut Manifest),
{
    let path = manifest_file.path();
    let string = fs::read_to_string(path).unwrap();
    let mut manifest: Manifest = string.parse().unwrap();
    edit(&mut manifest);
    let string = toml::to_string(&manifest).unwrap();
    fs::write(path, string.as_bytes()).unwrap();
    let mut new_manifest_file = ManifestFile::from_path(path).unwrap();
    std::mem::swap(manifest_file, &mut new_manifest_file);
}

/// Add the given `dep` as a dependency to the given `manifest`.
pub(crate) fn insert_dep(manifest: &mut Manifest, dep: &ManifestFile) {
    let name = dep.pkg.name.clone();
    let kind = dep.pkg.kind.clone();
    let path = dep.dir().to_owned();
    let path = manifest::dependency::Path { path };
    let source = manifest::dependency::Source::Path(path);
    let package = None;
    let dep = manifest::Dependency { source, package };
    match kind {
        manifest::PackageKind::Contract => manifest.contract_deps.insert(name, dep),
        manifest::PackageKind::Library => manifest.deps.insert(name, dep),
    };
}
