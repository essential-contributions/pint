//! Tests for the `new_pkg` fn.

use pint_pkg::{manifest::ManifestFile, new::new_pkg};
use util::with_temp_dir;

mod util;

#[test]
fn new_defaults() {
    with_temp_dir(|dir| {
        let path = dir.join("foo");
        let opts = pint_pkg::new::Options::default();
        new_pkg(&path, opts).unwrap();
    });
}

#[test]
fn new_name_contract() {
    with_temp_dir(|dir| {
        let path = dir.join("foo");
        let opts = pint_pkg::new::Options {
            name: Some("foo".to_string()),
            kind: Some("contract".parse().unwrap()),
        };
        new_pkg(&path, opts).unwrap();
    });
}

#[test]
fn new_name_library() {
    with_temp_dir(|dir| {
        let path = dir.join("foo");
        let opts = pint_pkg::new::Options {
            name: Some("foo".to_string()),
            kind: Some("library".parse().unwrap()),
        };
        new_pkg(&path, opts).unwrap();
    });
}

#[test]
fn valid_manifest() {
    with_temp_dir(|dir| {
        let path = dir.join("foo");
        let opts = pint_pkg::new::Options::default();
        new_pkg(&path, opts).unwrap();
        let manifest_path = path.join(ManifestFile::FILE_NAME);
        let _manifest = ManifestFile::from_path(&manifest_path).unwrap();
    });
}

#[test]
#[should_panic]
fn invalid_name() {
    with_temp_dir(|dir| {
        let path = dir.join("foo");
        let opts = pint_pkg::new::Options {
            name: Some("42foo".to_string()),
            ..Default::default()
        };
        new_pkg(&path, opts).unwrap();
    })
}
