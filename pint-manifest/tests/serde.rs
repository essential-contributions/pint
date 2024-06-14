//! Manifest serialization tests. Note that these do not do validation checks.

use pint_manifest::Manifest;

// Ensures that the `manifest` remains the same after serializing then deserializing.
fn check_roundtrip(manifest: &Manifest) {
    let string = toml::to_string(manifest).unwrap();
    let roundtrip = toml::from_str(&string).unwrap();
    assert_eq!(manifest, &roundtrip);
}

#[test]
fn all_fields() {
    let toml_str = r#"
        [package]
        name = "foo"
        license = "MIT"
        kind = "contract"
        entry-point = "path/to/my/contract.pnt"

        [dependencies]
        bar = { path = "../relative/path/to/bar", package = "barney" }

        [contract-dependencies]
        baz = { path = "/absolute/path/to/baz" }
    "#;
    let manifest: Manifest = toml::from_str(toml_str).unwrap();
    check_roundtrip(&manifest);
}

#[test]
fn minimal() {
    let toml_str = r#"
        [package]
        name = "foo"
    "#;
    let manifest: Manifest = toml::from_str(toml_str).unwrap();
    check_roundtrip(&manifest);
}

#[test]
#[should_panic]
fn missing_name() {
    let toml_str = r#"
        [package]
        license = "MIT"
    "#;
    let _manifest: Manifest = toml::from_str(toml_str).unwrap();
}

#[test]
#[should_panic]
fn missing_package() {
    let toml_str = r#"
        name = "foo"
        license = "MIT"
    "#;
    let _manifest: Manifest = toml::from_str(toml_str).unwrap();
}
