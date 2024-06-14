//! Pint manifest validation.

use pint_manifest::Manifest;
use std::str::FromStr;

#[test]
fn valid_end_number() {
    let toml_str = r#"
        [package]
        name = "foo42"
    "#;
    Manifest::from_str(toml_str).unwrap();
}

#[test]
fn valid_inner_hyphen() {
    let toml_str = r#"
        [package]
        name = "foo-bar"
    "#;
    Manifest::from_str(toml_str).unwrap();
}

#[test]
fn valid_inner_underscore() {
    let toml_str = r#"
        [package]
        name = "foo-bar"
    "#;
    Manifest::from_str(toml_str).unwrap();
}

#[test]
#[should_panic]
fn non_ascii() {
    let toml_str = r#"
        [package]
        name = "名前"
    "#;
    Manifest::from_str(toml_str).unwrap();
}

#[test]
#[should_panic]
fn uppercase() {
    let toml_str = r#"
        [package]
        name = "Foo"
    "#;
    Manifest::from_str(toml_str).unwrap();
}

#[test]
#[should_panic]
fn invalid_start_hyphen() {
    let toml_str = r#"
        [package]
        name = "-foo"
    "#;
    Manifest::from_str(toml_str).unwrap();
}

#[test]
#[should_panic]
fn invalid_start_underscore() {
    let toml_str = r#"
        [package]
        name = "_foo"
    "#;
    Manifest::from_str(toml_str).unwrap();
}

#[test]
#[should_panic]
fn invalid_start_numeric() {
    let toml_str = r#"
        [package]
        name = "42foo"
    "#;
    Manifest::from_str(toml_str).unwrap();
}

#[test]
#[should_panic]
fn invalid_end_hyphen() {
    let toml_str = r#"
        [package]
        name = "foo-"
    "#;
    Manifest::from_str(toml_str).unwrap();
}

#[test]
#[should_panic]
fn invalid_end_underscore() {
    let toml_str = r#"
        [package]
        name = "foo_"
    "#;
    Manifest::from_str(toml_str).unwrap();
}
