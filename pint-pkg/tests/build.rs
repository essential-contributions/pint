//! Tests for building compilation plans.

#![allow(clippy::disallowed_names)]

use pint_pkg::{
    build::{build_plan, BuiltPkg},
    manifest::PackageKind,
};
use util::{edit_manifest, insert_dep, new_pkg, with_temp_dir};

mod util;

#[test]
fn build_default_contract() {
    with_temp_dir(|dir| {
        let foo = new_pkg(&dir.join("foo"), PackageKind::Contract);
        let members = [(foo.pkg.name.to_string(), foo)].into_iter().collect();
        let plan = pint_pkg::plan::from_members(&members).unwrap();
        let _built = build_plan(&plan).unwrap();
    });
}

#[test]
fn build_default_library() {
    with_temp_dir(|dir| {
        let foo = new_pkg(&dir.join("foo"), PackageKind::Library);
        let members = [(foo.pkg.name.to_string(), foo)].into_iter().collect();
        let plan = pint_pkg::plan::from_members(&members).unwrap();
        let _built = build_plan(&plan).unwrap();
    });
}

// Simple graph `foo` -> `bar`, i.e. foo depends on bar.
#[test]
fn build_contract_one_dep() {
    const BAR_SRC: &str = r#"type Age = int;"#;
    const FOO_SRC: &str = r#"use bar::Age;

var bob_age: Age = 42;

constraint bob_age == 6 * 7;
solve satisfy;"#;

    with_temp_dir(|dir| {
        let mut foo = new_pkg(&dir.join("foo"), PackageKind::Contract);
        let bar = new_pkg(&dir.join("bar"), PackageKind::Library);

        // Overwrite the default source.
        std::fs::write(foo.entry_point(), FOO_SRC.as_bytes()).unwrap();
        std::fs::write(bar.entry_point(), BAR_SRC.as_bytes()).unwrap();

        // Create dependency foo -> bar.
        edit_manifest(&mut foo, |m| insert_dep(m, &bar));

        let members = [(foo.pkg.name.to_string(), foo)].into_iter().collect();
        let plan = pint_pkg::plan::from_members(&members).unwrap();
        let built = match build_plan(&plan) {
            Ok(built) => built,
            Err(err) => {
                err.print();
                panic!()
            }
        };

        let order = plan.compilation_order();
        assert_eq!(order.len(), 2, "expected two nodes in the plan");

        // Check the first was bar and that it's a library.
        let bar_n = order[0];
        let built_bar = &built.pkgs[&bar_n];
        assert!(
            matches!(built_bar, &pint_pkg::build::BuiltPkg::Library(_)),
            "expected first built package `bar` to be a library"
        );

        // Check that the last was foo and that it's a contract.
        let foo_n = order[1];
        let BuiltPkg::Contract(contract) = &built.pkgs[&foo_n] else {
            panic!("expected last built package `foo` to be a contract");
        };

        // There's only one nameless intent in our simple `foo` test contract.
        assert_eq!(&contract.intents.names[0], "");
        let intent = &contract.intents.intents[0];

        // It should have at least one constraint.
        assert!(
            !intent.constraints.is_empty(),
            "built foo should have at least one constraint"
        );
    });
}

// Simple graph `foo` -> `bar` -> `baz`, i.e. foo depends on bar which depends on baz.
#[test]
fn build_contract_with_transitive_dep() {
    const BAZ_SRC: &str = "type Age = int;";

    const BAR_SRC: &str = r#"use baz::Age;
type Person = {
    age: Age,
};"#;

    const FOO_SRC: &str = r#"
use bar::Person;

var bob = {
    age: 42,
};

constraint bob.age == 6 * 7;
solve satisfy;"#;

    with_temp_dir(|dir| {
        let mut foo = new_pkg(&dir.join("foo"), PackageKind::Contract);
        let mut bar = new_pkg(&dir.join("bar"), PackageKind::Library);
        let baz = new_pkg(&dir.join("baz"), PackageKind::Library);

        // Overwrite the default source.
        std::fs::write(foo.entry_point(), FOO_SRC.as_bytes()).unwrap();
        std::fs::write(bar.entry_point(), BAR_SRC.as_bytes()).unwrap();
        std::fs::write(baz.entry_point(), BAZ_SRC.as_bytes()).unwrap();

        // Create dependencies foo -> bar -> baz.
        edit_manifest(&mut foo, |m| insert_dep(m, &bar));
        edit_manifest(&mut bar, |m| insert_dep(m, &baz));

        let members = [(foo.pkg.name.to_string(), foo)].into_iter().collect();
        let plan = pint_pkg::plan::from_members(&members).unwrap();
        let built = match build_plan(&plan) {
            Ok(built) => built,
            Err(err) => {
                err.print();
                panic!()
            }
        };

        let order = plan.compilation_order();
        assert_eq!(order.len(), 3, "expected three nodes in the plan");

        // Check the first was baz and that it's a library.
        let baz_n = order[0];
        let built_baz = &built.pkgs[&baz_n];
        assert!(
            matches!(built_baz, &pint_pkg::build::BuiltPkg::Library(_)),
            "expected first built package `baz` to be a library"
        );

        // Check the second was bar and that it's a library.
        let bar_n = order[1];
        let built_bar = &built.pkgs[&bar_n];
        assert!(
            matches!(built_bar, &pint_pkg::build::BuiltPkg::Library(_)),
            "expected second built package `bar` to be a library"
        );

        // Check that the last was foo and that it's a contract.
        let foo_n = order[2];
        let BuiltPkg::Contract(contract) = &built.pkgs[&foo_n] else {
            panic!("expected last built package `foo` to be a contract");
        };

        // There's only one nameless intent in our simple `foo` test contract.
        assert_eq!(&contract.intents.names[0], "");
        let intent = &contract.intents.intents[0];

        // It should have at least one constraint.
        assert!(
            !intent.constraints.is_empty(),
            "built foo should have at least one constraint"
        );
    });
}
