//! Tests for building compilation plans.

#![allow(clippy::disallowed_names)]

use essential_types::{ContentAddress, Word};
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
        let _built = build_plan(&plan).finish().unwrap();
    });
}

#[test]
fn build_default_library() {
    with_temp_dir(|dir| {
        let foo = new_pkg(&dir.join("foo"), PackageKind::Library);
        let members = [(foo.pkg.name.to_string(), foo)].into_iter().collect();
        let plan = pint_pkg::plan::from_members(&members).unwrap();
        let _built = build_plan(&plan).finish().unwrap();
    });
}

// Simple graph `foo` -> `bar`, i.e. foo depends on bar.
#[test]
fn build_contract_one_lib_dep() {
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
        let built_pkgs = match build_plan(&plan).finish() {
            Ok(built) => built,
            Err(err) => {
                err.pkg_err.eprint();
                panic!()
            }
        };

        let order = plan.compilation_order();
        assert_eq!(order.len(), 2, "expected two nodes in the plan");

        // Check the first was bar and that it's a library.
        let bar_n = order[0];
        let built_bar = &built_pkgs[&bar_n];
        assert!(
            matches!(built_bar, &pint_pkg::build::BuiltPkg::Library(_)),
            "expected first built package `bar` to be a library"
        );

        // Check that the last was foo and that it's a contract.
        let foo_n = order[1];
        let BuiltPkg::Contract(contract) = &built_pkgs[&foo_n] else {
            panic!("expected last built package `foo` to be a contract");
        };

        // There's only one nameless intent in our simple `foo` test contract.
        assert_eq!(&contract.intents[0].name, "");
        let intent = &contract.intents[0];

        // It should have at least one constraint.
        assert!(
            !intent.intent.constraints.is_empty(),
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
        let built_pkgs = match build_plan(&plan).finish() {
            Ok(built) => built,
            Err(err) => {
                err.pkg_err.eprint();
                panic!()
            }
        };

        let order = plan.compilation_order();
        assert_eq!(order.len(), 3, "expected three nodes in the plan");

        // Check the first was baz and that it's a library.
        let baz_n = order[0];
        let built_baz = &built_pkgs[&baz_n];
        assert!(
            matches!(built_baz, &pint_pkg::build::BuiltPkg::Library(_)),
            "expected first built package `baz` to be a library"
        );

        // Check the second was bar and that it's a library.
        let bar_n = order[1];
        let built_bar = &built_pkgs[&bar_n];
        assert!(
            matches!(built_bar, &pint_pkg::build::BuiltPkg::Library(_)),
            "expected second built package `bar` to be a library"
        );

        // Check that the last was foo and that it's a contract.
        let foo_n = order[2];
        let BuiltPkg::Contract(contract) = &built_pkgs[&foo_n] else {
            panic!("expected last built package `foo` to be a contract");
        };

        // There's only one nameless intent in our simple `foo` test contract.
        assert_eq!(&contract.intents[0].name, "");
        let intent = &contract.intents[0];

        // It should have at least one constraint.
        assert!(
            !intent.intent.constraints.is_empty(),
            "built foo should have at least one constraint"
        );
    });
}

// Simple graph `foo` -> `bar`, i.e. foo depends on bar, both are contracts.
#[test]
fn build_contract_one_contract_dep() {
    const BAR_SRC: &str = r#"storage {
    counter: int,
}

intent Init {
    var value: int;
    state counter: int = storage::counter;
    constraint counter' == value;
}

intent Increment {
    state counter: int = storage::counter;
    constraint counter' == counter + 1;
}
"#;

    const FOO_SRC: &str = r#"
// The top-level contract address.
constraint bar::ADDRESS != 0x0000000000000000000000000000000000000000000000000000000000000000;

// The intent addresses.
constraint bar::Init::ADDRESS != 0x0000000000000000000000000000000000000000000000000000000000000000;
constraint bar::Increment::ADDRESS != 0x0000000000000000000000000000000000000000000000000000000000000000;

solve satisfy;"#;

    with_temp_dir(|dir| {
        // Overwrite `bar`'s source with a simple counter example.
        let bar = new_pkg(&dir.join("bar"), PackageKind::Contract);
        std::fs::write(bar.entry_point(), BAR_SRC.as_bytes()).unwrap();

        // Overwrite `foo`'s source with our custom source that references `bar`'s CA.
        let mut foo = new_pkg(&dir.join("foo"), PackageKind::Contract);
        std::fs::write(foo.entry_point(), FOO_SRC.as_bytes()).unwrap();

        // Create dependency foo -> bar.
        edit_manifest(&mut foo, |m| insert_dep(m, &bar));

        let members = [(foo.pkg.name.to_string(), foo)].into_iter().collect();
        let plan = pint_pkg::plan::from_members(&members).unwrap();
        let built_pkgs = match build_plan(&plan).finish() {
            Ok(built) => built,
            Err(err) => {
                err.pkg_err.eprint();
                panic!()
            }
        };

        let order = plan.compilation_order();
        assert_eq!(order.len(), 2, "expected two nodes in the plan");

        // Check the first was bar and that it's a contract.
        let bar_n = order[0];
        let built_bar = &built_pkgs[&bar_n];
        let BuiltPkg::Contract(bar_contract) = &built_bar else {
            panic!("expected first built package `bar` to be a library");
        };

        fn intent_ca<'a>(
            contract: &'a pint_pkg::build::BuiltContract,
            name: &str,
        ) -> &'a ContentAddress {
            &contract
                .intents
                .iter()
                .find(|i| i.name.contains(name))
                .unwrap()
                .ca
        }

        // Retrieve the CA hex strings.
        let bar_ca = &bar_contract.ca;
        let bar_init_ca = intent_ca(bar_contract, "Init");
        let bar_increment_ca = intent_ca(bar_contract, "Increment");

        // Check that the last was foo and that it's a contract.
        let foo_n = order[1];
        let built_foo = &built_pkgs[&foo_n];
        let BuiltPkg::Contract(foo_contract) = built_foo else {
            panic!("expected last built package `foo` to be a contract");
        };

        // Check that the bar contract and intent CAs appear in foo's constraints.
        fn constraints_contain_ca(
            contract: &pint_pkg::build::BuiltContract,
            ca: &ContentAddress,
        ) -> bool {
            // Check one word-sized chunk of the CA at a time. We do this because
            // in the bytecode, the CA is "pushed" to the stack one word at a time.
            for ca_chunk in ca.0.chunks(std::mem::size_of::<Word>()) {
                let mut contains_chunk = false;
                for intent in &contract.intents {
                    for constraint in &intent.intent.constraints {
                        if constraint.windows(ca_chunk.len()).any(|w| w == ca_chunk) {
                            contains_chunk |= true;
                        }
                    }
                }
                if !contains_chunk {
                    return false;
                }
            }
            true
        }
        assert!(constraints_contain_ca(foo_contract, bar_ca));
        assert!(constraints_contain_ca(foo_contract, bar_init_ca));
        assert!(constraints_contain_ca(foo_contract, bar_increment_ca));
    });
}
