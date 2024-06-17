//! Tests for creation of compilation `Plan`s.

#![allow(clippy::disallowed_names)]

use pint_pkg::manifest::PackageKind;
use util::{edit_manifest, insert_dep, new_pkg, with_temp_dir};

mod util;

#[test]
fn one_pkg() {
    with_temp_dir(|dir| {
        // Create a package.
        let name = "foo";
        let manifest = new_pkg(&dir.join(name), PackageKind::Contract);

        // Collect the member manifests (only one).
        let members = [(name.to_string(), manifest)].into_iter().collect();

        // Create the plan.
        let plan = pint_pkg::plan::from_members(&members).unwrap();
        assert_eq!(plan.graph().node_count(), 1);
        let pinned = plan.graph().node_weights().next().unwrap();
        assert_eq!(pinned.name, name);
    });
}

#[test]
fn two_pkgs() {
    with_temp_dir(|dir| {
        // Create two packages.
        let mut foo = new_pkg(&dir.join("foo"), PackageKind::Library);
        let bar = new_pkg(&dir.join("bar"), PackageKind::Library);

        // Create dependency foo -> bar.
        edit_manifest(&mut foo, |m| insert_dep(m, &bar));

        // Create the member map.
        let members = [
            (foo.pkg.name.to_string(), foo.clone()),
            (bar.pkg.name.to_string(), bar),
        ]
        .into_iter()
        .collect();

        // Create the plan.
        let plan = pint_pkg::plan::from_members(&members).unwrap();

        // Bar should be built first as foo depends on it.
        let graph = plan.graph();
        let order = plan.compilation_order();
        assert_eq!(graph[order[0]].name, "bar");
        assert_eq!(graph[order[1]].name, "foo");

        // Check that plan still works with only `foo` as member.
        let members = [(foo.pkg.name.to_string(), foo)].into_iter().collect();
        let _plan = pint_pkg::plan::from_members(&members).unwrap();
    });
}

#[test]
fn three_pkgs() {
    with_temp_dir(|dir| {
        let mut foo = new_pkg(&dir.join("foo"), PackageKind::Library);
        let mut bar = new_pkg(&dir.join("bar"), PackageKind::Library);
        let baz = new_pkg(&dir.join("baz"), PackageKind::Library);

        // Create dependencies foo -> bar -> baz.
        edit_manifest(&mut bar, |m| insert_dep(m, &baz));
        edit_manifest(&mut foo, |m| insert_dep(m, &bar));

        // Create the member map.
        let members = [
            (foo.pkg.name.to_string(), foo.clone()),
            (bar.pkg.name.to_string(), bar),
            (baz.pkg.name.to_string(), baz),
        ]
        .into_iter()
        .collect();

        // Create the plan.
        let plan = pint_pkg::plan::from_members(&members).unwrap();

        // Compilation order should be baz, bar, foo.
        let graph = plan.graph();
        let order = plan.compilation_order();
        assert_eq!(graph[order[0]].name, "baz");
        assert_eq!(graph[order[1]].name, "bar");
        assert_eq!(graph[order[2]].name, "foo");

        // Check that plan still works with only `foo` as member.
        let members = [(foo.pkg.name.to_string(), foo)].into_iter().collect();
        let _plan = pint_pkg::plan::from_members(&members).unwrap();
    });
}

#[test]
fn diamond_pkgs() {
    with_temp_dir(|dir| {
        let mut foo = new_pkg(&dir.join("foo"), PackageKind::Library);
        let mut bar = new_pkg(&dir.join("bar"), PackageKind::Library);
        let mut baz = new_pkg(&dir.join("baz"), PackageKind::Library);
        let qux = new_pkg(&dir.join("qux"), PackageKind::Library);

        // Create dependencies foo -> bar, foo -> baz, bar -> qux, baz -> qux.
        edit_manifest(&mut bar, |m| insert_dep(m, &qux));
        edit_manifest(&mut baz, |m| insert_dep(m, &qux));
        edit_manifest(&mut foo, |m| {
            insert_dep(m, &bar);
            insert_dep(m, &baz);
        });

        // Create the member map.
        let members = [
            (foo.pkg.name.to_string(), foo.clone()),
            (bar.pkg.name.to_string(), bar),
            (baz.pkg.name.to_string(), baz),
            (qux.pkg.name.to_string(), qux),
        ]
        .into_iter()
        .collect();

        // Create the plan.
        let plan = pint_pkg::plan::from_members(&members).unwrap();

        // Compilation order should be qux, bar|baz, foo.
        let graph = plan.graph();
        let order = plan.compilation_order();
        assert_eq!(graph[order[0]].name, "qux");
        assert!(matches!(&graph[order[1]].name[..], "bar" | "baz"));
        assert!(matches!(&graph[order[2]].name[..], "bar" | "baz"));
        assert_eq!(graph[order[3]].name, "foo");

        // Check that plan still works with only `foo` as member.
        let members = [(foo.pkg.name.to_string(), foo)].into_iter().collect();
        let _plan = pint_pkg::plan::from_members(&members).unwrap();
    });
}

#[test]
#[should_panic]
fn cycle() {
    with_temp_dir(|dir| {
        // Create two packages.
        let mut foo = new_pkg(&dir.join("foo"), PackageKind::Library);
        let mut bar = new_pkg(&dir.join("bar"), PackageKind::Library);

        // Create dependencies foo -> bar, bar -> foo.
        edit_manifest(&mut foo, |m| insert_dep(m, &bar));
        edit_manifest(&mut bar, |m| insert_dep(m, &foo));

        // Create the member map.
        let members = [
            (foo.pkg.name.to_string(), foo.clone()),
            (bar.pkg.name.to_string(), bar),
        ]
        .into_iter()
        .collect();

        // Create the plan, fails on detected cycle.
        let _plan = pint_pkg::plan::from_members(&members).unwrap();
    });
}
