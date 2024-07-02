//! Build all pint packages in the `test-pkgs` directory.
//!
//! This ensures all of the contract ABIs are built prior to proc macro invocation.

use pint_pkg::manifest::ManifestFile;
use std::path::Path;

/// Builds the package at the given manifest path.
fn build_test_pkg(manifest: ManifestFile) {
    // Plan and execute the build.
    let name = manifest.pkg.name.to_string();
    let members = [(name, manifest)].into_iter().collect();
    let plan = pint_pkg::plan::from_members(&members).expect("failed to plan compilation");
    let built_pkgs = pint_pkg::build::build_plan(&plan)
        .build_all()
        .expect("failed to build package");

    // Retrieve the built target package.
    let n = *plan
        .compilation_order()
        .last()
        .expect("compilation plan contains no packages");
    let built = &built_pkgs[&n];

    // Create the output directory.
    let pinned = &plan.graph()[n];
    let manifest = &plan.manifests()[&pinned.id()];
    let out_dir = manifest.out_dir();
    let profile = "debug";
    let profile_dir = out_dir.join(profile);
    std::fs::create_dir_all(&profile_dir).expect("failed to create directory");

    // Write the output
    built
        .write_to_dir(&pinned.name, &profile_dir)
        .expect("failed to write output artifacts");
}

fn main() {
    // Construct the test-pkgs directory path.
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let manifest_dir_path = Path::new(&manifest_dir);
    let test_pkgs_path = manifest_dir_path.join("test-pkgs");

    // Iterate over all inner app directories.
    for entry in std::fs::read_dir(test_pkgs_path)
        .expect("failed to read test-pkgs directory")
        .filter_map(Result::ok)
    {
        let path = entry.path();
        if !path.is_dir() {
            continue;
        }
        // Load the test pkg's manifest.
        let pint_manifest_path = path.join("pint.toml");
        let manifest =
            ManifestFile::from_path(&pint_manifest_path).expect("failed to load manifest");

        // Create the src path so we can check if it changes.
        let src_dir = manifest.src_dir().to_path_buf();

        build_test_pkg(manifest);

        // Rerun the build script if any of the package manifests or src files
        // change.
        println!("cargo:rerun-if-changed={}", pint_manifest_path.display());
        for entry in walkdir::WalkDir::new(&src_dir)
            .into_iter()
            .filter_map(Result::ok)
        {
            println!("cargo:rerun-if-changed={}", entry.path().display());
        }
    }
    println!("cargo:rerun-if-changed=test-pkgs");
}
