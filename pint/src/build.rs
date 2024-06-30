//! `pint build` implementation.

use anyhow::Context;
use clap::{builder::styling::Style, Parser};
use pint_pkg::{build::BuiltPkg, manifest::ManifestFile};
use std::path::{Path, PathBuf};

/// Build a package, writing the generated artifacts to `out/`.
#[derive(Parser, Debug)]
pub(crate) struct Args {
    /// The path to the package manifest.
    ///
    /// If not provided, the current directory is checked and then each parent
    /// recursively until a manifest is found.
    #[arg(long = "manifest-path")]
    manifest_path: Option<PathBuf>,
    /// Print the flattened pint program.
    #[arg(long)]
    print_flat: bool,
    /// Don't print anything that wasn't explicitly requested.
    #[arg(long)]
    silent: bool,
}

// Find the file within the current directory or parent directories with the given name.
fn find_file(mut dir: PathBuf, file_name: &str) -> Option<PathBuf> {
    loop {
        let path = dir.join(file_name);
        if path.exists() {
            return Some(path);
        }
        if !dir.pop() {
            return None;
        }
    }
}

pub(crate) fn cmd(args: Args) -> anyhow::Result<()> {
    let build_start = std::time::Instant::now();

    // Determine the manifest location.
    let manifest_path = match args.manifest_path {
        Some(path) => path,
        None => {
            let current_dir = std::env::current_dir()?;
            match find_file(current_dir, ManifestFile::FILE_NAME) {
                None => anyhow::bail!("no `pint.toml` in the current or parent directories"),
                Some(path) => path,
            }
        }
    };

    // Prepare some ANSI formatting styles for output.
    let bold = Style::new().bold();

    // Prepare the compilation plan.
    let manifest = ManifestFile::from_path(&manifest_path).context("failed to load manifest")?;
    let name = manifest.pkg.name.to_string();
    let members = [(name, manifest)].into_iter().collect();
    // TODO: Print fetching process here when remote deps included.
    let plan = pint_pkg::plan::from_members(&members).context("failed to plan compilation")?;

    // Build the given compilation plan.
    let mut builder = pint_pkg::build::build_plan(&plan);
    while let Some(prebuilt) = builder.next_pkg() {
        let pinned = prebuilt.pinned();
        let manifest = &plan.manifests()[&pinned.id()];
        let source_str = source_string(pinned, manifest.dir());

        if !args.silent {
            println!(
                "   {}Compiling{} {} [{}] ({})",
                bold.render(),
                bold.render_reset(),
                pinned.name,
                manifest.pkg.kind,
                source_str,
            );
        }

        // Build the package.
        let _built = match prebuilt.build() {
            Ok(built) => built,
            Err(err) => {
                let msg = format!("{}", err.kind);
                err.eprint();
                anyhow::bail!("{msg}");
            }
        };
    }

    // Write our built member package to the `out/` directory.
    if let Some(&n) = plan.compilation_order().last() {
        let built = &builder.built_pkgs()[&n];
        let pinned = &plan.graph()[n];
        let manifest = &plan.manifests()[&pinned.id()];

        // Create the output and profile directories.
        // TODO: Add build profiles with compiler params.
        let out_dir = manifest.out_dir();
        let profile = "debug";
        let profile_dir = out_dir.join(profile);
        std::fs::create_dir_all(&profile_dir)
            .with_context(|| format!("failed to create directory {profile_dir:?}"))?;

        // Write the output artifacts to the directory.
        built
            .write_to_dir(&pinned.name, &profile_dir)
            .with_context(|| format!("failed to write output artifacts to {profile_dir:?}"))?;

        if !args.silent {
            // Print the build summary.
            println!(
                "    {}Finished{} build [{profile}] in {:?}",
                bold.render(),
                bold.render_reset(),
                build_start.elapsed()
            );
        }

        // Print the build summary for our member package.
        let kind_str = format!("{}", manifest.pkg.kind);
        let padded_kind_str = format!("{kind_str:>12}");
        let padding = &padded_kind_str[..padded_kind_str.len() - kind_str.len()];
        let ca = match built {
            BuiltPkg::Contract(contract) => format!("{}", &contract.ca),
            _ => "".to_string(),
        };
        let name_col_w = name_col_w(&pinned.name, built);

        if !args.silent {
            println!(
                "{padding}{}{kind_str}{} {:<name_col_w$} {}",
                bold.render(),
                bold.render_reset(),
                pinned.name,
                ca,
            );
        }

        // For contracts, print their predicates too.
        if let BuiltPkg::Contract(contract) = built {
            let mut iter = contract.predicate_metadata.iter().peekable();
            while let Some(predicate) = iter.next() {
                let name = format!("{}{}", pinned.name, predicate.name);
                let pipe = iter.peek().map(|_| "├──").unwrap_or("└──");
                if !args.silent {
                    println!("         {pipe} {:<name_col_w$} {}", name, predicate.ca);
                }
            }
        }
    }

    // Print all flattened contract packages if the flag is set.
    if args.print_flat {
        for &n in plan.compilation_order() {
            let built = &builder.built_pkgs()[&n];
            let pinned = &plan.graph()[n];
            let manifest = &plan.manifests()[&pinned.id()];
            let source_str = source_string(pinned, manifest.dir());
            if let BuiltPkg::Contract(built) = built {
                println!(
                    "{}{}{} ({})",
                    bold.render(),
                    pinned.name,
                    bold.render_reset(),
                    source_str,
                );
                println!("{}", built.flattened);
            }
        }
    }

    Ok(())
}

// Package name formatted (including source if not a member).
fn source_string(pinned: &pint_pkg::plan::Pinned, manifest_dir: &Path) -> String {
    match pinned.source {
        pint_pkg::source::Pinned::Member(_) => {
            format!("{}", manifest_dir.display())
        }
        _ => format!("{}", pinned.source),
    }
}

/// Determine the width of the column required to fit the name and all
/// name+predicate combos.
fn name_col_w(name: &str, built: &BuiltPkg) -> usize {
    let mut name_w = 0;
    if let BuiltPkg::Contract(contract) = built {
        for predicate in &contract.predicate_metadata {
            let w = predicate.name.chars().count();
            name_w = std::cmp::max(name_w, w);
        }
    }
    name.chars().count() + name_w
}
