//! `pint build` implementation.

use anyhow::Context;
use clap::builder::styling::Style;
use pint_pkg::{
    build::{BuiltPkg, BuiltPkgs},
    manifest::{ManifestFile, PackageKind},
    plan::Plan,
};
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

/// Build a package, writing the generated artifacts to `out/`.
#[derive(clap::Args, Debug)]
pub struct Args {
    /// The path to the package manifest.
    ///
    /// If not provided, the current directory is checked and then each parent
    /// recursively until a manifest is found.
    #[arg(long = "manifest-path")]
    manifest_path: Option<PathBuf>,
    /// A 256-bit unsigned integer in hexadeciaml format that represents the contract "salt". The
    /// value is left padded with zeros if it has less than 64 hexadecimal digits.
    ///
    /// The "salt" is hashed along with the contract's bytecode in order to make the address of the
    /// contract unique.
    ///
    /// If "salt" is provided for a library package, an error is emitted.
    #[arg(long, value_parser = parse_hex)]
    salt: Option<[u8; 32]>,
    /// Print the parsed package.
    #[arg(long = "print-parsed")]
    print_parsed: bool,
    /// Print the flattened package.
    #[arg(long = "print-flat")]
    print_flat: bool,
    /// Print the optimized package.
    #[arg(long = "print-optimized")]
    print_optimized: bool,
    /// Print the assembly generated for the package.
    #[arg(long = "print-asm")]
    print_asm: bool,
    /// Skip optimizing the package.
    #[arg(long = "skip-optimize", hide = true)]
    skip_optimize: bool,
    /// Don't print anything that wasn't explicitly requested.
    #[arg(long)]
    silent: bool,
}

/// Parses a `&str` that represents a 256-bit unsigned integer in hexadecimal format and converts
/// it into a `[u8; 32]`. If the string has less than 64 hexadecimal digits, left pad with zeros.
///
/// Emits an error if the conversion is not possible.
fn parse_hex(value: &str) -> Result<[u8; 32], String> {
    if value.len() > 64 || !value.chars().all(|c| c.is_ascii_hexdigit()) {
        return Err("Salt must be a hexadecimal number with up to 64 digts (256 bits)".to_string());
    }

    // Pad the value to 64 characters by prepending zeros if needed
    let padded_value = format!("{:0>64}", value);
    let mut salt = [0u8; 32];
    for i in 0..32 {
        salt[i] = u8::from_str_radix(&padded_value[2 * i..2 * i + 2], 16)
            .map_err(|_| "Invalid hexadecimal value")?;
    }
    Ok(salt)
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

/// Build a pint package or workspace given a set of `build` args.
///
/// Returns the build [`Plan`] that was used, along with the set of packages that were built.
pub fn cmd(args: Args) -> anyhow::Result<(Plan, BuiltPkgs)> {
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
    if let PackageKind::Library = manifest.pkg.kind {
        if args.salt.is_some() {
            anyhow::bail!("specifying `salt` for a library package is not allowed");
        }
    }

    let name = manifest.pkg.name.to_string();
    let members = [(name, manifest.clone())].into_iter().collect();
    // TODO: Print fetching process here when remote deps included.
    let plan = pint_pkg::plan::from_members(&members).context("failed to plan compilation")?;

    // Build the given compilation plan.
    let mut builder = pint_pkg::build::build_plan(&plan);
    let options = pint_pkg::build::BuildOptions {
        salts: HashMap::from_iter([(manifest.clone(), args.salt.unwrap_or_default())]),
        print_parsed: args.print_parsed,
        print_flat: args.print_flat,
        print_optimized: args.print_optimized,
        print_asm: args.print_asm,
        skip_optimize: args.skip_optimize,
    };

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
        let _built = match prebuilt.build(&options) {
            Ok(built) => {
                built.print_warnings();
                built
            }
            Err(err) => {
                let msg = format!("{}", err.kind);
                err.print_diagnostics();
                anyhow::bail!("{msg}");
            }
        };
    }

    // Consume the builder and produce the built pkgs.
    let built_pkgs = builder.into_built_pkgs();

    // Write our built member package to the `out/` directory.
    if let Some(&n) = plan.compilation_order().last() {
        let built = &built_pkgs[&n];
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
                let pred_name = summary_predicate_name(&predicate.name);
                let name = format!("{}{}", pinned.name, pred_name);
                let pipe = iter.peek().map(|_| "├──").unwrap_or("└──");
                if !args.silent {
                    println!("         {pipe} {:<name_col_w$} {}", name, predicate.ca);
                }
            }
        }
    }

    Ok((plan, built_pkgs))
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

// In the summary, the root predicate name is empty
fn summary_predicate_name(pred_name: &str) -> &str {
    match pred_name {
        "" => " (predicate)",
        _ => pred_name,
    }
}

/// Determine the width of the column required to fit the name and all
/// name+predicate combos.
fn name_col_w(name: &str, built: &BuiltPkg) -> usize {
    let mut name_w = 0;
    if let BuiltPkg::Contract(contract) = built {
        for predicate in &contract.predicate_metadata {
            let w = summary_predicate_name(&predicate.name).chars().count();
            name_w = std::cmp::max(name_w, w);
        }
    }
    name.chars().count() + name_w
}
