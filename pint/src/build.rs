//! `pint build` implementation.

use anyhow::Context;
use clap::{builder::styling::Style, Parser};
use pint_pkg::{build::BuiltPkg, manifest::ManifestFile};
use std::path::PathBuf;

/// Build a package, writing the generated artifacts to `out/`.
#[derive(Parser, Debug)]
pub(crate) struct Args {
    /// The path to the package manifest.
    ///
    /// If not provided, the current directory is checked and then each parent
    /// recursively until a manifest is found.
    #[arg(long = "manifest-path")]
    manifest_path: Option<PathBuf>,
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

        // Package name formatted (including source if not a member).
        let source_str = match pinned.source {
            pint_pkg::source::Pinned::Member(_) => {
                format!("{}", manifest.dir().display())
            }
            _ => format!("{}", pinned.source),
        };

        println!(
            "   {}Compiling{} {} [{}] ({})",
            bold.render(),
            bold.render_reset(),
            pinned.name,
            manifest.pkg.kind,
            source_str,
        );

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

        // Create the output directory.
        let out_dir = manifest.out_dir();
        std::fs::create_dir_all(&out_dir)
            .with_context(|| format!("failed to create directory {out_dir:?}"))?;

        // Create the build profile directory.
        // TODO: Add build profiles with compiler params.
        let profile = "debug";
        let profile_dir = out_dir.join(profile);
        std::fs::create_dir_all(&profile_dir)
            .with_context(|| format!("failed to create directory {profile_dir:?}"))?;

        match built {
            // Nothing to write for `lib`
            BuiltPkg::Library(_lib) => {}
            // Write the contract intents to JSON.
            // TODO: Also write the ABI.
            BuiltPkg::Contract(contract) => {
                let intents: Vec<_> = contract.intents.iter().map(|i| &i.intent).collect();
                let intents_string = serde_json::to_string_pretty(&intents)
                    .context("failed to serialize intents to JSON")?;
                let intents_path = profile_dir.join(&pinned.name).with_extension("json");
                std::fs::write(&intents_path, intents_string)
                    .with_context(|| format!("failed to write {intents_path:?}"))?;
            }
        }

        // Print the build summary.
        println!(
            "    {}Finished{} build [{profile}] in {:?}",
            bold.render(),
            bold.render_reset(),
            build_start.elapsed()
        );

        // Print the build summary for our member package.
        let kind_str = format!("{}", manifest.pkg.kind);
        let padded_kind_str = format!("{kind_str:>12}");
        let padding = &padded_kind_str[..padded_kind_str.len() - kind_str.len()];
        let ca = match built {
            BuiltPkg::Contract(contract) => format!("{}", &contract.ca),
            _ => "".to_string(),
        };
        let name_col_w = name_col_w(&pinned.name, built);

        println!(
            "{padding}{}{kind_str}{} {:<name_col_w$} {}",
            bold.render(),
            bold.render_reset(),
            pinned.name,
            ca,
        );

        // For contracts, print their intents too.
        if let BuiltPkg::Contract(contract) = built {
            let mut iter = contract.intents.iter().peekable();
            while let Some(intent) = iter.next() {
                let name = format!("{}{}", pinned.name, intent.name);
                let pipe = iter.peek().map(|_| "├──").unwrap_or("└──");
                println!("         {pipe} {:<name_col_w$} {}", name, intent.ca);
            }
        }
    }

    Ok(())
}

/// Determine the width of the column required to fit the name and all
/// name+intent combos.
fn name_col_w(name: &str, built: &BuiltPkg) -> usize {
    let mut name_w = 0;
    if let BuiltPkg::Contract(contract) = built {
        for intent in &contract.intents {
            let w = intent.name.chars().count();
            name_w = std::cmp::max(name_w, w);
        }
    }
    name.chars().count() + name_w
}
