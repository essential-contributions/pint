//! `pint new` implementation.

use clap::{builder::styling::Style, Parser};
use pint_pkg::{
    manifest::{ManifestFile, PackageKind},
    new::new_pkg,
};
use std::path::PathBuf;

/// Create a new package.
#[derive(Parser, Debug)]
pub struct Args {
    /// Specify the "contract" package kind.
    ///
    /// This is the default behaviour.
    #[arg(long)]
    contract: bool,
    /// Specify the "library" package kind.
    ///
    /// By default, new packages are created with the "contract" kind.
    #[arg(long)]
    lib: bool,
    /// Optionally provide a name.
    ///
    /// By default, the package name is the last directory in the canonicalized
    /// representation of the given path.
    #[arg(long)]
    name: Option<String>,
    /// The directory path in which the package should be created.
    path: PathBuf,
}

fn kind_from_bools(contract: bool, lib: bool) -> anyhow::Result<Option<PackageKind>> {
    let opt = match (contract, lib) {
        (false, false) => None,
        (true, false) => Some(PackageKind::Contract),
        (false, true) => Some(PackageKind::Library),
        (true, true) => anyhow::bail!("must choose either `contract` or `lib`, not both"),
    };
    Ok(opt)
}

pub(crate) fn cmd(args: Args) -> anyhow::Result<()> {
    let name = args.name;
    let kind = kind_from_bools(args.contract, args.lib)?;
    let opts = pint_pkg::new::Options { name, kind };
    let manifest_path = new_pkg(&args.path, opts)?;
    let manifest = ManifestFile::from_path(&manifest_path)?;
    let bold = Style::new().bold();
    println!(
        "     {}Created{} {} [{}] ({})",
        bold.render(),
        bold.render_reset(),
        manifest.pkg.name,
        manifest.pkg.kind,
        manifest.dir().display(),
    );
    Ok(())
}
