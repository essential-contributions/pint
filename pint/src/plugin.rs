//! Support for pint plugins.

use anyhow::Context;
use clap::builder::styling::Style;
use std::{
    path::{Path, PathBuf},
    process::{self, Stdio},
};

/// Find the executable on the path.
pub(crate) fn find_exe(name: &str) -> Option<PathBuf> {
    let file_name = format!("pint-{}", name);
    all().find(|p| p.file_name().and_then(|os| os.to_str()) == Some(&file_name[..]))
}

/// Execute the executable pint plugin at the given path with the given args.
pub(crate) fn exec(plugin_exe: &Path, args: &[String]) -> anyhow::Result<std::process::Output> {
    process::Command::new(plugin_exe)
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .args(args)
        .output()
        .with_context(|| format!("failed to execute pint plugin {plugin_exe:?}"))
}

/// Print all installed plugin subcommands alongside the path to their executable.
pub(crate) fn print_all() {
    // Collect all plugin subcommands alongside their exe path.
    let plugins: Vec<_> = all()
        .map(|p| {
            let file_name = p.file_name().and_then(|os| os.to_str()).unwrap_or("");
            let subcommand = file_name.replace("pint-", "");
            (subcommand, p)
        })
        .collect();

    // If there are none, let the user know.
    if plugins.is_empty() {
        println!("No pint plugins detected in `PATH`");
        return;
    }

    // Otherwise print all plugins.
    println!("Installed Plugins:");

    let name_col_w = plugins
        .iter()
        .map(|(cmd, _)| cmd.chars().count())
        .max()
        .unwrap_or(0);

    let bold = Style::new().bold();
    for (cmd, path) in plugins {
        println!(
            "  {}{cmd:<name_col_w$}{} {}",
            bold.render(),
            bold.render_reset(),
            path.display()
        );
    }
}

/// Iterate over all plugins in the user's `PATH`.
fn all() -> impl Iterator<Item = PathBuf> {
    let dirs = path_dirs();
    dirs.into_iter()
        .flat_map(walkdir::WalkDir::new)
        .filter_map(Result::ok)
        .map(|entry| entry.path().to_path_buf())
        .filter(|p| is_plugin(p))
}

/// All directories in the user's `PATH`.
fn path_dirs() -> Vec<PathBuf> {
    match std::env::var_os("PATH") {
        Some(s) => std::env::split_paths(&s).collect(),
        None => vec![],
    }
}

// Does the given path point to a pint plugin executable.
fn is_plugin(path: &Path) -> bool {
    if let Some(name) = path.file_name().and_then(|os| os.to_str()) {
        if name.starts_with("pint-") && is_executable(path) {
            return true;
        }
    }
    false
}

// Is the given path an executable.
fn is_executable(path: &Path) -> bool {
    #[cfg(unix)]
    {
        use std::os::unix::prelude::*;
        std::fs::metadata(path)
            .map(|meta| meta.is_file() && meta.permissions().mode() & 0o111 != 0)
            .unwrap_or(false)
    }
    #[cfg(not(unix))]
    {
        path.is_file()
    }
}
