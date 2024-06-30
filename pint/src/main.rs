use anyhow::Context;
use clap::{builder::styling::Style, CommandFactory, Parser, Subcommand};

mod build;
mod new;
mod plugin;

#[derive(Parser, Debug)]
#[command(
    name = "pint",
    about = "Pint's package manager and CLI plugin host",
    version
)]
struct Pint {
    #[command(subcommand)]
    cmd: Cmd,
}

#[derive(Debug, Subcommand)]
enum Cmd {
    #[command(alias = "b")]
    Build(build::Args),
    New(new::Args),
    /// Print all pint plugins found in `PATH`.
    Plugins,
    /// A catch-all for unknown pint plugins and their arguments.
    ///
    /// Checks for a plugin exe named `pint-<unknown-subcommand>` and tries to
    /// execute it with the remaining args.
    #[clap(external_subcommand)]
    Plugin(Vec<String>),
}

// Manually print the help output for missing/unknown commands or plugins.
fn print_help_output() -> anyhow::Result<()> {
    let mut cli = Pint::command();
    cli.print_help()?;
    Ok(())
}

fn run() -> anyhow::Result<()> {
    let pint = Pint::parse();
    match pint.cmd {
        Cmd::New(arg) => new::cmd(arg),
        Cmd::Build(arg) => build::cmd(arg),
        Cmd::Plugins => {
            plugin::print_all();
            Ok(())
        }
        Cmd::Plugin(args) if !args.is_empty() => {
            let Some(plugin_exe_path) = plugin::find_exe(&args[0]) else {
                // There's no plugin with the given name, so print help output.
                print_help_output()?;
                anyhow::bail!("no known command or plugin {:?}", &args[0]);
            };
            let output = plugin::exec(&plugin_exe_path, &args[1..])?;
            std::process::exit(output.status.code().context("unknown exit status")?);
        }
        _ => print_help_output(),
    }
}

fn main() {
    if let Err(err) = run() {
        let bold = Style::new().bold();
        eprintln!("{}Error:{} {err}", bold.render(), bold.render_reset());
        std::process::exit(1);
    }
}
