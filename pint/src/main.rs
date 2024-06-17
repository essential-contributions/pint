use clap::{Parser, Subcommand};

mod build;
mod new;

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
    Build(build::Args),
    New(new::Args),
}

fn main() {
    let pint = Pint::parse();
    let res = match pint.cmd {
        Cmd::New(arg) => new::cmd(arg),
        Cmd::Build(arg) => build::cmd(arg),
    };
    if let Err(err) = res {
        eprintln!("Error: {err}");
    }
}
