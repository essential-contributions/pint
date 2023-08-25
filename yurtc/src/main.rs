#[macro_use]
mod error;

mod ast;
mod contract;
mod expr;
pub mod intent;
mod lexer;
mod parser;
mod span;
mod types;

fn main() -> anyhow::Result<()> {
    let (srcs, compile_flag) = parse_cli();
    let asts = srcs
        .iter()
        .map(|src| parser::parse_path_to_ast(std::path::Path::new(src), src))
        .collect::<anyhow::Result<Vec<_>>>()?;

    dbg!(&asts);

    if compile_flag {
        let intents = asts
            .iter()
            .map(|ast| intent::Intent::from_ast(ast))
            .collect::<anyhow::Result<Vec<_>>>()?;

        dbg!(&intents);
    }

    Ok(())
}

fn parse_cli() -> (Vec<String>, bool) {
    // This is very basic for now.  Doesn't take any options or anything, just a list of source
    // file path strings.  It'll also just exit if `-h` or `-V` are passed, or if there's an error.
    let cli = clap::command!()
        .arg(
            clap::Arg::new("compile")
                .short('c')
                .long("compile")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            clap::Arg::new("filename")
                .required(true)
                .action(clap::ArgAction::Append),
        )
        .get_matches();

    let srcs = cli
        .get_many::<String>("filename")
        .map(|fs| fs.cloned().collect())
        .unwrap();
    let compile_flag = cli.get_flag("compile");

    (srcs, compile_flag)
}
