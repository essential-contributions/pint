mod ast;

#[macro_use]
mod error;
mod lexer;
mod parser;

fn main() -> anyhow::Result<()> {
    let srcs = parse_cli();
    let asts = srcs
        .iter()
        .map(|src| parser::parse_path_to_ast(std::path::Path::new(src), src))
        .collect::<anyhow::Result<Vec<_>>>()?;

    dbg!(asts);

    Ok(())
}

fn parse_cli() -> Vec<String> {
    // This is very basic for now.  Doesn't take any options or anything, just a list of source
    // file path strings.  It'll also just exit if `-h` or `-V` are passed, or if there's an error.
    clap::command!()
        .arg(
            clap::Arg::new("filename")
                .required(true)
                .action(clap::ArgAction::Append),
        )
        .get_matches()
        .get_many::<String>("filename")
        .map(|fs| fs.cloned().collect())
        .unwrap()
}
