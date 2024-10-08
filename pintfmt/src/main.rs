mod ast;
mod cli;
#[macro_use]
mod error;
mod formatter;
mod lexer;
mod parser;

use clap::Parser;
use cli::Args;
use error::print_on_failure;
use formatter::Format;
use std::{
    fs::{read_to_string, write},
    path::Path,
};

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    args.filepaths
        .iter()
        .map(|src| format_file(src))
        .collect::<anyhow::Result<Vec<_>>>()?;

    Ok(())
}

fn format_file(filename: &str) -> anyhow::Result<()> {
    let src = read_to_string(Path::new(&filename))?;
    let ast = parser::parse_str_to_ast(&src);

    let ast = match ast {
        Ok(ast) => ast,
        Err(errors) => {
            if !cfg!(test) {
                print_on_failure(filename, &src, &errors);
            }
            pintfmt_bail!(errors.len(), filename);
        }
    };

    // Write the ast back into `formatted_code`
    let mut formatted_code = formatter::FormattedCode::new();
    if let Err(error) = ast.format(&mut formatted_code) {
        if !cfg!(test) {
            print_on_failure(filename, &src, &vec![error]);
        }
        pintfmt_bail!(1, filename);
    }

    // Write `formatted_code` back into the original file
    write(Path::new(&filename), formatted_code.as_str())?;

    Ok(())
}
