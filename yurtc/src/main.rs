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

use std::{fs::read_to_string, path::Path, rc::Rc};

fn main() -> anyhow::Result<()> {
    let (filepath, compile_flag) = parse_cli();
    let source_code = read_to_string(Path::new(&filepath))?;

    let filepath: Rc<Path> = Rc::from(Path::new(&filepath));

    // Lex + Parse
    let ast = match parser::parse_str_to_ast(&source_code, filepath.clone()) {
        Ok(ast) => ast,
        Err(errors) => {
            if !cfg!(test) {
                error::print_errors(&errors);
            }
            yurtc_bail!(errors.len(), filepath)
        }
    };

    // Compile ast down to a final intent
    if compile_flag {
        let intent = match intent::Intent::from_ast(&ast) {
            Ok(intent) => intent,
            Err(error) => {
                if !cfg!(test) {
                    error::print_errors(&vec![error::Error::Compile { error }]);
                }
                yurtc_bail!(1, filepath)
            }
        };

        dbg!(&intent);
    }

    // If `compile_flag` is set, there is no need to print the initial AST.
    if !compile_flag {
        dbg!(&ast);
    }

    Ok(())
}

fn parse_cli() -> (String, bool) {
    // This is very basic for now.  It only take a single source file and a single optional flag.
    // It'll also just exit if `-h` or `-V` are passed, or if there's an error.
    let cli = clap::command!()
        .arg(
            clap::Arg::new("compile")
                .short('c')
                .long("compile")
                .action(clap::ArgAction::SetTrue),
        )
        .arg(
            clap::Arg::new("filepath")
                .required(true)
                .action(clap::ArgAction::Set),
        )
        .get_matches();

    let filepath = cli.get_one::<String>("filepath").unwrap();

    let compile_flag = cli.get_flag("compile");

    (filepath.clone(), compile_flag)
}
