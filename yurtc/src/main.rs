use yurtc::{error, parser};

use std::path::Path;

fn main() -> anyhow::Result<()> {
    let (filepath, compile_flag) = parse_cli();
    let filepath = Path::new(&filepath);

    // Lex + Parse
    let intermediate_intent = match parser::parse_project(filepath) {
        Ok(ii) => dbg!(ii),
        Err(errors) => {
            if !cfg!(test) {
                error::print_errors(&errors);
            }
            yurtc::yurtc_bail!(errors.len(), filepath)
        }
    };

    // Compile ast down to a final intent
    if compile_flag {
        let intent = match intermediate_intent.compile() {
            Ok(intent) => intent,
            Err(error) => {
                if !cfg!(test) {
                    error::print_errors(&vec![error::Error::Compile { error }]);
                }
                yurtc::yurtc_bail!(1, filepath)
            }
        };

        dbg!(&intent);
    } else {
        // If `compile_flag` is set, there is no need to print the intermediate intent.
        eprintln!("{intermediate_intent}");
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
