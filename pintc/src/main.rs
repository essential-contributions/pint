use clap::Parser;
use pintc::{asm_gen::program_to_intents, cli::Args, error, parser};
use std::{
    fs::{create_dir_all, File},
    path::{Path, PathBuf},
};

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let filepath = Path::new(&args.filepath);

    // Lex + Parse
    let handler = error::Handler::default();
    let parsed = match parser::parse_project(&handler, filepath) {
        Ok(parsed) => {
            if args.print_parsed {
                println!("{parsed}");
            }
            parsed
        }
        Err(_) => {
            let errors = handler.consume();
            let errors_len = errors.len();
            if !cfg!(test) {
                error::print_errors(&error::Errors(errors));
            }
            pintc::pintc_bail!(errors_len, filepath)
        }
    };

    // Type check and flatten
    let flattened = match handler.scope(|handler| parsed.compile(handler)) {
        Ok(flattened) => {
            if args.print_flat {
                println!("{flattened}");
            }
            flattened
        }
        Err(_) => {
            let errors = handler.consume();
            let errors_len = errors.len();
            if !cfg!(test) {
                error::print_errors(&error::Errors(errors));
            }
            pintc::pintc_bail!(errors_len, filepath)
        }
    };

    // The default backend is "codegen" which generates assembly.
    //
    // If `--solve` is passed to `pintc`, skip assembly generation. I think, eventually, we want to
    // always be generating assembly, but codegen is currently quite lacking (no reals, no negative
    // numbers, etc.) and so, we can solve more intents than we can generate assembly for. When
    // this changes, we will always generate assembly and only solve when requested via `--solve`.
    if args.solve {
        if args.solve && !cfg!(feature = "solver-scip") {
            eprintln!("Solving is disabled in this build.");
        }

        #[cfg(feature = "solver-scip")]
        if args.solve {
            let flattened = &flattened.iis.get(&"".to_string()).unwrap();
            let flatpint = match pint_solve::parse_flatpint(&format!("{flattened}")[..]) {
                Ok(flatpint) => flatpint,
                Err(err) => {
                    if !cfg!(test) {
                        eprintln!("Malformed FlatPint: {err}");
                    }
                    pintc::pintc_bail!(1, filepath)
                }
            };
            let solver = pint_solve::solver(&flatpint);
            let solver = match solver.solve() {
                Ok(solver) => solver,
                Err(err) => {
                    if !cfg!(test) {
                        eprintln!("Solver failed: {err}");
                    }
                    pintc::pintc_bail!(1, filepath)
                }
            };
            solver.print_solution();
        }
    } else {
        // This is WIP. So far, simply print the serialized JSON to `<filename>.json` or to
        // `<output>`. That'll likely change in the future when we decide on a serialized scheme.
        match handler.scope(|handler| program_to_intents(handler, &flattened)) {
            Ok(intents) => {
                if args.print_asm {
                    println!("{intents}");
                }
                serde_json::to_writer(
                    if let Some(output) = args.output {
                        let output_file_path = PathBuf::from(&output);
                        let output_directory_path = output_file_path.parent().unwrap();
                        if !output_directory_path.exists() {
                            create_dir_all(output_directory_path)?;
                        }
                        File::create(output_file_path)?
                    } else {
                        File::create(filepath.with_extension("json"))?
                    },
                    &intents.intents,
                )?;
            }
            Err(_) => {
                let errors = handler.consume();
                let errors_len = errors.len();
                if !cfg!(test) {
                    error::print_errors(&error::Errors(errors));
                }
                pintc::pintc_bail!(errors_len, filepath)
            }
        };
    }

    Ok(())
}
