use clap::Parser;
use pintc::{
    asm_gen::compile_contract, cli::Args, error, parser, predicate::CompileOptions, warning,
};
use std::{
    fs::{create_dir_all, File},
    path::{Path, PathBuf},
};

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let filepath = Path::new(&args.filepath);

    // Lex + Parse
    let handler = error::Handler::default();
    let deps = Default::default(); // Allow for passing lib deps by CLI?
    let parsed = match parser::parse_project(&handler, &deps, filepath) {
        Ok(parsed) => {
            if args.print_parsed {
                println!("{parsed}");
            }
            parsed
        }
        Err(_) => {
            let errors = handler.consume().0;
            let errors_len = errors.len();
            if !cfg!(test) {
                error::print_errors(&error::Errors(errors));
            }
            pintc::pintc_bail!(errors_len, filepath)
        }
    };

    // Type check, flatten and optimize
    let contract = match handler.scope(|handler| {
        parsed.compile(
            handler,
            CompileOptions {
                skip_optimize: args.skip_optimize,
                print_flat: args.print_flat,
            },
        )
    }) {
        Ok(optimized) => {
            if args.print_optimized && !args.skip_optimize {
                println!("{optimized}");
            }
            optimized
        }
        Err(_) => {
            let errors = handler.consume().0;
            let errors_len = errors.len();
            if !cfg!(test) {
                error::print_errors(&error::Errors(errors));
            }
            pintc::pintc_bail!(errors_len, filepath)
        }
    };

    match handler.scope(|handler| compile_contract(handler, &contract)) {
        Ok(compiled_contract) => {
            if args.print_asm {
                println!("{compiled_contract}");
            }

            // Determine output directory
            let mut output_directory_path = PathBuf::from("");
            let mut output_file_path = filepath.with_extension("json");
            if let Some(ref output) = args.output {
                output_file_path = PathBuf::from(&output);
                output_directory_path = output_file_path.parent().unwrap().to_path_buf();
                if !output_directory_path.exists() {
                    create_dir_all(output_directory_path.clone())?;
                }
            }

            // Produce `json` ABI
            let mut filepath_stem = filepath
                .file_stem()
                .expect("Failed to get file stem")
                .to_os_string();
            filepath_stem.push("-abi");
            let mut json_abi_path = PathBuf::from(filepath);
            json_abi_path.set_file_name(filepath_stem);
            json_abi_path.set_extension("json");
            json_abi_path = output_directory_path.join(json_abi_path);

            // Compute the JSON ABI
            let abi = match handler.scope(|handler| contract.abi(handler)) {
                Ok(abi) => abi,
                Err(_) => {
                    let errors = handler.consume().0;
                    let errors_len = errors.len();
                    if !cfg!(test) {
                        error::print_errors(&error::Errors(errors));
                    }
                    pintc::pintc_bail!(errors_len, filepath)
                }
            };

            // Write ABI and contract
            serde_json::to_writer_pretty(File::create(json_abi_path)?, &abi)?;
            serde_json::to_writer(
                File::create(output_file_path)?,
                &essential_types::contract::Contract {
                    predicates: compiled_contract.predicates,
                    salt: compiled_contract.salt,
                },
            )?;

            // Report any warnings
            if handler.has_warnings() && !cfg!(test) {
                warning::print_warnings(&warning::Warnings(handler.consume().1));
            }
        }
        Err(_) => {
            let (errors, warnings) = handler.consume();
            let errors_len = errors.len();
            if !cfg!(test) {
                error::print_errors(&error::Errors(errors));
                warning::print_warnings(&warning::Warnings(warnings));
            }
            pintc::pintc_bail!(errors_len, filepath)
        }
    };

    Ok(())
}
