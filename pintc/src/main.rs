use clap::{builder::styling::Style, Parser};
use essential_types::ContentAddress;
use pintc::{
    asm_gen::compile_contract, cli::Args, error, parser, predicate::CompileOptions, warning,
};
use std::{
    fs::{create_dir_all, File},
    path::{Path, PathBuf},
};
use yansi::Condition;

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
            let (errors, warnings) = handler.consume();
            let errors_len = errors.len();
            if !cfg!(test) {
                error::print_errors(&error::Errors(errors));
                warning::print_warnings(&warning::Warnings(warnings));
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
            let (errors, warnings) = handler.consume();
            let errors_len = errors.len();
            if !cfg!(test) {
                error::print_errors(&error::Errors(errors));
                warning::print_warnings(&warning::Warnings(warnings));
            }
            pintc::pintc_bail!(errors_len, filepath)
        }
    };

    match handler
        .scope(|handler| compile_contract(handler, args.salt.unwrap_or_default(), &contract))
    {
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

            // Output content addresses for the contract and it's predicates
            // todo - ian - figure out contract address
            // todo - ian - figure out salting properly
            // todo - ian - figure out where to put this and how to best organize the helper functions
            // could do a struct that has display?
            let pinned_name = filepath
                .file_stem()
                .expect("Failed to get file stem")
                .to_str()
                .expect("Invalid unicode in contract name");

            let name_col_w = name_col_w(pinned_name, &compiled_contract.names);
            let compiled_predicate_contract_addresses = &compiled_contract
                .predicates
                .iter()
                .map(|compiled_predicate| essential_hash::content_addr(compiled_predicate));

            let ca = essential_hash::contract_addr::from_predicate_addrs(
                compiled_predicate_contract_addresses
                    .clone()
                    .map(|ca| ca.clone()),
                &args.salt.unwrap_or_default(),
            );

            let bold = Style::new().bold();
            let kind_str = "contract";
            let padded_kind_str = format!("{kind_str:>12}");
            let padding = &padded_kind_str[..padded_kind_str.len() - kind_str.len()];
            println!(
                "{padding}{}contract{} {:<name_col_w$} {}",
                bold.render(),
                bold.render_reset(),
                pinned_name,
                ca,
            );

            let iter = &mut compiled_predicate_contract_addresses
                .clone()
                .zip(compiled_contract.names)
                .map(|(ca, name)| (name, ca))
                .peekable();
            while let Some((name, ca)) = iter.next() {
                let pred_name = summary_predicate_name(&name);
                let name = format!("{}{}", pinned_name, pred_name);
                let pipe = iter.peek().map(|_| "├──").unwrap_or("└──");
                println!("         {pipe} {:<name_col_w$} {}", name, ca);
            }

            // Compute the JSON ABI
            let abi = match handler.scope(|handler| contract.abi(handler)) {
                Ok(abi) => abi,
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

// In the summary, the root predicate name is empty
fn summary_predicate_name(pred_name: &str) -> &str {
    match pred_name {
        "" => " (predicate)",
        _ => pred_name,
    }
}

/// Determine the width of the column required to fit the name and all
/// name+predicate combos.
fn name_col_w(name: &str, compiled_predicate_names: &Vec<String>) -> usize {
    let mut name_w = 0;
    for name in compiled_predicate_names {
        let w = summary_predicate_name(&name).chars().count();
        name_w = std::cmp::max(name_w, w);
    }
    name.chars().count() + name_w
}
