use clap::Parser;
use std::{
    fs::{create_dir_all, File},
    path::{Path, PathBuf},
};
use yurtc::{asm_gen, cli::Args, error, error::Errors, parser};

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let filepath = Path::new(&args.filepath);

    // Lex + Parse
    let initial_iis = match parser::parse_project(filepath) {
        Ok(iis) => iis,
        Err(errors) => {
            if !cfg!(test) {
                error::print_errors(&errors);
            }
            yurtc::yurtc_bail!(errors.0.len(), filepath)
        }
    };

    // Convert the initial IntermediateIntent to a final IntermediateIntent by performing type
    // checking, flattening, optimizations, etc.
    #[allow(unused_mut)]
    let mut final_iis = &match initial_iis
        .values()
        .map(|ii| ii.clone().compile())
        .collect::<Result<Vec<_>, _>>()
    {
        Ok(ii) => ii,
        Err(error) => {
            if !cfg!(test) {
                error::print_errors(&Errors(vec![error::Error::Compile { error }]));
            }
            yurtc::yurtc_bail!(1, filepath)
        }
    };

    // The default backend is "codegen" which generates assembly.
    //
    // If `--solve` is passed to `yurtc`, skip assembly generation. I think, eventually, we want to
    // always be generating assembly, but codegen is currently quite lacking (no reals, no negative
    // numbers, etc.) and so, we can solve more intents than we can generate assembly for. When
    // this changes, we will always generate assembly and only solve when requested via `--solve`.
    if args.solve {
        let final_ii = &final_iis[0];
        if args.solve && !cfg!(feature = "solver-scip") {
            eprintln!("Solving is disabled in this build.");
        }

        #[cfg(feature = "solver-scip")]
        if args.solve {
            let flatyurt = match yurt_solve::parse_flatyurt(&format!("{final_ii}")[..]) {
                Ok(flatyurt) => flatyurt,
                Err(err) => {
                    if !cfg!(test) {
                        eprintln!("Malformed FlatYurt: {err}");
                    }
                    yurtc::yurtc_bail!(1, filepath)
                }
            };
            let solver = yurt_solve::solver(&flatyurt);
            let solver = match solver.solve() {
                Ok(solver) => solver,
                Err(err) => {
                    if !cfg!(test) {
                        eprintln!("Solver failed: {err}");
                    }
                    yurtc::yurtc_bail!(1, filepath)
                }
            };
            solver.print_solution();
        }
    } else {
        // This is WIP. So far, simply print the serialized JSON to `<filename>.json` or to
        // `<output>`. That'll likely change in the future when we decide on a serialized scheme.
        match final_iis
            .iter()
            .map(asm_gen::intent_to_asm)
            .collect::<Result<Vec<_>, _>>()
        {
            Ok(intents) => {
                if args.print_asm {
                    intents.iter().for_each(asm_gen::print_asm);
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
                    &intents,
                )?;
            }
            Err(error) => {
                if !cfg!(test) {
                    error::print_errors(&Errors(vec![error::Error::Compile { error }]));
                }
                yurtc::yurtc_bail!(1, filepath)
            }
        };
    }

    Ok(())
}
