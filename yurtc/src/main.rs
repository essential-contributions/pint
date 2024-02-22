use clap::Parser;
use std::{
    fs::{create_dir_all, File},
    path::{Path, PathBuf},
};
use yurtc::{asm_gen, cli::Args, error, parser};

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let filepath = Path::new(&args.filepath);

    // Lex + Parse
    let initial_ii = match parser::parse_project(filepath) {
        Ok(ii) => ii,
        Err(errors) => {
            if !cfg!(test) {
                error::print_errors(&errors);
            }
            yurtc::yurtc_bail!(errors.len(), filepath)
        }
    };

    // Convert the initial IntermediateIntent to a final IntermediateIntent by performing type
    // checking, flattening, optimizations, etc.
    #[allow(unused_mut)]
    let mut final_ii = match initial_ii.compile() {
        Ok(ii) => ii,
        Err(error) => {
            if !cfg!(test) {
                error::print_errors(&vec![error::Error::Compile { error }]);
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
        if args.solve && !cfg!(feature = "solver-scip") {
            eprintln!("Solving is disabled in this build.");
        }

        #[cfg(feature = "solver-scip")]
        if args.solve {
            use russcip::ProblemCreated;
            use yurtc::solvers::scip::*;

            // Solve the final intermediate intent. This assumes, for now, that the final
            // intermediate intent has no state variables
            let solver = Solver::<ProblemCreated>::new(&mut final_ii);
            let solver = match solver.solve() {
                Ok(solver) => solver,
                Err(error) => {
                    if !cfg!(test) {
                        error::print_errors(&vec![error::Error::Solve { error }]);
                    }
                    yurtc::yurtc_bail!(1, filepath)
                }
            };
            solver.print_solution();
        }
    } else {
        // This is WIP. So far, simply print the serialized JSON to `<filename>.json` or to
        // `<output>`. That'll likely change in the future when we decide on a serialized scheme.
        match asm_gen::intent_to_asm(&final_ii) {
            Ok(intent) => {
                if args.print_asm {
                    asm_gen::print_asm(&intent);
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
                    &intent,
                )?;
            }
            Err(error) => {
                if !cfg!(test) {
                    error::print_errors(&vec![error::Error::Compile { error }]);
                }
                yurtc::yurtc_bail!(1, filepath)
            }
        };
    }

    Ok(())
}
