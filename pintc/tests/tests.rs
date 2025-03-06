use pintc::{
    error::{Errors, Handler, ReportableError},
    predicate::Contract,
    warning::Warnings,
};
use std::{
    fs::{read_dir, File},
    io::{BufRead, BufReader, Read},
    path::{Path, PathBuf},
};
use test_util::{parse_test_data, update_expect, update_expected, TestData};
use yansi::Paint;

mod cli;

fn run_tests(sub_dir: &str, test_name: Option<&str>) -> anyhow::Result<()> {
    let mut failed_tests = vec![];

    // Loop for each file or directory in the `sub_dir`, unless an individual test name has been specified.
    let dir: PathBuf = format!("tests/{sub_dir}").into();
    let entries = match test_name {
        Some(test_name) => {
            let mut full_path = dir;
            full_path.push(test_name);
            vec![full_path]
        }
        None => read_dir(dir)?
            .filter_map(|entry| entry.ok().map(|e| e.path()))
            .collect(),
    };

    for entry in entries {
        println!("Testing {}.", entry.display());

        // If it's a file it's expected to be a self contained pint script.  If it's a directory
        // then `main.pnt` must exist within and will be used.
        let mut path = entry;
        if path.is_dir() {
            path.push("main.pnt");
        }

        // Only go over pint file
        if path.extension().is_none() || path.extension().unwrap() != "pnt" {
            continue;
        }

        // Tests are enabled and set to "solve" mode by default.
        // `<disabled>` disables tests from running completely
        let handle = File::open(path.clone())?;
        if let Some(Ok(first_line)) = BufReader::new(handle).lines().next() {
            // Skip disabled tests.
            if first_line.contains("<disabled>") {
                continue;
            }
        }

        // Parse the file for the expected results for pint parsing, optimising and final output,
        // or errors at any of those stages.
        let test_data = parse_test_data(&path, false /* db_only */)?;

        // Parse the project and check its output.
        let program = parse_test_and_check(&path, &test_data, &mut failed_tests)
            .and_then(|pred|
                // Type check the parsed intent.
                type_check(pred, &test_data, &mut failed_tests, &path))
            .and_then(|pred|
                // Flatten the parsed intent and check the result.
                flatten_and_check(pred, &test_data, &mut failed_tests, &path))
            .map(|pred| optimize(pred, &test_data, &path));

        // Check the `json` ABI if a reference file exists.
        if let Some(program) = program {
            let mut path_stem = path
                .file_stem()
                .expect("Failed to get file stem")
                .to_os_string();
            path_stem.push("-abi");
            path.set_file_name(path_stem);
            path.set_extension("json");

            if let Ok(mut file) = File::open(&path) {
                // The JSON ABI of the produce program
                let handler = Handler::default();
                let json_abi = serde_json::to_string_pretty(&program.abi(&handler).unwrap())?;

                // The JSON ABI from the reference file
                let mut json_abi_reference = String::new();
                file.read_to_string(&mut json_abi_reference)?;

                // Compare the two without any whitespaces
                similar_asserts::assert_eq!(
                    json_abi
                        .chars()
                        .filter(|c| !c.is_whitespace())
                        .collect::<Vec<_>>(),
                    json_abi_reference
                        .chars()
                        .filter(|c| !c.is_whitespace())
                        .collect::<Vec<_>>()
                );
            }
        }
    }

    if !failed_tests.is_empty() {
        panic!();
    }

    Ok(())
}

fn parse_test_and_check(
    path: &Path,
    test_data: &TestData,
    failed_tests: &mut Vec<String>,
) -> Option<Contract> {
    let handler = Handler::default();
    let deps = Default::default();
    match pintc::parser::parse_project(&handler, &deps, path) {
        Err(_) => {
            let errs_str = handler
                .consume()
                .0
                .iter()
                .map(|err| err.display_raw())
                .collect::<String>()
                .trim_end()
                .to_string();
            if update_expect() {
                update_expected(path, "parse_failure", &errs_str);
            } else if let Some(parse_error_str) = &test_data.parse_failure {
                similar_asserts::assert_eq!(parse_error_str.trim_end(), errs_str);
            } else {
                failed_tests.push(path.display().to_string());
                println!(
                    "{} {}. {}\n{}",
                    "FAILED TO PARSE".red(),
                    path.display().to_string().cyan(),
                    "Reported errors:".red(),
                    errs_str.yellow(),
                );
            }
            None
        }
        Ok(pred) => {
            if update_expect() {
                update_expected(path, "parsed", format!("{pred}").trim());
            } else if let Some(expected_intent_str) = &test_data.parsed {
                similar_asserts::assert_eq!(expected_intent_str.trim(), format!("{pred}").trim());
            } else if test_data.parse_failure.is_some() {
                failed_tests.push(path.display().to_string());
                println!(
                    "{} {}.",
                    "UNEXPECTED SUCCESSFUL COMPILE".red(),
                    path.display().to_string().cyan(),
                );
            } else {
                failed_tests.push(path.display().to_string());
                println!(
                    "{} {}.",
                    "MISSING 'parsed' OR 'parse_failure' DIRECTIVE".red(),
                    path.display().to_string().cyan(),
                );
            }
            Some(pred)
        }
    }
}

fn type_check(
    pred: Contract,
    test_data: &TestData,
    failed_tests: &mut Vec<String>,
    path: &Path,
) -> Option<Contract> {
    let handler = Handler::default();
    let res = pred.type_check(&handler);

    let (e, w) = handler.consume();

    let mut err_output = String::default();
    if !e.is_empty() {
        err_output.push_str(&format!("{}", Errors(e)));
    }
    if !w.is_empty() {
        if !err_output.is_empty() {
            err_output.push('\n');
        }
        err_output.push_str(&format!("{}", Warnings(w)));
    }

    // For testing a warning is treated as an error, so *any* output from the handler is a
    // 'failure'.
    if err_output.is_empty() {
        if !update_expect() && test_data.typecheck_failure.is_some() {
            failed_tests.push(path.display().to_string());
            println!(
                "{} {}.",
                "UNEXPECTED SUCCESSFUL TYPE CHECK".red(),
                path.display().to_string().cyan(),
            );
        }

        res.ok()
    } else {
        if update_expect() {
            update_expected(path, "typecheck_failure", &err_output);
        } else if let Some(typecheck_error_str) = &test_data.typecheck_failure {
            similar_asserts::assert_eq!(typecheck_error_str.trim_end(), err_output);
        } else {
            failed_tests.push(path.display().to_string());
            println!(
                "{} {}. {}\n{}",
                "FAILED TO TYPE CHECK INTERMEDIATE INTENT".red(),
                path.display().to_string().cyan(),
                "Reported errors:".red(),
                err_output.yellow(),
            );
        }

        None
    }
}

fn flatten_and_check(
    pred: Contract,
    test_data: &TestData,
    failed_tests: &mut Vec<String>,
    path: &Path,
) -> Option<Contract> {
    let handler = Handler::default();
    pred.flatten(&handler)
        .map(|flattened| {
            if update_expect() {
                update_expected(path, "flattened", format!("{flattened}").trim());
            } else if let Some(expected_flattened_str) = &test_data.flattened {
                similar_asserts::assert_eq!(
                    expected_flattened_str.trim(),
                    format!("{flattened}").trim()
                );
            } else if test_data.flattening_failure.is_some() {
                failed_tests.push(path.display().to_string());
                println!(
                    "{} {}.",
                    "UNEXPECTED SUCCESSFUL COMPILE".red(),
                    path.display().to_string().cyan(),
                );
            } else {
                failed_tests.push(path.display().to_string());
                println!(
                    "{} {}.",
                    "MISSING 'flattened' OR 'flattening_failure' DIRECTIVE".red(),
                    path.display().to_string().cyan(),
                );
            }
            flattened
        })
        .map_err(|_| {
            let err = Errors(handler.consume().0);
            if update_expect() {
                update_expected(path, "flattening_failure", format!("{err}").trim());
            } else if let Some(flattening_error_str) = &test_data.flattening_failure {
                similar_asserts::assert_eq!(flattening_error_str.trim_end(), format!("{err}"));
            } else {
                failed_tests.push(path.display().to_string());
                println!(
                    "{} {}. {}\n{}",
                    "FAILED TO FLATTEN INTERMEDIATE INTENT".red(),
                    path.display().to_string().cyan(),
                    "Reported errors:".red(),
                    format!("{err}").yellow(),
                );
            }
        })
        .ok()
}

// TODO: This probably needs a bit of work. Kept it as is for now, but we need to do the following:
// 1. Always include `optimized` in every test.
// 2. Figure out what to do with warnings
// For now, we just worry about `optimized` and `warnings` if they're already there to start with.
fn optimize(pred: Contract, test_data: &TestData, path: &Path) -> Contract {
    let handler = Handler::default();
    let optimized = pred.optimize(&handler);

    if let Some(expected_optimized_str) = &test_data.optimized {
        if update_expect() {
            update_expected(path, "optimized", format!("{optimized}").trim());
        } else {
            similar_asserts::assert_eq!(
                expected_optimized_str.trim(),
                format!("{optimized}").trim()
            );
        }
    }

    if let Some(expected_warnings_str) = &test_data.warnings {
        let warnings = Warnings(handler.consume().1);
        if update_expect() {
            update_expected(path, "warnings", format!("{warnings}").trim());
        } else {
            similar_asserts::assert_eq!(expected_warnings_str.trim(), format!("{warnings}").trim());
        }
    }

    optimized
}

#[cfg(test)]
mod e2e {
    use crate::run_tests;

    macro_rules! e2e_test {
        ($name: ident) => {
            #[test]
            fn $name() {
                let test_name = std::env::var("TEST_NAME").ok();

                if let Err(err) = run_tests(stringify!($name), test_name.as_deref()) {
                    eprintln!("{err}");
                }
            }
        };
    }

    e2e_test!(abi);
    e2e_test!(arrays);
    e2e_test!(asm);
    e2e_test!(basic_tests);
    e2e_test!(consts);
    e2e_test!(contracts);
    e2e_test!(dyn_arrays);
    e2e_test!(evaluator);
    e2e_test!(generators);
    e2e_test!(interfaces);
    e2e_test!(intrinsics);
    e2e_test!(loops);
    e2e_test!(macros);
    e2e_test!(modules);
    e2e_test!(optimizations);
    e2e_test!(regression);
    e2e_test!(root_types);
    e2e_test!(storage);
    e2e_test!(types);
    e2e_test!(unions);

    #[cfg(feature = "experimental-types")]
    e2e_test!(experimental);
}
