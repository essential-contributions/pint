use pintc::{
    error::{Errors, Handler, ReportableError},
    intermediate::Program,
};
use std::{
    fs::{read_dir, File},
    io::{BufRead, BufReader},
    path::{Path, PathBuf},
};
use test_util::{parse_test_data, TestData};
use yansi::Paint;

mod cli;

fn run_tests(sub_dir: &str) -> anyhow::Result<()> {
    let mut failed_tests = vec![];

    // Loop for each file or directory in the `sub_dir`.
    let dir: PathBuf = format!("tests/{sub_dir}").into();
    for entry in read_dir(dir)? {
        let entry = entry?;
        println!("Testing {}.", entry.path().display());

        // If it's a file it's expected to be a self contained pint script.  If it's a directory
        // then `main.pnt` must exist within and will be used.
        let mut path = entry.path();
        if entry.file_type()?.is_dir() {
            path.push("main.pnt");
        }

        // Only go over pint file
        if path.extension().unwrap() != "pnt" {
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
        let test_data = parse_test_data(&path)?;

        // Parse the project and check its output.
        parse_test_and_check(&path, &test_data, &mut failed_tests)
            .and_then(|ii|
                // Type check the intermediate intent.
                type_check(ii, &test_data, &mut failed_tests, &path))
            .and_then(|ii|
                // Flatten the intermediate intent check the result.
                flatten_and_check(ii, &test_data, &mut failed_tests, &path));
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
) -> Option<Program> {
    let handler = Handler::default();
    match pintc::parser::parse_project(&handler, path) {
        Err(_) => {
            let errs_str = handler
                .consume()
                .iter()
                .map(|err| err.display_raw())
                .collect::<String>()
                .trim_end()
                .to_string();

            if let Some(parse_error_str) = &test_data.parse_failure {
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
        Ok(ii) => {
            if let Some(expected_intent_str) = &test_data.intermediate {
                similar_asserts::assert_eq!(expected_intent_str.trim(), format!("{ii}").trim());
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
                    "MISSING 'intermediate' OR 'parse_failure' DIRECTIVE".red(),
                    path.display().to_string().cyan(),
                );
            }
            Some(ii)
        }
    }
}

fn type_check(
    ii: Program,
    test_data: &TestData,
    failed_tests: &mut Vec<String>,
    path: &Path,
) -> Option<Program> {
    let handler = Handler::default();
    ii.type_check(&handler)
        .map(|checked| {
            if test_data.typecheck_failure.is_some() {
                failed_tests.push(path.display().to_string());
                println!(
                    "{} {}.",
                    "UNEXPECTED SUCCESSFUL TYPE CHECK".red(),
                    path.display().to_string().cyan(),
                );
            }
            checked
        })
        .map_err(|_| {
            let err = Errors(handler.consume());
            if let Some(typecheck_error_str) = &test_data.typecheck_failure {
                similar_asserts::assert_eq!(typecheck_error_str.trim_end(), format!("{err}"));
            } else {
                failed_tests.push(path.display().to_string());
                println!(
                    "{} {}. {}\n{}",
                    "FAILED TO TYPE CHECK INTERMEDIATE INTENT".red(),
                    path.display().to_string().cyan(),
                    "Reported errors:".red(),
                    format!("{err}").yellow(),
                );
            }
        })
        .ok()
}

fn flatten_and_check(
    ii: Program,
    test_data: &TestData,
    failed_tests: &mut Vec<String>,
    path: &Path,
) -> Option<Program> {
    let handler = Handler::default();
    ii.flatten(&handler)
        .map(|flattened| {
            if let Some(expected_flattened_str) = &test_data.flattened {
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
            let err = Errors(handler.consume());
            if let Some(flattening_error_str) = &test_data.flattening_failure {
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

#[cfg(test)]
mod e2e {
    use crate::run_tests;

    macro_rules! e2e_test {
        ($name: ident) => {
            #[test]
            fn $name() {
                if let Err(err) = run_tests(stringify!($name)) {
                    eprintln!("{err}");
                }
            }
        };
    }

    e2e_test!(basic_tests);
    e2e_test!(macros);
    e2e_test!(types);
    e2e_test!(generators);
    e2e_test!(arrays);
    e2e_test!(modules);
    e2e_test!(asm);
    e2e_test!(directives);
    e2e_test!(canonicalizes);
    e2e_test!(sets_of_intents);
    e2e_test!(storage);
    e2e_test!(intrinsics);
    e2e_test!(root_types);
    e2e_test!(interfaces);
    e2e_test!(evaluator);
}
