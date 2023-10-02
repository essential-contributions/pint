use std::{ffi::OsStr, path::PathBuf};
use yansi::Color::{Cyan, Red, Yellow};

fn run_tests(sub_dir: &str, _args: &[&OsStr]) -> anyhow::Result<()> {
    let dir: PathBuf = format!("tests/{sub_dir}").into();

    let mut failed_tests = vec![];

    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        println!("Testing {}.", entry.path().display());

        let mut path = entry.path();
        if entry.file_type()?.is_dir() {
            path.push("main.yrt");
        }

        let mut expected_to_fail_txt_path = entry.path();
        expected_to_fail_txt_path.push("expected_to_fail.txt");
        let expected_to_fail = expected_to_fail_txt_path.exists();

        let parse_result = yurtc::ast::parse_project(&path);

        match parse_result {
            Err(errs) => {
                let errs_str = errs
                    .iter()
                    .map(|err| err.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                if !expected_to_fail {
                    failed_tests.push(path.display().to_string());
                    println!(
                        "{} {}. {}\n{}",
                        Red.paint("FAILED TO COMPILE"),
                        Cyan.paint(path.display().to_string()),
                        Red.paint("Reported errors:"),
                        Yellow.paint(errs_str),
                    );
                } else {
                    let expected_str =
                        String::from_utf8_lossy(&std::fs::read(expected_to_fail_txt_path)?)
                            .to_string();

                    assert_eq!(errs_str, expected_str.trim_end());
                }
            }

            Ok(ast) => {
                let mut expected_txt_path = entry.path();
                expected_txt_path.push("expected.txt");
                if expected_txt_path.exists() {
                    let ast_str = ast
                        .iter()
                        .map(|decl| format!("{decl}; "))
                        .collect::<Vec<_>>()
                        .concat();
                    let expected_str =
                        String::from_utf8_lossy(&std::fs::read(expected_txt_path)?).to_string();
                    assert_eq!(ast_str.trim_end(), expected_str.trim_end());
                }
            }
        }
    }

    if !failed_tests.is_empty() {
        panic!();
    }
    Ok(())
}

#[cfg(test)]
mod e2e {
    use crate::run_tests;

    #[test]
    fn basic_tests() {
        if let Err(err) = run_tests("basic_tests", &[]) {
            eprintln!("{err}");
        }
    }

    #[test]
    fn modules() {
        if let Err(err) = run_tests("modules", &[]) {
            eprintln!("{err}");
        }
    }
}
