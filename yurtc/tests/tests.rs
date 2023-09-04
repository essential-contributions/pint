use ansi_term::Colour::{Cyan, Purple, Red, Yellow};
use std::{
    ffi::OsStr,
    io::{self, Write},
    path::PathBuf,
};

fn run_tests(sub_dir: &str, args: &[&OsStr]) -> anyhow::Result<()> {
    let dir: PathBuf = format!("tests/{sub_dir}").into();

    let mut failed_tests = vec![];

    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        println!("Testing {}.", entry.path().display());

        let mut path = entry.path();
        if entry.file_type()?.is_dir() {
            path.push("main.yrt");
        }

        let mut args = args.to_vec();
        let args_path = path.clone();
        args.push(args_path.as_ref());

        let output = test_bin::get_test_bin("yurtc")
            .args(args)
            .output()
            .expect("Failed to run 'yurtc' binary.");

        let mut expected_to_fail_txt_path = entry.path();
        expected_to_fail_txt_path.push("expected_to_fail.txt");
        let expected_to_fail = expected_to_fail_txt_path.exists();

        if !output.status.success() {
            if !expected_to_fail {
                failed_tests.push(path.display().to_string());
                println!(
                    "{} {}. {}\n{}\n{}",
                    Purple.paint("FAILED TO COMPILE"),
                    Cyan.paint(path.display().to_string()),
                    Red.paint("Reported errors:"),
                    Yellow.paint(String::from_utf8_lossy(&output.stderr)),
                    String::from_utf8_lossy(&output.stdout),
                );

                io::stdout().write_all(&output.stdout)?;
                println!("\n");
            } else {
                let stdout_str = String::from_utf8_lossy(&output.stdout).to_string();
                let expected_str =
                    String::from_utf8_lossy(&std::fs::read(expected_to_fail_txt_path)?).to_string();
                assert_eq!(stdout_str, expected_str);
            }
        }

        // This is pretty clunky, but it's OK for now.  It only works while we're printing the AST
        // to stderr by default.  The next step is to either have options to print the AST by
        // request, or to make `yurtc` a library crate and call the functions directly rather than
        // use `test_bin`.
        let mut expected_txt_path = entry.path();
        expected_txt_path.push("expected.txt");
        if expected_txt_path.exists() {
            let stderr_str = String::from_utf8_lossy(&output.stderr).to_string();
            let expected_str =
                String::from_utf8_lossy(&std::fs::read(expected_txt_path)?).to_string();
            assert_eq!(stderr_str, expected_str);
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
