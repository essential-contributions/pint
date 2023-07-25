use ansi_term::Colour::{Cyan, Purple, Red};
use std::io::{self, Write};
use std::path::PathBuf;

fn run_tests(sub_dir: &str) {
    let dir: PathBuf = format!("tests/{sub_dir}").into();

    let mut failed_tests = vec![];

    for entry in std::fs::read_dir(dir).unwrap() {
        let path = entry.unwrap().path();

        let output = test_bin::get_test_bin("yurtc")
            .args([path.clone()])
            .output()
            .expect("Failed to run 'yurtc' binary.");

        if !output.status.success() {
            failed_tests.push(path.display().to_string());
            println!(
                "{} {}. {}\n",
                Purple.paint("FAILED TO COMPILE"),
                Cyan.paint(path.display().to_string()),
                Red.paint("Reported errors:"),
            );

            io::stdout().write_all(&output.stdout).unwrap();
            println!("\n");
        }

        // TODO: actually check the output of the compiler in case of success
    }

    if !failed_tests.is_empty() {
        panic!();
    }
}

#[cfg(test)]
mod e2e {
    use crate::run_tests;

    #[test]
    fn basic_tests() {
        run_tests("basic_tests");
    }
}
