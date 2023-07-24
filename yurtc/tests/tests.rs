use std::path::PathBuf;

use yurtc;

fn run_tests(sub_dir: &str) {
    let dir: PathBuf = format!("tests/{sub_dir}").into();

    let mut failed_tests = vec![];

    for entry in std::fs::read_dir(dir).unwrap() {
        let path = entry.unwrap().path();

        let _ =
            yurtc::parser::parse_path_to_ast(&path, &path.display().to_string()).map_err(|_| {
                failed_tests.push(path.display().to_string());
                println!("Failed to compile {}", path.display());
            });

        // TODO: actually check the output of the compiler
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
