use pint_solve::parse_flatpint;
use std::{fs, fs::read_dir, path::PathBuf};
use test_util::unwrap_or_continue;
use yansi::Color::Red;

#[test]
fn flat_pint_solve() {
    // Loop for each file or directory in the `tests/tests`.
    let dir: PathBuf = "tests/tests".to_string().into();
    let mut failed_tests = vec![];
    for entry in read_dir(dir).unwrap() {
        let path = entry.unwrap().path();
        println!("Testing {}.", path.display());
        let src = fs::read_to_string(&path).unwrap();

        // Source -> Flatpint
        #[allow(unused)]
        let flatpint = unwrap_or_continue!(parse_flatpint(&src[..]), "parse", failed_tests, path);

        #[cfg(feature = "solver-scip")]
        {
            // Flatpint -> solve
            let solver = unwrap_or_continue!(
                pint_solve::solver(&flatpint).solve(),
                "solve",
                failed_tests,
                path
            );

            // Verify the solution
            if !solver.verify_solution() {
                failed_tests.push(path);
            }
        }
    }

    if !failed_tests.is_empty() {
        println!("\n{}", Red.paint("Failed pint_solve_e2e tests:"));
        failed_tests
            .iter()
            .for_each(|path| println!("  {}", Red.paint(path.display().to_string()),));
        panic!();
    }
}
