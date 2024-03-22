use std::{fs::read_dir, path::PathBuf};
use test_util::unwrap_or_continue;
use yansi::Color::Red;

#[test]
fn solver_e2e() {
    let dir: PathBuf = format!("solver_tests").into();
    let mut failed_tests = vec![];
    for entry in read_dir(dir).unwrap() {
        let entry = entry.unwrap();
        println!("Testing {}.", entry.path().display());

        // If it's a file it's expected to be a self contained Yurt script.  If it's a directory
        // then `main.yrt` must exist within and will be used.
        let mut path = entry.path();
        if entry.file_type().unwrap().is_dir() {
            path.push("main.yrt");
        }

        // Error handler
        let handler = yurtc::error::Handler::default();

        // Produce the initial `IntermediateIntent`
        let parsed = unwrap_or_continue!(
            yurtc::parser::parse_project(&handler, &path),
            "parse yurt",
            failed_tests,
            path,
            handler
        );

        // Parsed II -> Type-checked II
        let type_checked = unwrap_or_continue!(
            parsed.type_check(&handler),
            "type check",
            failed_tests,
            path,
            handler
        );

        // Type checked II -> Flattened II
        let flattened = unwrap_or_continue!(
            type_checked.flatten(&handler),
            "flatten",
            failed_tests,
            path,
            handler
        );

        // Flattened II -> FlatYurt
        #[allow(unused)]
        let flatyurt = unwrap_or_continue!(
            yurt_solve::parse_flatyurt(&format!("{flattened}",)[..]),
            "parse FlatYurt",
            failed_tests,
            path
        );

        #[cfg(feature = "solver-scip")]
        {
            // FlatYurt -> solve
            let solver = unwrap_or_continue!(
                yurt_solve::solver(&flatyurt).solve(),
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
        println!("Failed solver E2E tests");
        failed_tests
            .iter()
            .for_each(|path| println!("{}", Red.paint(path.display().to_string()),));
        panic!();
    }
}
