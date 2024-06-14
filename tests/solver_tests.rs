use std::{fs::read_dir, path::PathBuf};
use test_util::unwrap_or_continue;
use yansi::Paint;

#[test]
fn solver_e2e() {
    let dir: PathBuf = "solver_tests".to_string().into();
    let mut failed_tests = vec![];
    for entry in read_dir(dir).unwrap() {
        let entry = entry.unwrap();
        println!("Testing {}.", entry.path().display());

        // If it's a file it's expected to be a self contained pint script.  If it's a directory
        // then `main.pnt` must exist within and will be used.
        let mut path = entry.path();
        if entry.file_type().unwrap().is_dir() {
            path.push("main.pnt");
        }

        // Error handler
        let handler = pintc::error::Handler::default();
        let deps = Default::default();

        // Produce the initial `IntermediateIntent`
        let parsed = unwrap_or_continue!(
            pintc::parser::parse_project(&handler, &deps, &path),
            "parse pint",
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

        // Flattened II -> Flatpint
        #[allow(unused)]
        let flatpint = unwrap_or_continue!(
            pint_solve::parse_flatpint(&format!("{flattened}",)[..]),
            "parse Flatpint",
            failed_tests,
            path
        );

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
        println!("Failed solver E2E tests");
        failed_tests
            .iter()
            .for_each(|path| println!("{}", path.display().to_string().red()));
        panic!();
    }
}
