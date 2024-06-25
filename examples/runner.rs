use std::{
    fs::{read_dir, File},
    io::{BufRead, BufReader},
    path::PathBuf,
};
use test_util::unwrap_or_continue;
use yansi::Paint;

#[test]
fn examples_runner() -> anyhow::Result<()> {
    let dir: PathBuf = ".".to_string().into();
    let mut failed_tests = vec![];
    for entry in read_dir(dir)? {
        let entry = entry?;

        // If it's a file it's expected to be a self contained pint script.  If it's a directory
        // then `main.pnt` must exist within and will be used.
        let mut path = entry.path();
        if entry.file_type()?.is_dir() {
            path.push("main.pnt");
        } else if path.extension() != Some(std::ffi::OsStr::new("pnt")) {
            continue;
        }

        // Disable examples marked as `<disabled>`. These examples generally require changes in the
        // compiler to pass and they should be re-enabled eventually.
        if let Some(Ok(first_line)) = BufReader::new(File::open(path.clone())?).lines().next() {
            if first_line.contains("<disabled>") {
                continue;
            }
        }

        println!("Testing {}.", entry.path().display());

        // Error handler
        let handler = pintc::error::Handler::default();

        // The example has no dependencies.
        let deps = Default::default();

        // Produce the initial parsed program
        let parsed = unwrap_or_continue!(
            pintc::parser::parse_project(&handler, &deps, &path),
            "parse pint",
            failed_tests,
            path,
            handler
        );

        // Parsed program -> Flattened program
        let _flattened = unwrap_or_continue!(
            parsed.compile(&handler),
            "compile",
            failed_tests,
            path,
            handler
        );

        // TODO: enable this when we can generate assembly for everything including reals, `if`
        // expressions, etc.
        // Flattened program -> Assembly (aka collection of compiled predicates)
        // unwrap_or_continue!(
        //    pintc::asm_gen::compile_program(&handler, &flattened),
        //    "asm gen",
        //    failed_tests,
        //    path,
        //    handler
        // );
    }

    if !failed_tests.is_empty() {
        println!("Failed to compile all examples");
        failed_tests.iter().for_each(|path: &std::path::PathBuf| {
            println!("{}", path.display().to_string().red(),)
        });
        panic!();
    }
    Ok(())
}
