use std::{
    fs::{read_dir, File},
    io::{BufRead, BufReader},
    path::{Path, PathBuf},
};

use yansi::Color::{Cyan, Red, Yellow};

#[cfg(test)]
mod e2e {
    use crate::run_tests;

    #[test]
    fn basic_tests() {
        if let Err(err) = run_tests("basic_tests") {
            eprintln!("{err}");
        }
    }

    #[test]
    fn modules() {
        if let Err(err) = run_tests("modules") {
            eprintln!("{err}");
        }
    }
}

fn run_tests(sub_dir: &str) -> anyhow::Result<()> {
    let mut failed_tests = vec![];

    // Loop for each file or directory in the `sub_dir`.
    let dir: PathBuf = format!("tests/{sub_dir}").into();
    for entry in read_dir(dir)? {
        let entry = entry?;
        println!("Testing {}.", entry.path().display());

        // If it's a file it's expected to be a self contained Yurt script.  If it's a directory
        // then `main.yrt` must exist within and will be used.
        let mut path = entry.path();
        if entry.file_type()?.is_dir() {
            path.push("main.yrt");
        }

        // Skip disabled tests
        let handle = File::open(path.clone())?;
        if let Some(Ok(first_line)) = BufReader::new(handle).lines().next() {
            if first_line.contains("<disabled>") {
                continue;
            }
        }

        // Parse the file for the expected results for Yurt parsing, optimising and final output,
        // or errors at any of those stages.
        let expectations = parse_expectations(&path)?;

        // Parse the project and check its output.
        let _ii = match yurtc::parser::parse_project(&path) {
            Err(errs) => {
                // Just comma separate all the errors on a single line.
                let errs_str = errs
                    .iter()
                    .map(|err| err.to_string())
                    .collect::<Vec<_>>()
                    .join("\n");

                if let Some(parse_error_str) = expectations.parse_failure {
                    similar_asserts::assert_eq!(parse_error_str.trim_end(), errs_str);
                } else {
                    failed_tests.push(path.display().to_string());
                    println!(
                        "{} {}. {}\n{}",
                        Red.paint("FAILED TO PARSE"),
                        Cyan.paint(path.display().to_string()),
                        Red.paint("Reported errors:"),
                        Yellow.paint(errs_str),
                    );
                }
                None
            }

            Ok(ii) => {
                if let Some(expected_intent_str) = expectations.intermediate {
                    // The slotmaps in the IntermediateIntent don't seem to iterate
                    // deterministically so we'll print them in a more predictable way here.
                    let intent_str = format!("{ii}");
                    similar_asserts::assert_eq!(expected_intent_str.trim(), intent_str.trim());
                } else if expectations.parse_failure.is_some() {
                    failed_tests.push(path.display().to_string());
                    println!(
                        "{} {}.",
                        Red.paint("UNEXPECTED SUCCESSFUL COMPILE"),
                        Cyan.paint(path.display().to_string()),
                    );
                }
                Some(ii)
            }
        };

        // TODO compile ii to inent and compare.
    }

    if !failed_tests.is_empty() {
        panic!();
    }

    Ok(())
}

#[derive(Default)]
struct Expectations {
    intermediate: Option<String>,
    parse_failure: Option<String>,
    compiled_intent: Option<String>,
    compile_failure: Option<String>,
    // optimized_intent
    // optimize_failure
    // final_intent
    // final_failure
}

// Search for sections within a file which contain the expected output from different stages of
// compilation.  These are within comments in the Yurt source file and are separated with special
// tags.
//
// A section containing a single expected result string has a tag and is delimited by `<<<` and
// `>>>`.  The tags are `intermediate`, `parse_failure`, `compiled_intent`, `compile_failure`.
//
// e.g. A simple test file may be:
// | let a: int;
// | constraint a == 42;
// | solve satisfy;
// |
// | // intermediate <<<
// | // let ::a: int;
// | // constraint (::a == 42);
// | // solve satisfy;
// | // >>>
//
// Similarly, an error may be tested:
// | state life = total_shambles();
// | solve my tax problem;
// |
// | // parse_failure <<<
// | // expected `maximize`, `minimize`, or `satisfy`, found `my`
// | // >>>

fn parse_expectations(path: &Path) -> anyhow::Result<Expectations> {
    let mut expectations = Expectations::default();

    #[derive(PartialEq)]
    enum Section {
        None,
        ParseFailure,
        IntermediateIntent,
        CompiledIntent,
        CompileFailure, // Deprecated.
    }
    let mut cur_section = Section::None;
    let mut section_lines = Vec::<String>::new();

    let comment_re = regex::Regex::new(r"^\s*//")?;
    let open_sect_re = regex::Regex::new(
        r"^\s*//\s*(intermediate|parse_failure|compiled_intent|compile_failure)\s*<<<",
    )?;
    let close_sect_re = regex::Regex::new(r"^\s*//\s*>>>")?;

    let handle = File::open(path)?;
    for line in BufReader::new(handle).lines() {
        let line = line?;

        // Ignore any line which is not a comment.
        if !comment_re.is_match(&line) {
            continue;
        }

        // Match an open tag.
        if let Some(tag) = open_sect_re.captures(&line) {
            // We shouldn't already be in a section.
            assert!(cur_section == Section::None && section_lines.is_empty());

            match &tag[1] {
                "intermediate" => cur_section = Section::IntermediateIntent,
                "parse_failure" => cur_section = Section::ParseFailure,
                "compiled_intent" => cur_section = Section::CompiledIntent,
                "compile_failure" => cur_section = Section::CompileFailure,
                _ => unreachable!("We can't capture strings not in the regex."),
            }

            continue;
        }

        // Match a close tag.
        if close_sect_re.is_match(&line) {
            // We must have already opened a section.
            assert!(cur_section != Section::None);

            // Gather the section lines into a single string.
            let section_str = section_lines.join("\n");

            // Store it in the correct part of our result.
            match cur_section {
                Section::IntermediateIntent => {
                    expectations.intermediate = Some(section_str);
                }
                Section::ParseFailure => {
                    expectations.parse_failure = Some(section_str);
                }
                Section::CompiledIntent => {
                    expectations.compiled_intent = Some(section_str);
                }
                Section::CompileFailure => {
                    expectations.compile_failure = Some(section_str);
                }
                Section::None => unreachable!("Can't be none, already checked."),
            }

            // Reset the section.
            cur_section = Section::None;
            section_lines.clear();

            continue;
        }

        // Otherwise add this string to the section lines if we're in a section.
        if cur_section != Section::None {
            // We're stripping exactly '// ' from the line, presumably.  Three characters.
            section_lines.push(line[3..].to_owned());
        }
    }

    Ok(expectations)
}
