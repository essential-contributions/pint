use num_traits::Num;
use std::{
    fs::File,
    io::{BufRead, BufReader},
    path::Path,
};

/// Given a `Result`, unwrap it or emit the errors and `continue;`. Will fail if used outside a
/// loop due to `continue;`.
///
/// This macro has two patterns. One that accepts a "handler" containing the errors and another
/// that does not. If a handler is passed, then extract the errors from it and print them.
/// Otherwise, print the error inside the `Result`.
#[macro_export]
macro_rules! unwrap_or_continue {
    // TODO: @mohammad, do we want to print warnings on our validation_e2e tests?
    // what about for runner.rs aka our examples runner? My best guess is that we don't want to output warnings
    // for either scenario
    ($step: expr, $step_name: expr, $failed: expr, $path: expr) => {{
        match $step {
            Ok(output) => output,
            Err(errs) => {
                $failed.push($path.clone());
                eprintln!(
                    "{}",
                    format!("Failed to {} {}: \n{}", $step_name, $path.display(), errs,).red()
                );
                continue;
            }
        }
    }};

    ($step: expr, $step_name: expr, $failed: expr, $path: expr, $handler: expr) => {{
        match $step {
            Ok(output) => output,
            Err(_) => {
                let errs = pintc::error::Errors($handler.consume().0);
                $failed.push($path.clone());
                eprintln!(
                    "{}",
                    format!("Failed to {} {}: \n{}", $step_name, $path.display(), errs,).red()
                );
                continue;
            }
        }
    }};
}

/// Given an `&str` that represents a hexadecimal number, split it into 4 hexadecimal numbers,
/// convert each number to an `i64`, and return an array of the 4 `i64`s produced. Panics upon
/// failure.
pub fn hex_to_four_ints(num: &str) -> [i64; 4] {
    let digits = num_bigint::BigInt::from_str_radix(&num[2..], 16)
        .expect("must be a hex")
        .to_u64_digits()
        .1
        .iter()
        .map(|d| *d as i64)
        .rev()
        .collect::<Vec<_>>();

    [vec![0; 4 - digits.len()], digits]
        .concat()
        .try_into()
        .unwrap()
}

/// Given an `&str` that represents a hexadecimal number, split it into 32 bytes (`u8`s), and
/// return an array of the 32 `u8`s produced. Panics upon failure
pub fn hex_to_bytes(num: &str) -> [u8; 32] {
    num[2..]
        .as_bytes()
        .chunks(2)
        .enumerate()
        .fold([0u8; 32], |mut acc, (idx, chunk)| {
            acc[idx] = u8::from_str_radix(std::str::from_utf8(chunk).unwrap(), 16).unwrap();
            acc
        })
}

/// Given an array of 4 `i64`s, convert them to hexadecimal and concatinate the result into a
/// single string.
pub fn four_ints_to_hex(arr: [i64; 4]) -> String {
    arr.iter()
        .fold("0x".to_string(), |acc, &num| acc + &format!("{:016X}", num))
}

/// Given an array of 32 `u8`s, convert them to hexadecimal and concatinate the result into a
/// single string.
pub fn bytes_to_hex(arr: [u8; 32]) -> String {
    arr.iter().fold("0x".to_string(), |acc, &byte| {
        acc + &format!("{:02X}", byte)
    })
}

#[derive(Default)]
pub struct TestData {
    pub parsed: Option<String>,
    pub parse_failure: Option<String>,
    pub typecheck_failure: Option<String>,
    pub flattened: Option<String>,
    pub flattening_failure: Option<String>,
    pub optimized: Option<String>,
    pub warnings: Option<String>,
    pub db: Option<String>,
}

// Search for sections within a file which contain the expected output from different stages of
// compilation.  These are within comments in the Pint source file and are separated with special
// tags.
//
// A section containing a single expected result string has a tag and is delimited by `<<<` and
// `>>>`. The tags are
//   * parsed
//   * parse_failure
//   * typecheck_failure
//   * flattened
//   * flattening_failure
//   * optimized
//   * db
//
// e.g. A simple test file may be:
// | let a: int;
// | constraint a == 42;
// | solve satisfy;
// |
// | // parsed <<<
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
pub fn parse_test_data(path: &Path) -> anyhow::Result<TestData> {
    let mut test_data = TestData::default();

    #[derive(PartialEq)]
    enum Section {
        None,
        ParseFailure,
        Predicate,
        TypeCheckFailure,
        Flattened,
        FlatteningFailure,
        Optimized,
        Warnings,
        Db,
    }
    let mut cur_section = Section::None;
    let mut section_lines = Vec::<String>::new();

    let comment_re = regex::Regex::new(r"^\s*//")?;
    let open_sect_re = regex::Regex::new(
        r"^\s*//\s*(parsed|parse_failure|typecheck_failure|flattened|flattening_failure|optimized|warnings|db)\s*<<<",
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
                "parsed" => cur_section = Section::Predicate,
                "parse_failure" => cur_section = Section::ParseFailure,
                "flattened" => cur_section = Section::Flattened,
                "flattening_failure" => cur_section = Section::FlatteningFailure,
                "typecheck_failure" => cur_section = Section::TypeCheckFailure,
                "optimized" => cur_section = Section::Optimized,
                "warnings" => cur_section = Section::Warnings,
                "db" => cur_section = Section::Db,
                _ => unreachable!("We can't capture strings not in the regex."),
            }

            continue;
        }

        // Match a close tag.
        if close_sect_re.is_match(&line) {
            // We must have already opened a section.
            assert!(
                cur_section != Section::None,
                "Unmatched section closing tag found. \
                    There's probably a typo in the section open tag."
            );

            // Gather the section lines into a single string.
            let section_str = section_lines.join("\n");

            // Store it in the correct part of our result.
            match cur_section {
                Section::Predicate => {
                    test_data.parsed = Some(section_str);
                }
                Section::ParseFailure => {
                    test_data.parse_failure = Some(section_str);
                }
                Section::TypeCheckFailure => {
                    test_data.typecheck_failure = Some(section_str);
                }
                Section::Flattened => {
                    test_data.flattened = Some(section_str);
                }
                Section::FlatteningFailure => {
                    test_data.flattening_failure = Some(section_str);
                }
                Section::Optimized => {
                    test_data.optimized = Some(section_str);
                }
                Section::Warnings => {
                    test_data.warnings = Some(section_str);
                }
                Section::Db => {
                    test_data.db = Some(section_str);
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
            // We're stripping exactly '// ' from the line, presumably. Three characters.
            // Unless the whole line is just `//`. In that case, just push an empty string.
            if line == "//" {
                section_lines.push("".to_string());
            } else {
                section_lines.push(line[3..].to_owned());
            }
        }
    }

    Ok(test_data)
}
