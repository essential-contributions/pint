use essential_types::{
    intent::Directive,
    slots::Slots,
    solution::{KeyMutation, Mutation, Sender, Solution, SolutionData, StateMutation},
    IntentAddress, SourceAddress,
};
use intent_server::{intent::ToIntentAddress, Server};
use num_traits::Num;
use std::{
    collections::HashMap,
    fs::{read_dir, File},
    io::{BufRead, BufReader},
    path::{Path, PathBuf},
};
use yansi::Color::{Cyan, Red, Yellow};
use yurtc::{error::ReportableError, intermediate::IntermediateIntent};
mod cli;

#[cfg(test)]
mod e2e {
    use crate::run_tests;

    macro_rules! e2e_test {
        ($name: ident) => {
            #[test]
            fn $name() {
                if let Err(err) = run_tests(stringify!($name)) {
                    eprintln!("{err}");
                }
            }
        };
    }

    e2e_test!(basic_tests);
    e2e_test!(macros);
    e2e_test!(types);
    e2e_test!(forall);
    e2e_test!(arrays);
    e2e_test!(modules);
    e2e_test!(asm);
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

        // Tests are enabled and set to "solve" mode by default.
        // `<disabled>` disables tests from running completely
        // `<validate>`
        //   - Enables the validation flow of a given solution using the intent server
        //   - Disables the "solve" flow
        // `<no-solve>` disables the "solve" flow
        let mut skip_solve = false;
        let mut validate = false;
        let handle = File::open(path.clone())?;
        if let Some(Ok(first_line)) = BufReader::new(handle).lines().next() {
            // Skip disabled tests.
            if first_line.contains("<disabled>") {
                continue;
            }
            // Check for tests that should validate solutions.
            if first_line.contains("<validate>") {
                validate = true;
            }
            // Check for solver ban.
            if first_line.contains("<no-solve>") {
                skip_solve = true;
            }
        }

        // Parse the file for the expected results for Yurt parsing, optimising and final output,
        // or errors at any of those stages.
        let test_data = parse_test_data(&path)?;

        // Parse the project and check its output.
        parse_test_and_check(&path, &test_data, &mut failed_tests)
            .and_then(|ii|
                // Type check the intermediate intent.
                type_check(ii, &test_data, &mut failed_tests, &path))
            .and_then(|ii|
                // Flatten the intermediate intent check the result.
                flatten_and_check(ii, &test_data, &mut failed_tests, &path))
            .map(|ii| {
                if validate {
                    // Check a given solution. This uses the `db` section if present and checks the
                    // solution against an intent server.
                    validate_solution(ii, &test_data);
                } else if !skip_solve {
                    // Solve the final intent and check the solution
                    solve_and_check(ii, &test_data, &mut failed_tests, &path);
                }
            });
    }

    if !failed_tests.is_empty() {
        panic!();
    }

    Ok(())
}

#[derive(Default)]
struct TestData {
    intermediate: Option<String>,
    parse_failure: Option<String>,
    typecheck_failure: Option<String>,
    flattened: Option<String>,
    flattening_failure: Option<String>,
    solution: Option<String>,
    solve_failure: Option<String>,
    db: Option<String>,
}

// Search for sections within a file which contain the expected output from different stages of
// compilation.  These are within comments in the Yurt source file and are separated with special
// tags.
//
// A section containing a single expected result string has a tag and is delimited by `<<<` and
// `>>>`.  The tags are
//   * `intermediate`
//   * `parse_failure`
//   * `solution`
//   * `solve_failure`
//   * `db`
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

fn parse_test_data(path: &Path) -> anyhow::Result<TestData> {
    let mut test_data = TestData::default();

    #[derive(PartialEq)]
    enum Section {
        None,
        ParseFailure,
        IntermediateIntent,
        TypeCheckFailure,
        FlattenedIntent,
        FlatteningFailure,
        Solution,
        SolveFailure,
        Db,
    }
    let mut cur_section = Section::None;
    let mut section_lines = Vec::<String>::new();

    let comment_re = regex::Regex::new(r"^\s*//")?;
    let open_sect_re = regex::Regex::new(
        r"^\s*//\s*(intermediate|parse_failure|typecheck_failure|flattened|flattening_failure|solution|solve_failure|db)\s*<<<",
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
                "flattened" => cur_section = Section::FlattenedIntent,
                "flattening_failure" => cur_section = Section::FlatteningFailure,
                "typecheck_failure" => cur_section = Section::TypeCheckFailure,
                "solution" => cur_section = Section::Solution,
                "solve_failure" => cur_section = Section::SolveFailure,
                "db" => cur_section = Section::Db,
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
                    test_data.intermediate = Some(section_str);
                }
                Section::ParseFailure => {
                    test_data.parse_failure = Some(section_str);
                }
                Section::TypeCheckFailure => {
                    test_data.typecheck_failure = Some(section_str);
                }
                Section::FlattenedIntent => {
                    test_data.flattened = Some(section_str);
                }
                Section::FlatteningFailure => {
                    test_data.flattening_failure = Some(section_str);
                }
                Section::Solution => {
                    test_data.solution = Some(section_str);
                }
                Section::SolveFailure => {
                    test_data.solve_failure = Some(section_str);
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
            // We're stripping exactly '// ' from the line, presumably.  Three characters.
            section_lines.push(line[3..].to_owned());
        }
    }

    Ok(test_data)
}

fn parse_test_and_check(
    path: &Path,
    test_data: &TestData,
    failed_tests: &mut Vec<String>,
) -> Option<IntermediateIntent> {
    match yurtc::parser::parse_project(path) {
        Err(errs) => {
            let errs_str = errs
                .iter()
                .map(|err| err.display_raw())
                .collect::<String>()
                .trim_end()
                .to_string();

            if let Some(parse_error_str) = &test_data.parse_failure {
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
            if let Some(expected_intent_str) = &test_data.intermediate {
                similar_asserts::assert_eq!(expected_intent_str.trim(), format!("{ii}").trim());
            } else if test_data.parse_failure.is_some() {
                failed_tests.push(path.display().to_string());
                println!(
                    "{} {}.",
                    Red.paint("UNEXPECTED SUCCESSFUL COMPILE"),
                    Cyan.paint(path.display().to_string()),
                );
            } else {
                failed_tests.push(path.display().to_string());
                println!(
                    "{} {}.",
                    Red.paint("MISSING 'intermediate' OR 'parse_failure' DIRECTIVE"),
                    Cyan.paint(path.display().to_string()),
                );
            }
            Some(ii)
        }
    }
}

fn type_check(
    ii: IntermediateIntent,
    test_data: &TestData,
    failed_tests: &mut Vec<String>,
    path: &Path,
) -> Option<IntermediateIntent> {
    ii.type_check()
        .map(|checked| {
            if test_data.typecheck_failure.is_some() {
                failed_tests.push(path.display().to_string());
                println!(
                    "{} {}.",
                    Red.paint("UNEXPECTED SUCCESSFUL TYPE CHECK"),
                    Cyan.paint(path.display().to_string()),
                );
            }
            checked
        })
        .map_err(|err| {
            if let Some(typecheck_error_str) = &test_data.typecheck_failure {
                similar_asserts::assert_eq!(
                    typecheck_error_str.trim_end(),
                    err.display_raw().trim_end()
                );
            } else {
                failed_tests.push(path.display().to_string());
                println!(
                    "{} {}. {}\n{}",
                    Red.paint("FAILED TO TYPE CHECK INTERMEDIATE INTENT"),
                    Cyan.paint(path.display().to_string()),
                    Red.paint("Reported errors:"),
                    Yellow.paint(err.display_raw().trim_end()),
                );
            }
        })
        .ok()
}

fn flatten_and_check(
    ii: IntermediateIntent,
    test_data: &TestData,
    failed_tests: &mut Vec<String>,
    path: &Path,
) -> Option<IntermediateIntent> {
    ii.flatten()
        .map(|flattened| {
            if let Some(expected_flattened_str) = &test_data.flattened {
                similar_asserts::assert_eq!(
                    expected_flattened_str.trim(),
                    format!("{flattened}").trim()
                );
            } else if test_data.flattening_failure.is_some() {
                failed_tests.push(path.display().to_string());
                println!(
                    "{} {}.",
                    Red.paint("UNEXPECTED SUCCESSFUL COMPILE"),
                    Cyan.paint(path.display().to_string()),
                );
            } else {
                failed_tests.push(path.display().to_string());
                println!(
                    "{} {}.",
                    Red.paint("MISSING 'flattened' OR 'flattening_failure' DIRECTIVE"),
                    Cyan.paint(path.display().to_string()),
                );
            }
            flattened
        })
        .map_err(|err| {
            if let Some(flattening_error_str) = &test_data.flattening_failure {
                similar_asserts::assert_eq!(
                    flattening_error_str.trim_end(),
                    err.display_raw().trim_end()
                );
            } else {
                failed_tests.push(path.display().to_string());
                println!(
                    "{} {}. {}\n{}",
                    Red.paint("FAILED TO FLATTEN INTERMEDIATE INTENT"),
                    Cyan.paint(path.display().to_string()),
                    Red.paint("Reported errors:"),
                    Yellow.paint(err.display_raw().trim_end()),
                );
            }
        })
        .ok()
}

#[cfg(not(feature = "solver-scip"))]
fn solve_and_check(_: IntermediateIntent, _: &TestData, _: &mut Vec<String>, _: &Path) {}

#[cfg(feature = "solver-scip")]
fn solve_and_check(
    ii: IntermediateIntent,
    test_data: &TestData,
    failed_tests: &mut Vec<String>,
    path: &Path,
) {
    let flatyurt = yurt_solve::parse_flatyurt(&format!("{ii}")[..]).expect("malformed FlatYurt");
    let solver = yurt_solve::solver(&flatyurt);
    solver
        .solve()
        .map(|solver| {
            if let Some(expected_solution_str) = &test_data.solution {
                similar_asserts::assert_eq!(
                    expected_solution_str.trim(),
                    solver.display_solution_raw().trim()
                );
            } else if test_data.solve_failure.is_some() {
                failed_tests.push(path.display().to_string());
                println!(
                    "{} {}.",
                    Red.paint("UNEXPECTED SUCCESSFUL SOLVE"),
                    Cyan.paint(path.display().to_string()),
                );
            } else {
                failed_tests.push(path.display().to_string());
                println!(
                    "{} {}.",
                    Red.paint("MISSING 'solution' OR 'solve_failure' DIRECTIVE"),
                    Cyan.paint(path.display().to_string()),
                );
            }
        })
        .map_err(|err| {
            if let Some(solve_error_str) = &test_data.solve_failure {
                similar_asserts::assert_eq!(
                    solve_error_str.trim_end(),
                    format!("{err}").trim_end()
                );
            } else {
                failed_tests.push(path.display().to_string());
                println!(
                    "{} {}. {}\n{}",
                    Red.paint("FAILED TO SOLVE TO FINAL INTENT"),
                    Cyan.paint(path.display().to_string()),
                    Red.paint("Reported errors:"),
                    Yellow.paint(format!("{err}").trim_end())
                );
            }
        })
        .ok();
}

/// Deploys a trivial persistent intent set that does nothing. It just has some database that we
/// can use to test things like `get_extern`. This is likely temporary until we have a good way of
/// having multiple *.yrt intents interact with each other in tests.
fn deploy_trivial_persistent_intent(server: &mut Server) {
    let persistent_intent = essential_types::intent::Intent {
        slots: Slots {
            decision_variables: 0,
            state: vec![],
            permits: 1,
        },
        state_read: vec![],
        constraints: vec![],
        directive: Directive::Satisfy,
    };
    let deployed_set_address = server
        .deploy_intent_set(vec![persistent_intent.clone()])
        .expect("failed to deploy intent set!");

    server
        .db()
        .stage(deployed_set_address.clone().into(), [0, 0, 0, 1], Some(69));
    server.db().commit();
}

/// Submits a trivial transient intent that does nothing. Mostly used for solving a persistent
/// intent
fn submit_trivial_transient_intent(server: &mut Server) -> IntentAddress {
    // Create a trivial transient intent that does nothing. It's only used to
    // interact with the deployed intent above
    let transient_intent = essential_types::intent::Intent {
        slots: Slots {
            decision_variables: 0,
            state: vec![],
            permits: 1,
        },
        state_read: vec![],
        constraints: vec![],
        directive: Directive::Satisfy,
    };

    let transient_intent_address = transient_intent.intent_address();

    // Submit the transient intent to the intent pool
    server
        .submit_intent(transient_intent)
        .expect("failed to submit transient intent to intent pool!");

    transient_intent_address
}

fn validate_solution(final_intent: IntermediateIntent, test_data: &TestData) {
    // Arbitrary EOA user address. Eventually, this should be config parameter per test.
    let eoa_address = [1, 2, 3, 4];

    // Convert a `&str` representing a 256-bit hex number (without `0x`) into an array of 4 `i64`
    let hex_to_4_ints = |num: &str| -> [i64; 4] {
        let digits = num_bigint::BigInt::from_str_radix(&num, 16)
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
    };

    yurtc::asm_gen::intent_to_asm(&final_intent)
        .map(|persistent_intent| {
            // For now, intents here are treated as persistent. A trivial transient intent is
            // later specified to interact with the persistent intent, for testing purposes.

            // Spin up a serve instance
            let mut server = Server::new();

            deploy_trivial_persistent_intent(&mut server);
            let transient_intent_address = submit_trivial_transient_intent(&mut server);

            // Deploy the set of intents. For now, this deploys a set that contains a single
            // intent. When we have the ability to extract multiple persistent intents from a
            // Yurt program, then we should deploy the whole set.
            let deployed_set_address = server
                .deploy_intent_set(vec![persistent_intent.clone()])
                .expect("failed to deploy intent set!");

            // Populate the database of the server if a db section is specified
            if let Some(db) = &test_data.db {
                for line in db.lines() {
                    // Collect key and value. Assume the key is a hex and the value is a u64
                    let split = line.split_ascii_whitespace().collect::<Vec<_>>();
                    assert!(
                        split.len() == 2,
                        "each line in the db must contain a key and value"
                    );

                    // <key: hex> <value: i64>
                    server.db().stage(
                        deployed_set_address.clone().into(),
                        hex_to_4_ints(&split[0][2..]),
                        Some(split[1].parse::<i64>().expect("value must be a i64")),
                    );
                    server.db().commit();
                }
            }

            // Parse a solution. Each line in the solution section is a path to a decision
            // variable followed by a number OR a hex key followed by a number
            if let Some(sol) = &test_data.solution {
                // Maps a given decision variable name to its low level decision variables.
                // Each Yurt variable may require multiple low level decision variables,
                // depending how wide the data type is.
                let mut decision_variables_map: HashMap<String, Vec<i64>> = HashMap::new();
                let mut state_mutations: HashMap<[i64; 4], Vec<Mutation>> = HashMap::new();

                for line in sol.lines() {
                    let split = line.split_ascii_whitespace().collect::<Vec<_>>();

                    // If the line starts with a hex key, then this is a state mutation.
                    // Otherwise, assume it's a decision var.
                    if split.len() == 2 && split[0].starts_with("0x") {
                        // <key: hex> <value: i64>
                        let mutation = Mutation::Key(KeyMutation {
                            key: hex_to_4_ints(&split[0][2..]),
                            value: Some(split[1].parse::<i64>().expect("value must be a i64")),
                        });
                        state_mutations
                            .entry(deployed_set_address)
                            .and_modify(|value| value.push(mutation.clone()))
                            .or_insert(vec![mutation]);
                    } else if split.len() == 3 {
                        // <address: hex> <key: hex> <value: i64>
                        let mutation = Mutation::Key(KeyMutation {
                            key: hex_to_4_ints(&split[1][2..]),
                            value: Some(split[2].parse::<i64>().expect("value must be a i64")),
                        });
                        state_mutations
                            .entry(hex_to_4_ints(&split[0][2..]))
                            .and_modify(|value| value.push(mutation.clone()))
                            .or_insert(vec![mutation]);
                    } else {
                        // <var: String> <value: i64>
                        decision_variables_map.insert(
                            split[0].to_string(),
                            split[1..]
                                .iter()
                                .map(|d| d.parse::<i64>().expect("expecting a decimal"))
                                .collect(),
                        );
                    }
                }

                let mut decision_variables = vec![];
                for var in &final_intent.vars {
                    decision_variables.extend(&decision_variables_map[&var.1.name]);
                }

                // Craft a solution, starting with the transitions for each intent.
                let transitions = [
                    // solution data for the trivial transient intent
                    SolutionData {
                        intent_to_solve: SourceAddress::transient(transient_intent_address.clone()),
                        // No decision variables
                        decision_variables: vec![],
                        sender: Sender::Eoa(eoa_address),
                    },
                    // solution data for the deployed intent set. This includes some assignment
                    // of all of its decision variables as well as specifying an input message
                    SolutionData {
                        decision_variables,
                        intent_to_solve: SourceAddress::persistent(
                            deployed_set_address.clone().into(),
                            persistent_intent.intent_address(),
                        ),
                        sender: Sender::transient(eoa_address, transient_intent_address),
                    },
                ];

                let solution = Solution {
                    data: transitions.into_iter().collect(),
                    // State mutations for the deployed intent sets
                    state_mutations: state_mutations
                        .iter()
                        .map(|(k, v)| StateMutation {
                            address: (*k).into(),
                            mutations: v.clone(),
                        })
                        .collect::<Vec<_>>(),
                };

                // check the solution for both intents. Expect a `1` for each intent, hence a
                // total utility of `2`.
                assert_eq!(
                    server
                        .submit_solution(solution)
                        .expect("failed to submit and check solution"),
                    2
                );
            }
        })
        .ok();
}
