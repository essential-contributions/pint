use essential_types::{
    intent::Directive,
    slots::Slots,
    solution::{KeyMutation, Mutation, Sender, Solution, SolutionData, StateMutation},
    IntentAddress, SourceAddress,
};
use intent_server::{intent::ToIntentAddress, Server};
use std::{collections::HashMap, fs::read_dir, path::PathBuf};
use test_util::{hex_to_4_ints, parse_test_data, unwrap_or_continue};
use yansi::Color::Red;

// Arbitrary EOA user address. Eventually, this should be config parameter per test.
const EOA_ADDRESS: [i64; 4] = [1, 2, 3, 4];

#[test]
fn validation_e2e() -> anyhow::Result<()> {
    // Loop for each file or directory in the `sub_dir`.
    let dir: PathBuf = format!("validation_tests").into();
    let mut failed_tests = vec![];
    for entry in read_dir(dir)? {
        let entry = entry?;
        println!("Testing {}.", entry.path().display());

        // If it's a file it's expected to be a self contained Yurt script.  If it's a directory
        // then `main.yrt` must exist within and will be used.
        let mut path = entry.path();
        if entry.file_type()?.is_dir() {
            path.push("main.yrt");
        }

        // Produce the initial `IntermediateIntent`
        let parsed = unwrap_or_continue!(
            yurtc::parser::parse_project(&path),
            "parse yurt",
            failed_tests,
            path
        );

        // Parsed II -> Type-checked II
        let type_checked =
            unwrap_or_continue!(parsed.type_check(), "type check", failed_tests, path);

        // Type checked II -> Flattened II
        let flattened = unwrap_or_continue!(type_checked.flatten(), "flatten", failed_tests, path);

        //
        let persistent_intent = unwrap_or_continue!(
            yurtc::asm_gen::intent_to_asm(&flattened),
            "asm gen",
            failed_tests,
            path
        );

        // Parse the file for the expected results for Yurt parsing, optimising and final output,
        // or errors at any of those stages.
        let test_data = parse_test_data(&path)?;

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
            for var in &flattened.vars {
                decision_variables.extend(&decision_variables_map[&var.1.name]);
            }

            // Craft a solution, starting with the transitions for each intent.
            let transitions = [
                // solution data for the trivial transient intent
                SolutionData {
                    intent_to_solve: SourceAddress::transient(transient_intent_address.clone()),
                    // No decision variables
                    decision_variables: vec![],
                    sender: Sender::Eoa(EOA_ADDRESS),
                },
                // solution data for the deployed intent set. This includes some assignment
                // of all of its decision variables as well as specifying an input message
                SolutionData {
                    decision_variables,
                    intent_to_solve: SourceAddress::persistent(
                        deployed_set_address.clone().into(),
                        persistent_intent.intent_address(),
                    ),
                    sender: Sender::transient(EOA_ADDRESS, transient_intent_address),
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
            match server.submit_solution(solution) {
                Ok(2) => {}
                _ => failed_tests.push(path),
            };
        }
    }

    if !failed_tests.is_empty() {
        println!("Failed validation E2E tests");
        failed_tests.iter().for_each(|path: &std::path::PathBuf| {
            println!("{}", Red.paint(path.display().to_string()),)
        });
        panic!();
    }

    Ok(())
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
