mod utils;

use anyhow::anyhow;
use essential_constraint_vm::mut_keys_set;
use essential_state_read_vm::{
    asm::{self, Op},
    constraint,
    types::{
        solution::{DecisionVariable, Mutation, Solution, SolutionData, StateMutation},
        ContentAddress, IntentAddress,
    },
    Access, BytecodeMapped, GasLimit, SolutionAccess, StateSlots, Vm,
};
use std::{fs::read_dir, path::PathBuf};
use test_util::{hex_to_bytes, hex_to_four_ints, parse_test_data, unwrap_or_continue};
use utils::*;
use yansi::Color::Red;

#[tokio::test]
async fn validation_e2e() -> anyhow::Result<()> {
    let dir: PathBuf = "validation_tests".to_string().into();
    let mut failed_tests = vec![];
    for entry in read_dir(dir)? {
        let entry = entry?;

        // If it's a file it's expected to be a self contained pint script.  If it's a directory
        // then `main.pnt` must exist within and will be used.
        let mut path = entry.path();
        if entry.file_type()?.is_dir() {
            path.push("main.pnt");
        }

        // Only go over pint file
        if path.extension().unwrap() != "pnt" {
            continue;
        }

        println!("Testing {}.", entry.path().display());

        // Error handler
        let handler = pintc::error::Handler::default();

        // Produce the initial parsed program
        let parsed = unwrap_or_continue!(
            pintc::parser::parse_project(&handler, &path),
            "parse pint",
            failed_tests,
            path,
            handler
        );

        // Parsed program -> Flattened program
        let flattened = unwrap_or_continue!(
            parsed.compile(&handler),
            "compile",
            failed_tests,
            path,
            handler
        );

        // Flattened program -> Assembly (aka collection of Intents)
        let intents = unwrap_or_continue!(
            pintc::asm_gen::program_to_intents(&handler, &flattened),
            "asm gen",
            failed_tests,
            path,
            handler
        );

        // Parse a solution file
        let solution = parse_solution(&path.with_extension("toml"), &intents)?;

        // We're only going to verify the intent in the first solution data, which we assume
        // corresponds to one of the intents in `intents` produce above. All other solution data
        // correspond to external intents that we're not going to verify here at this point.
        let intent_to_check_index = 0; // Only the first one!
        let intent_to_check = &solution.data[intent_to_check_index].intent_to_solve;

        // This is the access that contains an access to some solution data and will contain the
        // pre and post states.
        let mutable_keys = mut_keys_set(&solution, intent_to_check_index as u16);
        let mut access = Access {
            solution: SolutionAccess::new(&solution, intent_to_check_index as u16, &mutable_keys),
            state_slots: StateSlots::EMPTY,
        };

        // Find the individual intent that corresponds to the intent address specified in
        // `intent_to_verify`. Here, we assume that the last byte in the address matches the
        // index of the intent in in the BTreeMap `intents.intents`.
        let intent = intents
            .intents
            .iter()
            .nth(intent_to_check.intent.0[31] as usize)
            .unwrap()
            .1;

        // Pre-populate the pre-state with all the db content, but first, every solution data
        // intent set has to be inserted.
        let mut pre_state = State::new(
            solution
                .data
                .iter()
                .map(|data| (data.intent_to_solve.set.clone(), vec![]))
                .collect(),
        );

        // Parse the db section. This can include internal and external storage addresses.
        if let Some(db) = &parse_test_data(&path)?.db {
            for line in db.lines() {
                // Collect key and value. Assume the key is a hex and the value is a u64
                let split = line.split_ascii_whitespace().collect::<Vec<_>>();
                if split.len() == 2 {
                    // Internal address
                    pre_state.set(
                        intent_to_check.set.clone(),
                        &hex_to_four_ints(split[0]),
                        Some(split[1].parse::<i64>().expect("value must be a i64")),
                    );
                } else if split.len() == 3 {
                    // External address
                    pre_state.set(
                        ContentAddress(hex_to_bytes(split[0])),
                        &hex_to_four_ints(split[1]),
                        Some(split[2].parse::<i64>().expect("value must be a i64")),
                    );
                } else {
                    panic!("Error parsing db section");
                }
            }
        }

        // Produce the pre state slots by running all the state read programs
        let mut pre_state_slots = vec![];
        for idx in 0..intent.state_read.len() {
            let mut vm = Vm::default();
            let state_read_ops: Vec<_> = asm::from_bytes(intent.state_read[idx].iter().copied())
                .collect::<Result<BytecodeMapped, _>>()
                .expect("expecting valid state read bytecode")
                .ops()
                .collect();

            vm.exec_ops(
                &state_read_ops[..],
                access,
                &pre_state,
                &|_: &Op| 1,
                GasLimit::UNLIMITED,
            )
            .await
            .unwrap();

            pre_state_slots.extend(vm.into_state_slots());
        }

        // Apply the state mutations to the state to produce the post state.
        let mut post_state = pre_state.clone();
        for mutation in &solution.state_mutations {
            let solution_data = &solution.data[usize::from(mutation.pathway)];
            let set_addr = &solution_data.intent_to_solve.set;
            for Mutation { key, value } in mutation.mutations.iter() {
                post_state.set(set_addr.clone(), key, *value);
            }
        }

        // Produce the post state slots by running all the state read programs using `post_state`
        let mut post_state_slots = vec![];
        for idx in 0..intent.state_read.len() {
            let mut vm = Vm::default();
            let ops: Vec<_> = asm::from_bytes(intent.state_read[idx].iter().copied())
                .collect::<Result<BytecodeMapped, _>>()
                .unwrap()
                .ops()
                .collect();

            vm.exec_ops(&ops, access, &post_state, &|_: &Op| 1, GasLimit::UNLIMITED)
                .await
                .unwrap();

            post_state_slots.extend(vm.into_state_slots());
        }

        // Now, set the state slots in `access` and verify them by running the constraints
        access.state_slots = StateSlots {
            pre: &pre_state_slots[..],
            post: &post_state_slots[..],
        };

        match constraint::check_intent(&intent.constraints, access) {
            Ok(_) => {}
            Err(err) => {
                println!(
                    "{}",
                    Red.paint(format!("    Error submitting solution: {err}"))
                );
                failed_tests.push(path)
            }
        }
    }

    if !failed_tests.is_empty() {
        println!("Failed validating validation E2E tests");
        failed_tests.iter().for_each(|path: &std::path::PathBuf| {
            println!("{}", Red.paint(path.display().to_string()))
        });
        panic!();
    }

    Ok(())
}

/// Parse a `toml` file into a `Solution`
fn parse_solution(
    path: &std::path::Path,
    intents: &pintc::asm_gen::Intents,
) -> anyhow::Result<Solution> {
    let toml_content_str = std::fs::read_to_string(path)?;
    let toml_content = toml_content_str.parse::<toml::Value>()?;

    let data = match toml_content.get("data") {
        Some(data) => data
            .as_array()
            .unwrap_or(&Vec::new())
            .iter()
            .map(|e| {
                // Decision variables are in a list of integers
                let decision_variables = e
                    .get("decision_variables")
                    .and_then(|dv| dv.as_array())
                    .unwrap_or(&Vec::new())
                    .iter()
                    .map(|d| match d.get("inline") {
                        Some(d) => d
                            .as_integer()
                            .map(DecisionVariable::Inline)
                            .ok_or_else(|| anyhow!("Invalid integer value in decision_variables")),
                        None => Err(anyhow!("'inline' field is missing")),
                    })
                    .collect::<anyhow::Result<Vec<_>>>()?;

                // The intent to solve is either `Transient` or `Persistent`
                let intent_to_solve = match e.get("intent_to_solve") {
                    Some(s) => IntentAddress {
                        set: ContentAddress(hex_to_bytes(
                            s.get("set")
                                .and_then(|set| set.as_str())
                                .ok_or_else(|| anyhow!("Invalid persistent intent_to_solve set"))?,
                        )),
                        intent: match s.get("intent") {
                            // Here, we convert the intent name into an address that is equal to
                            // the index of the intent in the set of intents. This just a way to
                            // later figure out what constraints we have to check.
                            Some(intent) => {
                                let index = intents
                                    .intents
                                    .iter()
                                    .position(|(k, _)| k == intent.as_str().unwrap())
                                    .unwrap_or_default();
                                let mut bytes: [u8; 32] = [0; 32];
                                bytes[31] = index as u8;
                                ContentAddress(bytes)
                            }
                            None => return Err(anyhow!("Invalid persistent intent_to_solve set")),
                        },
                    },
                    None => return Err(anyhow!("'intent_to_solve' field is missing")),
                };

                Ok(SolutionData {
                    intent_to_solve,
                    decision_variables,
                })
            })
            .collect::<anyhow::Result<Vec<_>>>()?,
        None => Vec::new(),
    };

    let state_mutations = match toml_content.get("state_mutations") {
        Some(mutations) => mutations
            .as_array()
            .unwrap_or(&Vec::new())
            .iter()
            .map(|e| {
                let pathway = e
                    .get("pathway")
                    .and_then(|addr| addr.as_integer())
                    .ok_or_else(|| anyhow!("'pathway' field is missing or not an integer"))?
                    as u16;
                let mutations = e
                    .get("mutations")
                    .and_then(|muta| muta.as_array())
                    .ok_or_else(|| anyhow!("'mutations' field is missing or not an array"))?
                    .iter()
                    .map(|mutation| {
                        Ok(Mutation {
                            key: hex_to_four_ints(
                                mutation
                                    .get("key")
                                    .and_then(|key| key.as_str())
                                    .ok_or_else(|| anyhow!("Invalid mutation key"))?,
                            ),
                            value: mutation.get("value").and_then(|val| val.as_integer()),
                        })
                    })
                    .collect::<anyhow::Result<Vec<_>>>()?;
                Ok(StateMutation { pathway, mutations })
            })
            .collect::<anyhow::Result<Vec<_>>>()?,
        None => Vec::new(),
    };

    Ok(Solution {
        data,
        state_mutations,
        partial_solutions: vec![],
    })
}
