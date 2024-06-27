mod utils;

use anyhow::anyhow;
use essential_constraint_vm::mut_keys_set;
use essential_state_read_vm::{
    asm::{self, Op},
    constraint,
    types::{
        solution::{Mutation, Solution, SolutionData},
        ContentAddress, PredicateAddress,
    },
    Access, BytecodeMapped, GasLimit, SolutionAccess, StateSlots, Vm,
};
use std::{
    fs::{read_dir, File},
    io::{BufRead, BufReader},
    path::PathBuf,
};
use test_util::{hex_to_bytes, parse_test_data, unwrap_or_continue};
use utils::*;
use yansi::Paint;

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

        // `<disabled>` disables tests from running completely
        let handle = File::open(path.clone())?;
        if let Some(Ok(first_line)) = BufReader::new(handle).lines().next() {
            // Skip disabled tests.
            if first_line.contains("<disabled>") {
                continue;
            }
        }

        println!("Testing {}.", entry.path().display());

        // Error handler
        let handler = pintc::error::Handler::default();

        // These tests have no dependencies.
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
        let flattened = unwrap_or_continue!(
            parsed.compile(&handler),
            "compile",
            failed_tests,
            path,
            handler
        );

        // Flattened program -> Assembly (aka collection of compiled predicates)
        let compiled_program = unwrap_or_continue!(
            pintc::asm_gen::compile_program(&handler, &flattened),
            "asm gen",
            failed_tests,
            path,
            handler
        );

        // Parse a solution file
        let solution = parse_solution(&path.with_extension("toml"), &compiled_program)?;

        // We're only going to verify the predicate in the first solution data, which we assume
        // corresponds to one of the predicates in `compiled_program` produce above. All other
        // solution data correspond to external predicate that we're not going to verify here at
        // this point.
        let predicate_to_check_index = 0; // Only the first one!
        let predicate_to_check = &solution.data[predicate_to_check_index].predicate_to_solve;
        let transient_data = essential_constraint_vm::transient_data(&solution);

        // This is the access that contains an access to some solution data and will contain the
        // pre and post states.
        let mutable_keys = mut_keys_set(&solution, predicate_to_check_index as u16);
        let mut access = Access {
            solution: SolutionAccess::new(
                &solution,
                predicate_to_check_index as u16,
                &mutable_keys,
                &transient_data,
            ),
            state_slots: StateSlots::EMPTY,
        };

        // Find the individual predicate that corresponds to the predicate address specified in
        // `predicate_to_verify`. Here, we assume that the last byte in the address matches the
        // index of the predicate in in the BTreeMap `compiled_program.predicates`.
        let predicate = &compiled_program.predicates[predicate_to_check.predicate.0[31] as usize];

        // Pre-populate the pre-state with all the db content, but first, every solution data
        // predicate set has to be inserted.
        let mut pre_state = State::new(
            solution
                .data
                .iter()
                .map(|data| (data.predicate_to_solve.contract.clone(), vec![]))
                .collect(),
        );

        // Parse the db section. This can include internal and external storage addresses.
        if let Some(db) = &parse_test_data(&path)?.db {
            for line in db.lines() {
                // Collect key and value. Assume the key is a hex and the value is a u64
                let split = line.split(',').collect::<Vec<_>>();
                let (set_address, key, value) = if split.len() == 3 {
                    (ContentAddress(hex_to_bytes(split[0])), split[1], split[2])
                } else if split.len() == 2 {
                    (predicate_to_check.contract.clone(), split[0], split[1])
                } else {
                    panic!("Error parsing db section");
                };

                pre_state.set(
                    set_address,
                    &key.split_ascii_whitespace()
                        .map(|k| k.parse::<i64>().expect("value must be a i64"))
                        .collect::<Vec<_>>(),
                    value
                        .split_ascii_whitespace()
                        .map(|k| k.parse::<i64>().expect("value must be a i64"))
                        .collect::<Vec<_>>(),
                );
            }
        }

        // Produce the pre state slots by running all the state read programs
        let mut pre_state_slots = vec![];
        for idx in 0..predicate.state_read.len() {
            let mut vm = Vm::default();
            let state_read_ops: Vec<_> = asm::from_bytes(predicate.state_read[idx].iter().copied())
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
            .unwrap_or_else(|_| {
                failed_tests.push(path.clone());
                0
            });

            pre_state_slots.extend(vm.into_state_slots());
        }

        // Apply the state mutations to the state to produce the post state.
        let mut post_state = pre_state.clone();
        for data in &solution.data {
            let set_addr = &data.predicate_to_solve.contract;
            for Mutation { key, value } in &data.state_mutations {
                post_state.set(set_addr.clone(), key, value.clone());
            }
        }

        // Produce the post state slots by running all the state read programs using `post_state`
        let mut post_state_slots = vec![];
        for idx in 0..predicate.state_read.len() {
            let mut vm = Vm::default();
            let ops: Vec<_> = asm::from_bytes(predicate.state_read[idx].iter().copied())
                .collect::<Result<BytecodeMapped, _>>()
                .unwrap()
                .ops()
                .collect();

            vm.exec_ops(&ops, access, &post_state, &|_: &Op| 1, GasLimit::UNLIMITED)
                .await
                .unwrap_or_else(|_| {
                    failed_tests.push(path.clone());
                    0
                });

            post_state_slots.extend(vm.into_state_slots());
        }

        // Now, set the state slots in `access` and verify them by running the constraints
        access.state_slots = StateSlots {
            pre: &pre_state_slots[..],
            post: &post_state_slots[..],
        };

        match constraint::check_predicate(&predicate.constraints, access) {
            Ok(_) => {}
            Err(err) => {
                println!("{}", format!("    Error submitting solution: {err}").red());
                failed_tests.push(path)
            }
        }
    }

    if !failed_tests.is_empty() {
        println!("Failed validating validation E2E tests");
        failed_tests
            .iter()
            .for_each(|path: &std::path::PathBuf| println!("{}", path.display().to_string().red()));
        panic!();
    }

    Ok(())
}

/// Parse a `toml` file into a `Solution`
fn parse_solution(
    path: &std::path::Path,
    compiled_program: &pintc::asm_gen::CompiledProgram,
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
                    .map(|d| {
                        d.as_array()
                            .unwrap_or(&Vec::new())
                            .iter()
                            .map(|d| {
                                d.as_integer().ok_or_else(|| {
                                    anyhow!("Invalid integer value in list of decision variables")
                                })
                            })
                            .collect::<anyhow::Result<Vec<_>, _>>()
                    })
                    .collect::<anyhow::Result<Vec<_>, _>>()?;

                // The predicate to solve is either `Transient` or `Persistent`
                let predicate_to_solve = match e.get("predicate_to_solve") {
                    Some(s) => PredicateAddress {
                        contract: ContentAddress(hex_to_bytes(
                            s.get("set").and_then(|set| set.as_str()).ok_or_else(|| {
                                anyhow!("Invalid persistent predicate_to_solve set")
                            })?,
                        )),
                        predicate: match s.get("predicate") {
                            // Here, we convert the predicate name into an address that is equal to
                            // the index of the predicate in the contract. This just a way to later
                            // figure out what constraints we have to check.
                            Some(predicate) => {
                                let index = compiled_program
                                    .names
                                    .iter()
                                    .position(|name| name == predicate.as_str().unwrap())
                                    .unwrap_or_default();
                                let mut bytes: [u8; 32] = [0; 32];
                                bytes[31] = index as u8;
                                ContentAddress(bytes)
                            }
                            None => {
                                return Err(anyhow!("Invalid persistent predicate_to_solve set"))
                            }
                        },
                    },
                    None => return Err(anyhow!("'predicate_to_solve' field is missing")),
                };

                let state_mutations = e
                    .get("state_mutations")
                    .and_then(|muta| muta.as_array())
                    .unwrap_or(&Vec::new())
                    .iter()
                    .map(|mutation| {
                        Ok(Mutation {
                            key: mutation
                                .get("key")
                                .and_then(|word| word.as_array())
                                .unwrap_or(&Vec::new())
                                .iter()
                                .map(|d| {
                                    d.as_integer().ok_or_else(|| {
                                        anyhow!("Invalid integer value in state mutation key")
                                    })
                                })
                                .collect::<anyhow::Result<Vec<_>, _>>()?,
                            value: mutation
                                .get("value")
                                .and_then(|word| word.as_array())
                                .unwrap_or(&Vec::new())
                                .iter()
                                .map(|d| {
                                    d.as_integer().ok_or_else(|| {
                                        anyhow!("Invalid integer value in state mutation word")
                                    })
                                })
                                .collect::<anyhow::Result<Vec<_>, _>>()?,
                        })
                    })
                    .collect::<anyhow::Result<Vec<_>>>()?;

                let transient_data = e
                    .get("transient_data")
                    .and_then(|muta| muta.as_array())
                    .unwrap_or(&Vec::new())
                    .iter()
                    .map(|mutation| {
                        Ok(Mutation {
                            key: mutation
                                .get("key")
                                .and_then(|word| word.as_array())
                                .unwrap_or(&Vec::new())
                                .iter()
                                .map(|d| {
                                    d.as_integer().ok_or_else(|| {
                                        anyhow!("Invalid integer value in state mutation key")
                                    })
                                })
                                .collect::<anyhow::Result<Vec<_>, _>>()?,
                            value: mutation
                                .get("value")
                                .and_then(|word| word.as_array())
                                .unwrap_or(&Vec::new())
                                .iter()
                                .map(|d| {
                                    d.as_integer().ok_or_else(|| {
                                        anyhow!("Invalid integer value in state mutation word")
                                    })
                                })
                                .collect::<anyhow::Result<Vec<_>, _>>()?,
                        })
                    })
                    .collect::<anyhow::Result<Vec<_>>>()?;

                Ok(SolutionData {
                    predicate_to_solve,
                    decision_variables,
                    state_mutations,
                    transient_data,
                })
            })
            .collect::<anyhow::Result<Vec<_>>>()?,
        None => Vec::new(),
    };

    Ok(Solution { data })
}
