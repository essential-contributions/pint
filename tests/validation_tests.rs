mod utils;

use anyhow::anyhow;
use essential_state_read_vm::types::{
    solution::{Mutation, Solution, SolutionData},
    ContentAddress, PredicateAddress,
};
use pintc::predicate::CompileOptions;
use std::{
    collections::HashMap,
    fs::{read_dir, File},
    io::{BufRead, BufReader},
    path::PathBuf,
    sync::Arc,
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

        // Produce the initial parsed contract
        let parsed = unwrap_or_continue!(
            pintc::parser::parse_project(&handler, &deps, &path),
            "parse pint",
            failed_tests,
            path,
            handler
        );

        // Parsed contract -> Flattened contract
        let flattened = unwrap_or_continue!(
            parsed.compile(
                &handler,
                CompileOptions {
                    skip_optimize: false,
                    print_flat: false,
                }
            ),
            "compile",
            failed_tests,
            path,
            handler
        );

        // Flattened contract -> Assembly (aka collection of compiled predicates)
        let compiled_contract = unwrap_or_continue!(
            pintc::asm_gen::compile_contract(&handler, Default::default(), &flattened),
            "asm gen",
            failed_tests,
            path,
            handler
        );

        let contract_addr = essential_hash::contract_addr::from_predicate_addrs(
            compiled_contract
                .predicates
                .iter()
                .map(essential_hash::content_addr),
            &compiled_contract.salt,
        );

        let solution = parse_solution(
            &path.with_extension("toml"),
            &compiled_contract,
            &contract_addr,
        )?;

        // Predicates to check are the ones that belong to our main contract
        let predicates_to_check = solution
            .data
            .iter()
            .enumerate()
            .filter(|&(_, data)| (data.predicate_to_solve.contract == contract_addr))
            .map(|(idx, data)| (idx, data.predicate_to_solve.predicate.clone()))
            .collect::<Vec<_>>();

        // Pre-populate the pre-state with all the db content, but first, every solution data
        // predicate set has to be inserted.
        let mut pre_state = State::new(
            solution
                .data
                .iter()
                .map(|data| (data.predicate_to_solve.contract.clone(), vec![]))
                .collect(),
        );

        // Parse the db section in `pre_state`. This can include internal and external storage
        // addresses.
        parse_db_section(&path, &mut pre_state, &contract_addr)?;

        // Apply the state mutations to the state to produce the post state.
        let mut post_state = pre_state.clone();
        post_state.apply_mutations(&solution);

        // Now check each predicate in `predicates_to_check`
        for (idx, addr) in predicates_to_check {
            let predicate = compiled_contract
                .predicates
                .iter()
                .find(|predicate| addr == essential_hash::content_addr(*predicate))
                .expect("predicate must exist");

            match essential_check::solution::check_predicate(
                &pre_state,
                &post_state,
                Arc::new(solution.clone()),
                Arc::new(predicate.clone()),
                idx as u16, // solution data index
                &Default::default(),
            )
            .await
            {
                Ok(_) => {}
                Err(err) => {
                    println!("{}", format!("    Error submitting solution: {err}").red());
                    failed_tests.push(path.clone());
                    break;
                }
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
    compiled_contract: &pintc::asm_gen::CompiledContract,
    contract_addr: &ContentAddress,
) -> anyhow::Result<Solution> {
    let toml_content_str = std::fs::read_to_string(path)?;
    let toml_content = toml_content_str.parse::<toml::Value>()?;

    let names_to_predicates = compiled_contract
        .names
        .iter()
        .zip(compiled_contract.predicates.iter())
        .map(|(name, predicate)| (name.clone(), predicate))
        .collect::<HashMap<_, _>>();

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

                let predicate_to_solve = match e.get("predicate_to_solve") {
                    Some(s) => PredicateAddress {
                        contract: {
                            if let Some(contract) = s.get("set") {
                                ContentAddress(hex_to_bytes(contract.as_str().ok_or_else(
                                    || anyhow!("Invalid persistent predicate_to_solve set"),
                                )?))
                            } else {
                                contract_addr.clone()
                            }
                        },
                        predicate: match s.get("predicate") {
                            // Here, we convert the predicate name into an address that is equal to
                            // the index of the predicate in the contract. This just a way to later
                            // figure out what constraints we have to check.
                            Some(predicate) => {
                                if let Some(predicate) =
                                    names_to_predicates.get(predicate.as_str().unwrap())
                                {
                                    essential_hash::content_addr(*predicate)
                                } else {
                                    ContentAddress(hex_to_bytes(predicate.as_str().ok_or_else(
                                        || anyhow!("Invalid persistent predicate_to_solve set"),
                                    )?))
                                }
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

                Ok(SolutionData {
                    predicate_to_solve,
                    decision_variables,
                    state_mutations,
                })
            })
            .collect::<anyhow::Result<Vec<_>>>()?,
        None => Vec::new(),
    };

    Ok(Solution { data })
}

fn parse_db_section(
    path: &std::path::Path,
    pre_state: &mut State,
    contract_addr: &ContentAddress,
) -> anyhow::Result<()> {
    // Parse the db section. This can include internal and external storage addresses.
    if let Some(db) = &parse_test_data(path, true /* db_only */)?.db {
        for line in db.lines() {
            // Collect key and value. Assume the key is a hex and the value is a u64
            let split = line.split(',').collect::<Vec<_>>();
            let (contract_addr, key, value) = if split.len() == 3 {
                // External key
                (ContentAddress(hex_to_bytes(split[0])), split[1], split[2])
            } else if split.len() == 2 {
                // Internal key
                (contract_addr.clone(), split[0], split[1])
            } else {
                panic!("Error parsing db section");
            };

            pre_state.set(
                contract_addr,
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

    Ok(())
}
