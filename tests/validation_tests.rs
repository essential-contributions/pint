mod utils;

use essential_vm::types::{solution::SolutionSet, ContentAddress};
use pintc::predicate::CompileOptions;
use regex::Regex;
use std::collections::HashMap;
use std::{
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
    let re = Regex::new(r"<([^>]+)>").unwrap();
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
                .contract
                .predicates
                .iter()
                .map(essential_hash::content_addr),
            &compiled_contract.contract.salt,
        );

        // Prase the solution JSON
        let solution_file_name = path.with_extension("solution.json");
        let Ok(mut solution_str_from_file) = std::fs::read_to_string(solution_file_name.clone())
        else {
            anyhow::bail!(
                "test {} is missing a `*.solution.json` file",
                entry.path().display(),
            )
        };

        // Replace `<>` with the address of the contract.
        solution_str_from_file =
            solution_str_from_file.replace("<>", &format!("{}", contract_addr));

        // Replace `<PredicateName>` with the address of `PredicateName`
        let solution_str_from_file =
            re.replace_all(&solution_str_from_file, |caps: &regex::Captures| {
                let predicate_name = &caps[1];
                let index = compiled_contract
                    .names
                    .iter()
                    .position(|name| predicate_name == name)
                    .expect("predicate must exist");
                format!(
                    "{}",
                    essential_hash::content_addr(&compiled_contract.contract.predicates[index])
                )
            });

        let Ok(solution_set) = serde_json::from_str::<SolutionSet>(&solution_str_from_file) else {
            anyhow::bail!(
                "solution file {} file is not valid JSON",
                solution_file_name.display()
            )
        };

        if solution_set
            .solutions
            .iter()
            .all(|data| data.predicate_to_solve.contract != contract_addr)
        {
            anyhow::bail!(
                "solution set for test {} does not solve any predicates in the test!",
                entry.path().display()
            )
        }

        // Predicates to check are the ones that belong to our main contract
        let predicates_to_check = solution_set
            .solutions
            .iter()
            .enumerate()
            .filter(|&(_, data)| (data.predicate_to_solve.contract == contract_addr))
            .map(|(idx, data)| (idx, data.predicate_to_solve.predicate.clone()))
            .collect::<Vec<_>>();

        // Pre-populate the pre-state with all the db content, but first, every solved contract has
        // to be inserted.
        let mut pre_state = State::new(
            solution_set
                .solutions
                .iter()
                .map(|data| (data.predicate_to_solve.contract.clone(), vec![]))
                .collect(),
        );

        // Parse the db section in `pre_state`. This can include internal and external storage
        // addresses.
        parse_db_section(&path, &mut pre_state, &contract_addr)?;

        // Apply the state mutations to the state to produce the post state.
        let mut post_state = pre_state.clone();
        post_state.apply_mutations(&solution_set);

        let get_programs = Arc::new(
            compiled_contract
                .programs
                .iter()
                .map(|program| {
                    (
                        essential_hash::content_addr(program),
                        Arc::new(program.clone()),
                    )
                })
                .collect::<HashMap<_, _>>(),
        );

        // Now check each predicate in `predicates_to_check`
        for (idx, addr) in predicates_to_check {
            let predicate = compiled_contract
                .contract
                .predicates
                .iter()
                .find(|predicate| addr == essential_hash::content_addr(*predicate))
                .expect("predicate must exist");

            match essential_check::solution::check_predicate(
                &pre_state,
                &post_state,
                Arc::new(solution_set.clone()),
                Arc::new(predicate.clone()),
                &get_programs,
                idx as u16, // solution index
                &Default::default(),
            )
            .await
            {
                Ok(_) => {}
                Err(err) => {
                    println!(
                        "{}",
                        format!("    Error submitting solution set: {err}").red()
                    );
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
