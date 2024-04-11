use anyhow::anyhow;
use essential_types::{
    solution::{KeyMutation, Mutation, Sender, Solution, SolutionData, StateMutation},
    IntentAddress, SourceAddress,
};
use intent_server::{intent::ToIntentAddress, Server};
use std::{collections::BTreeMap, fs::read_dir, path::PathBuf};
use test_util::{
    bytes_to_hex, four_ints_to_hex, hex_to_bytes, hex_to_four_ints, parse_test_data,
    unwrap_or_continue,
};
use yansi::Color::Red;

#[test]
fn validation_e2e() -> anyhow::Result<()> {
    let mut server = Server::new();
    deploy(&mut server)?;
    submit_and_check_solution(&mut server)?;
    Ok(())
}

/// Deploy all persistent intents under `validation_tests/deployed`
fn deploy(server: &mut Server) -> anyhow::Result<()> {
    // Loop for each file or directory in the `sub_dir`.
    let dir: PathBuf = format!("validation_tests/deployed").into();
    let mut failed_tests = vec![];
    for entry in read_dir(dir)? {
        let entry = entry?;
        println!("Deploying {}.", entry.path().display());

        // If it's a file it's expected to be a self contained pint script.  If it's a directory
        // then `main.pnt` must exist within and will be used.
        let mut path = entry.path();
        if entry.file_type()?.is_dir() {
            path.push("main.pnt");
        }

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

        // Get the addresses of all produced persistent intents
        let intents_addresses = intents
            .intents
            .iter()
            .map(|(name, intent)| (name, intent.intent_address()))
            .collect::<BTreeMap<_, _>>();

        // Deploy the set of intents.
        let deployed_set_address = server
            .deploy_intent_set(intents.intents.values().cloned().collect())
            .expect("failed to deploy intent set!");

        // Populate the database of the server if a db section is specified
        if let Some(db) = &parse_test_data(&path)?.db {
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
                    hex_to_four_ints(&split[0]),
                    Some(split[1].parse::<i64>().expect("value must be a i64")),
                );
                server.db().commit();
            }
        }

        println!("  Set address: {}", four_ints_to_hex(deployed_set_address));
        println!("  Individual intent addresses:");
        intents_addresses
            .iter()
            .for_each(|(name, address)| println!("    {name}: {}", bytes_to_hex(address.0)));
    }

    if !failed_tests.is_empty() {
        println!("Failed deploying E2E tests");
        failed_tests.iter().for_each(|path: &std::path::PathBuf| {
            println!("{}", Red.paint(path.display().to_string()),)
        });
        panic!();
    }

    Ok(())
}

/// Submit every intent under `validation_tests/submitted` and check their solutions. The solution
/// files have the same name as the pint files but a `toml` extension.
fn submit_and_check_solution(server: &mut Server) -> anyhow::Result<()> {
    // Loop for each file or directory in the `sub_dir`.
    let dir: PathBuf = format!("validation_tests/submitted").into();
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

        println!("Submitting {}.", entry.path().display());

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

        // Extract the transient intent. There should be a single intent in `intents`.
        let mut intent = intents.root_intent().clone();

        // Enforce a single permit for now. This should change in the future
        intent.slots.permits = 1;

        let intent_address = intent.intent_address();

        // Submit the transient intent to the server
        server
            .submit_intent(intent)
            .expect("failed to submit transient intent to intent pool!");
        println!(
            "  Submitted intent address: {}",
            bytes_to_hex(intent_address.0.clone())
        );

        // Parse the solution `toml` file which must have the same name as the pint file but with a
        // `toml` extension.
        let solution = parse_solution(&path.with_extension("toml"))?;

        // check the solution for both intents. Expect a `1` for each intent, hence a
        // total utility of `2`.
        match server.submit_solution(solution) {
            Ok(2) => {}
            Ok(_) => println!("{}", Red.paint("    Validation failed")),
            Err(err) => {
                println!(
                    "{}",
                    Red.paint(format!("    Error submitting solution: {err}"))
                );
                failed_tests.push(path)
            }
        };
    }

    if !failed_tests.is_empty() {
        println!("Failed submitting and validating validation E2E tests");
        failed_tests.iter().for_each(|path: &std::path::PathBuf| {
            println!("{}", Red.paint(path.display().to_string()))
        });
        panic!();
    }

    Ok(())
}

/// Parse a `toml` file into a `Solution`
fn parse_solution(path: &std::path::Path) -> anyhow::Result<Solution> {
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
                        d.as_integer()
                            .ok_or_else(|| anyhow!("Invalid integer value in decision_variables"))
                    })
                    .collect::<anyhow::Result<Vec<_>>>()?;

                // The sender is either an `Eoa` or a `Transient`
                let sender = match e.get("sender") {
                    Some(sender) => match (sender.get("Eoa"), sender.get("Transient")) {
                        (Some(eoa), None) => Sender::Eoa(hex_to_four_ints(
                            &eoa.as_str().ok_or_else(|| anyhow!("Invalid eoa sender"))?,
                        )),
                        (None, Some(transient)) => Sender::transient(
                            hex_to_four_ints(
                                &transient
                                    .get("eoa")
                                    .and_then(|eoa| eoa.as_str())
                                    .ok_or_else(|| anyhow!("Invalid eoa for transient sender"))?,
                            ),
                            IntentAddress(hex_to_bytes(
                                &transient
                                    .get("intent")
                                    .and_then(|intent| intent.as_str())
                                    .ok_or_else(|| {
                                        anyhow!("Invalid intent for transient sender")
                                    })?,
                            )),
                        ),
                        _ => return Err(anyhow!("Invalid sender")),
                    },
                    None => return Err(anyhow!("'sender' field is missing")),
                };

                // The intent to solve is either `Transient` or `Persistent`
                let intent_to_solve = match e.get("intent_to_solve") {
                    Some(intent_to_solve) => match (
                        intent_to_solve.get("Transient"),
                        intent_to_solve.get("Persistent"),
                    ) {
                        (Some(s), None) => SourceAddress::transient(IntentAddress(hex_to_bytes(
                            &s.as_str()
                                .ok_or_else(|| anyhow!("Invalid transient intent_to_solve"))?,
                        ))),
                        (None, Some(s)) => SourceAddress::persistent(
                            IntentAddress(hex_to_bytes(
                                &s.get("set").and_then(|set| set.as_str()).ok_or_else(|| {
                                    anyhow!("Invalid persistent intent_to_solve set")
                                })?,
                            )),
                            IntentAddress(hex_to_bytes(
                                &s.get("intent")
                                    .and_then(|intent| intent.as_str())
                                    .ok_or_else(|| {
                                        anyhow!("Invalid persistent intent_to_solve intent")
                                    })?,
                            )),
                        ),
                        _ => return Err(anyhow!("Invalid intent_to_solve")),
                    },
                    None => return Err(anyhow!("'intent_to_solve' field is missing")),
                };

                Ok(SolutionData {
                    decision_variables,
                    sender,
                    intent_to_solve,
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
                let address = e
                    .get("address")
                    .and_then(|addr| addr.as_str())
                    .ok_or_else(|| anyhow!("'address' field is missing or not a string"))?;
                let mutations = e
                    .get("mutations")
                    .and_then(|muta| muta.as_array())
                    .ok_or_else(|| anyhow!("'mutations' field is missing or not an array"))?
                    .iter()
                    .map(|mutation| {
                        Ok(Mutation::Key(KeyMutation {
                            key: hex_to_four_ints(
                                mutation
                                    .get("key")
                                    .and_then(|key| key.as_str())
                                    .ok_or_else(|| anyhow!("Invalid mutation key"))?,
                            ),
                            value: mutation.get("value").and_then(|val| val.as_integer()),
                        }))
                    })
                    .collect::<anyhow::Result<Vec<_>>>()?;
                Ok(StateMutation {
                    address: IntentAddress(hex_to_bytes(address)),
                    mutations,
                })
            })
            .collect::<anyhow::Result<Vec<_>>>()?,
        None => Vec::new(),
    };

    Ok(Solution {
        data,
        state_mutations,
    })
}
