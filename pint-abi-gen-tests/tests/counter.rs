use pint_abi::types::essential::{
    solution::{Mutation, Solution, SolutionSet},
    PredicateAddress,
};
use pint_abi_gen_tests::counter;
use std::sync::Arc;
use util::State;

mod util;

#[tokio::test]
async fn test_solution_increment() {
    tracing_subscriber::fmt::init();

    // Construct the package path.
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let manifest_dir_path = std::path::Path::new(&manifest_dir);
    let pkg_dir = manifest_dir_path.join("test-pkgs/counter");

    // Determine the content address of the contract.
    let contract_path = pkg_dir.join("out/debug/counter.json");
    let (contract, programs) = pint_abi::contract_from_path(&contract_path).unwrap();

    // Determine the predicate address by loading the ABI and finding the matching predicate.
    let abi_path = pkg_dir.join("out/debug/counter-abi.json");
    let abi = pint_abi::from_path(&abi_path).unwrap();
    let (pred, _pred_abi) = pint_abi::find_predicate(&contract, &abi, "Increment").unwrap();

    // Check the generated addresses are correct.
    let contract_ca = essential_hash::contract_addr::from_contract(&contract);
    let pred_addr = PredicateAddress {
        contract: contract_ca.clone(),
        predicate: essential_hash::content_addr(pred),
    };
    assert_eq!(contract_ca, counter::ADDRESS);
    assert_eq!(pred_addr, counter::Increment::ADDRESS);

    // The first increment is from `nil` to `1`.
    const INIT_VALUE: i64 = 1;
    let state_mutations: Vec<Mutation> = counter::storage::mutations().counter(INIT_VALUE).into();

    // Create the solution for solving `Increment` the first time.
    let solution = Solution {
        predicate_to_solve: counter::Increment::ADDRESS,
        predicate_data: vec![],
        state_mutations,
    };

    // Create the solution set.
    let solution_set = SolutionSet {
        solutions: vec![solution],
    };

    // Check the solution set is valid.
    essential_check::solution::check_set(&solution_set).unwrap();

    // Start with an empty pre-state.
    let mut state = State::new(vec![(counter::ADDRESS, vec![])]);

    // Create the post-state by applying the mutations.
    // TODO: do this directly in the pint contract instead
    state.apply_mutations(&solution_set);

    // Our `get_predicate` function can only return `Increment`.
    let predicate = Arc::new(pred.clone());
    let get_predicate = |_: &_| predicate.clone();
    let get_programs = Arc::new(
        programs
            .iter()
            .map(|program| {
                (
                    essential_hash::content_addr(program),
                    Arc::new(program.clone()),
                )
            })
            .collect::<std::collections::HashMap<_, _>>(),
    );

    // Default configuration.
    let config = Default::default();

    // Check our proposed mutations are valid against the contract.
    essential_check::solution::check_and_compute_solution_set_two_pass(
        &state,
        solution_set,
        get_predicate,
        get_programs,
        config,
    )
    .unwrap();
}
