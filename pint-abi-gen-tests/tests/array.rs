use pint_abi::types::essential::{
    solution::{Mutation, Solution, SolutionData},
    PredicateAddress,
};
use pint_abi_gen_tests::array;
use std::sync::Arc;
use util::State;

mod util;

#[tokio::test]
async fn test_array_solution_foo() {
    tracing_subscriber::fmt::init();

    // Construct the package path.
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let manifest_dir_path = std::path::Path::new(&manifest_dir);
    let pkg_dir = manifest_dir_path.join("test-pkgs/array");

    // Determine the content address of the contract.
    let contract_path = pkg_dir.join("out/debug/array.json");
    let contract = pint_abi::contract_from_path(&contract_path).unwrap();
    let contract_ca = essential_hash::contract_addr::from_contract(&contract);

    // Determine the predicate address by loading the ABI and finding the matching predicate.
    let abi_path = pkg_dir.join("out/debug/array-abi.json");
    let abi = pint_abi::from_path(&abi_path).unwrap();
    let (pred, _pred_abi) = pint_abi::find_predicate(&contract, &abi, "Foo").unwrap();
    let pred_ca = essential_hash::content_addr(pred);
    let pred_addr = PredicateAddress {
        contract: contract_ca.clone(),
        predicate: pred_ca,
    };

    // State mutations.
    let state_mutations: Vec<Mutation> = array::storage::mutations()
        // A 2-element array.
        .arr_0(|arr| arr.entry(0, 42).entry(1, 43))
        // A 10-element array.
        .arr_1(|arr| {
            [9; 10]
                .into_iter()
                .enumerate()
                .fold(arr, |arr, (i, v)| arr.entry(i, v))
        })
        // A 2D-array.
        .arr_2(|arr| {
            [[4, 4, 4], [2, 2, 2]]
                .into_iter()
                .enumerate()
                .fold(arr, |arr, (i, elems)| {
                    arr.entry(i, |arr| {
                        elems
                            .into_iter()
                            .enumerate()
                            .fold(arr, |arr, (i, v)| arr.entry(i, v))
                    })
                })
        })
        // A 3D-array.
        .arr_3(|arr| {
            [[[0, 0], [1, 1]], [[2, 2], [3, 3]], [[4, 4], [5, 5]]]
                .into_iter()
                .enumerate()
                .fold(arr, |arr, (i, vs)| {
                    arr.entry(i, |arr| {
                        vs.into_iter().enumerate().fold(arr, |arr, (i, vs)| {
                            arr.entry(i, |arr| {
                                vs.into_iter()
                                    .enumerate()
                                    .fold(arr, |arr, (i, v)| arr.entry(i, v))
                            })
                        })
                    })
                })
        })
        // An array of tuples.
        .arr_4(|arr| {
            arr.entry(0, |tup| tup._0(1)._1(2))
                .entry(1, |tup| tup._0(3)._1(4))
                .entry(2, |tup| tup._0(5)._1(6))
        })
        // A tuple of arrays.
        .tup_5(|tup| {
            tup._0(|arr| arr.entry(0, 0).entry(1, 1))
                ._1(|arr| arr.entry(0, 2).entry(1, 3))
                ._2(|arr| arr.entry(0, 4).entry(1, 5))
        })
        // An array of tuple of arrays.
        .arr_6(|arr| {
            arr.entry(0, |tup| {
                tup._0(|arr| arr.entry(0, 0).entry(1, 1))
                    ._1(|arr| arr.entry(0, 2).entry(1, 3))
            })
            .entry(1, |tup| {
                tup._0(|arr| arr.entry(0, 4).entry(1, 5))
                    ._1(|arr| arr.entry(0, 6).entry(1, 7))
            })
        })
        .into();

    // Create the solution data.
    let solution_data = SolutionData {
        predicate_to_solve: pred_addr,
        decision_variables: vec![],
        transient_data: vec![],
        state_mutations,
    };

    // Create the solution.
    let solution = Arc::new(Solution {
        data: vec![solution_data],
    });

    // Check the solution is valid.
    essential_check::solution::check(&solution).unwrap();

    // Start with an empty pre-state.
    let pre_state = State::new(vec![(contract_ca, vec![])]);

    // Create the post-state by applying the mutations.
    let mut post_state = pre_state.clone();
    post_state.apply_mutations(&solution);

    // Our `get_predicate` function can only return `Foo`.
    let predicate = Arc::new(pred.clone());
    let get_predicate = |_: &_| predicate.clone();

    // Default configuration.
    let config = Default::default();

    // Check our proposed mutations are valid against the contract.
    essential_check::solution::check_predicates(
        &pre_state,
        &post_state,
        solution,
        get_predicate,
        config,
    )
    .await
    .unwrap();
}
