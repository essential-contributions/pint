use pint_abi::types::essential::{
    solution::{Mutation, Solution, SolutionSet},
    Key, PredicateAddress, Value,
};
use pint_abi_gen_tests::optional;
use std::sync::Arc;
use util::State;

mod util;

// Test that we can trivially create a full `Solution` for the optional contract's `Foo` predicate.
// Set the values to adhere to the arbitrary constraints of the predicate.
#[tokio::test]
async fn test_solution_foo() {
    tracing_subscriber::fmt::init();

    // Construct the package path.
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let manifest_dir_path = std::path::Path::new(&manifest_dir);
    let pkg_dir = manifest_dir_path.join("test-pkgs/optional");

    // Determine the content address of the contract.
    let contract_path = pkg_dir.join("out/debug/optional.json");
    let (contract, programs) = pint_abi::contract_from_path(&contract_path).unwrap();

    // Determine the predicate address by loading the ABI and finding the matching predicate.
    let abi_path = pkg_dir.join("out/debug/optional-abi.json");
    let abi = pint_abi::from_path(&abi_path).unwrap();
    let (pred, _pred_abi) = pint_abi::find_predicate(&contract, &abi, "Foo").unwrap();

    // Check the generated addresses are correct.
    let contract_ca = essential_hash::contract_addr::from_contract(&contract);
    let pred_addr = PredicateAddress {
        contract: contract_ca.clone(),
        predicate: essential_hash::content_addr(pred),
    };
    assert_eq!(contract_ca, optional::ADDRESS);
    assert_eq!(pred_addr, optional::Foo::ADDRESS);

    // Decision variables.
    let vars = optional::Foo::Args {
        x: Some(42),
        y: Some(69),
        z: None,
        t: Some((Some(99), true)),
        t_nil: None,
        a1: Some([[1, 2, 3], [4, 5, 6]]),
        a2: [[Some(1), Some(2), Some(3)], [Some(4), Some(5), Some(6)]],
        e1: Some(optional::E::A(Some(98))),
        e2: Some(optional::E::C([9, 10, 11])),
    };

    // State mutations.
    let state_mutations: Vec<Mutation> = optional::storage::mutations()
        .x(Some(42))
        .y(Some(69))
        .z(None)
        .t(Some((Some(99), true)))
        .t_nil(None)
        .a1(Some([[1, 2, 3], [4, 5, 6]]))
        .a2(|arr| {
            [[Some(1), Some(2), Some(3)], [Some(4), Some(5), Some(6)]]
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
        .e1(Some(optional::E::A(Some(98))))
        .e2(Some(optional::E::C([9, 10, 11])))
        .into();

    // Build the same set of keys, so we can ensure they match the mutations.
    let keys: Vec<Key> = optional::storage::keys()
        .x()
        .y()
        .z()
        .t()
        .t_nil()
        .a1()
        .a2(|arr| {
            [[Some(1), Some(2), Some(3)], [Some(4), Some(5), Some(6)]]
                .into_iter()
                .enumerate()
                .fold(arr, |arr, (i, elems)| {
                    arr.entry(i, |arr| (0..elems.len()).fold(arr, |arr, i| arr.entry(i)))
                })
        })
        .e1()
        .e2()
        .into();

    // Check keys match the mutation keys.
    for (key, mutation) in keys.iter().zip(&state_mutations) {
        assert_eq!(key, &mutation.key);
    }

    // Check Encoding/Decoding roundtrip for decision vars.
    let words = pint_abi::encode(&vars);
    let vars2: optional::Foo::Args = pint_abi::decode(&words[..]).unwrap();
    assert_eq!(&vars, &vars2);

    // Check To/From Vec<Value> roundtrip.
    let values: Vec<Value> = vars.clone().into();
    let vars3 = optional::Foo::Args::try_from(&values[..]).unwrap();
    assert_eq!(&vars, &vars3);

    // Create the solution for predicate `Foo`.
    let solution = Solution {
        predicate_to_solve: optional::Foo::ADDRESS,
        predicate_data: vars.into(),
        state_mutations,
    };

    // Create the solution set.
    let solution_set = Arc::new(SolutionSet {
        solutions: vec![solution],
    });

    // Check the solution set is valid.
    essential_check::solution::check_set(&solution_set).unwrap();

    // Start with an empty pre-state.
    let pre_state = State::new(vec![(optional::ADDRESS, vec![])]);

    // Create the post-state by applying the mutations.
    let mut post_state = pre_state.clone();
    post_state.apply_mutations(&solution_set);

    // Our `get_predicate` function can only return `Foo`.
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
    essential_check::solution::check_set_predicates(
        &pre_state,
        &post_state,
        solution_set,
        get_predicate,
        get_programs,
        config,
    )
    .await
    .unwrap();
}
