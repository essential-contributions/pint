use pint_abi::types::essential::{
    solution::{Mutation, Solution, SolutionData},
    PredicateAddress,
};
use pint_abi_gen_tests::simple;
use std::sync::Arc;
use util::State;

mod util;

// Test that we can trivially create a full `SolutionData` for the simple
// contract's `Foo` predicate. Set the values to adhere to the arbitrary
// constraints of the predicate.
#[tokio::test]
async fn test_solution_foo() {
    tracing_subscriber::fmt::init();

    // Construct the package path.
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let manifest_dir_path = std::path::Path::new(&manifest_dir);
    let pkg_dir = manifest_dir_path.join("test-pkgs/simple");

    // Determine the content address of the contract.
    let contract_path = pkg_dir.join("out/debug/simple.json");
    let contract = pint_abi::contract_from_path(&contract_path).unwrap();
    let contract_ca = essential_hash::contract_addr::from_contract(&contract);

    // Determine the predicate address by loading the ABI and finding the matching predicate.
    let abi_path = pkg_dir.join("out/debug/simple-abi.json");
    let abi = pint_abi::from_path(&abi_path).unwrap();
    let (pred, _pred_abi) = pint_abi::find_predicate(&contract, &abi, "Foo").unwrap();
    let pred_ca = essential_hash::content_addr(pred);
    let pred_addr = PredicateAddress {
        contract: contract_ca.clone(),
        predicate: pred_ca,
    };

    // Decision variables.
    let vars = simple::Foo::Vars {
        v0: true,
        v1: 42,
        v2: [0x1111111100000000; 4],
        v3: [30, 31].into(),
        v4: (40, 41, (420, 421)),
        anon_0_v5: 42,
        anon_0_v6: (43, 44),
    };

    // Public decision variables (i.e. transient data).
    let pub_vars: Vec<Mutation> = simple::Foo::pub_vars::mutations()
        .t0(false)
        .t1(11)
        .t2([0x2222222222222222; 4])
        .anon_1_t3(45)
        .into();

    // State mutations.
    let state_mutations: Vec<Mutation> = simple::storage::mutations()
        .s0(true)
        .s1(42)
        .s2([0x4242424242424242; 4])
        .s3(|tup| tup._0(30)._1(31))
        .s4(|tup| tup._0(40)._1(41)._2(|tup| tup._0(420)._1(421)))
        .my_map0(|map| map.entry(42, 24))
        .my_map1(|map| {
            map.entry(1, |tup| {
                tup._0(1111)
                    ._1(|tup| tup._0([0x2222222222222222; 4])._1(3333))
            })
        })
        .my_nested_map0(|map| map.entry(1, |map| map.entry(2, 1234)))
        .my_nested_map1(|map| {
            map.entry(2, |map| {
                map.entry([0x3333333333333333; 4], |tup| {
                    tup._0(69)._1(|tup| tup._0([0x1111111100000000; 4])._1(96))
                })
            })
        })
        .into();

    // Create the solution data.
    let solution_data = SolutionData {
        predicate_to_solve: pred_addr,
        decision_variables: vars.into(),
        transient_data: pub_vars,
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
