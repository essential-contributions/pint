use pint_abi::types::essential::{
    solution::{Mutation, Solution, SolutionSet},
    Key, PredicateAddress, Value,
};
use pint_abi_gen_tests::exhaustive_storage;
use std::sync::Arc;
use util::State;

mod util;

// Test that we can trivially create a full `Solution` for the exhaustive_storage contract's `Foo`
// predicate. Set the values to adhere to the arbitrary constraints of the predicate.
#[tokio::test]
async fn test_solution_foo() {
    tracing_subscriber::fmt::init();

    // Construct the package path.
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let manifest_dir_path = std::path::Path::new(&manifest_dir);
    let pkg_dir = manifest_dir_path.join("test-pkgs/exhaustive_storage");

    // Determine the content address of the contract.
    let contract_path = pkg_dir.join("out/debug/exhaustive_storage.json");
    let (contract, programs) = pint_abi::contract_from_path(&contract_path).unwrap();

    // Determine the predicate address by loading the ABI and finding the matching predicate.
    let abi_path = pkg_dir.join("out/debug/exhaustive_storage-abi.json");
    let abi = pint_abi::from_path(&abi_path).unwrap();
    let (pred, _pred_abi) = pint_abi::find_predicate(&contract, &abi, "Foo").unwrap();

    // Check the generated addresses are correct.
    let contract_ca = essential_hash::contract_addr::from_contract(&contract);
    let pred_addr = PredicateAddress {
        contract: contract_ca.clone(),
        predicate: essential_hash::content_addr(pred),
    };
    assert_eq!(contract_ca, exhaustive_storage::ADDRESS);
    assert_eq!(pred_addr, exhaustive_storage::Foo::ADDRESS);

    // Predicate arguments.
    let args = exhaustive_storage::Foo::Args {
        v0: true,
        v1: 42,
        v2: [0x1111111100000000; 4],
        v3: [30, 31].into(),
        v4: (40, 41, (420, 421)),
        v5: (
            69,
            [70, 71, 72],
            [[0x3333333333333333; 4], [0x4444444444444444; 4]],
        ),
    };

    // State mutations.
    let state_mutations: Vec<Mutation> = exhaustive_storage::storage::mutations()
        .my_map0(|map| map.entry(true, 111))
        .my_map1(|map| map.entry([0x4444444444444444; 4], false))
        .my_map2(|map| map.entry((true, 30), [0x0000000000000001; 4]))
        .my_map3(|map| map.entry((490, [0x1111111111111111; 4]), true))
        .my_map4(|map| map.entry([0, 1, 2, 3, 4], 720))
        .my_map5(|map| {
            map.entry(
                [
                    [(false, 2), (true, 40), (false, 24)],
                    [(true, 42), (false, 21), (false, 25)],
                ],
                49,
            )
        })
        .my_map6(|map| map.entry(exhaustive_storage::UU::A(81), 82))
        .my_tuple_map0(|tup| tup._0(|map| map.entry(25, 26)))
        .my_tuple_map1(|map| map.entry(6, |tup| tup._1(|map| map.entry(2, |tup| tup._1(13)))))
        .into();

    // Build the same set of keys, so we can ensure they match the mutations.
    let keys: Vec<Key> = exhaustive_storage::storage::keys()
        .my_map0(|map| map.entry(true))
        .my_map1(|map| map.entry([0x4444444444444444; 4]))
        .my_map2(|map| map.entry((true, 30)))
        .my_map3(|map| map.entry((490, [0x1111111111111111; 4])))
        .my_map4(|map| map.entry([0, 1, 2, 3, 4]))
        .my_map5(|map| {
            map.entry([
                [(false, 2), (true, 40), (false, 24)],
                [(true, 42), (false, 21), (false, 25)],
            ])
        })
        .my_map6(|map| map.entry(exhaustive_storage::UU::A(81)))
        .my_tuple_map0(|tup| tup._0(|map| map.entry(25)))
        .my_tuple_map1(|map| map.entry(6, |tup| tup._1(|map| map.entry(2, |tup| tup._1()))))
        .into();

    // Check keys match the mutation keys.
    for (key, mutation) in keys.iter().zip(&state_mutations) {
        assert_eq!(key, &mutation.key);
    }

    // Check Encoding/Decoding roundtrip for decision args.
    let words = pint_abi::encode(&args);
    let args2: exhaustive_storage::Foo::Args = pint_abi::decode(&words[..]).unwrap();
    assert_eq!(&args, &args2);

    // Check To/From Vec<Value> roundtrip.
    let values: Vec<Value> = args.clone().into();
    let args3 = exhaustive_storage::Foo::Args::try_from(&values[..]).unwrap();
    assert_eq!(&args, &args3);

    // Create the solution for predicate `Foo`.
    let solution = Solution {
        predicate_to_solve: exhaustive_storage::Foo::ADDRESS,
        predicate_data: args.into(),
        state_mutations,
    };

    // Create the solution set.
    let solution_set = Arc::new(SolutionSet {
        solutions: vec![solution],
    });

    // Check the solution set is valid.
    essential_check::solution::check_set(&solution_set).unwrap();

    // Start with an empty pre-state.
    let pre_state = State::new(vec![(exhaustive_storage::ADDRESS, vec![])]);

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
