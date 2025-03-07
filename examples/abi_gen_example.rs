use pint_abi::types::essential::PredicateAddress;
use std::sync::Arc;
use util::State;

mod util;

// ANCHOR: gen_from_file
pint_abi::gen_from_file! {
    abi: "abi_gen_example/out/debug/abi_gen_example-abi.json",
    contract: "abi_gen_example/out/debug/abi_gen_example.json",
}
// ANCHOR_END: gen_from_file

#[tokio::test]
async fn test_abi_gen_example() {
    tracing_subscriber::fmt::init();

    // Construct the package path.
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let manifest_dir_path = std::path::Path::new(&manifest_dir);
    let pkg_dir = manifest_dir_path.join("abi_gen_example");

    // Determine the content address of the contract.
    let contract_path = pkg_dir.join("out/debug/abi_gen_example.json");
    let (contract, programs) = pint_abi::contract_from_path(&contract_path).unwrap();

    // Determine the predicate address by loading the ABI and finding the matching predicate.
    let abi_path = pkg_dir.join("out/debug/abi_gen_example-abi.json");
    let abi = pint_abi::from_path(&abi_path).unwrap();
    let (pred, _pred_abi) = pint_abi::find_predicate(&contract, &abi, "MyPredicate").unwrap();

    // Check the generated addresses are correct.
    let contract_ca = essential_hash::contract_addr::from_contract(&contract);
    let pred_addr = PredicateAddress {
        contract: contract_ca.clone(),
        predicate: essential_hash::content_addr(pred),
    };

    // ANCHOR: addresses
    let contract_address = ADDRESS;
    let my_predicate_address = MyPredicate::ADDRESS;
    // ANCHOR_END: addresses

    assert_eq!(contract_ca, contract_address);
    assert_eq!(pred_addr, my_predicate_address);

    // ANCHOR: predicate_data
    let predicate_data = MyPredicate::Args {
        x: 42,
        y: true,
        z: (2, [0x1111111100000000; 4]),
        a: [(true, 1), (false, 2)],
        u: MyUnion::A(3),
    };
    // ANCHOR_END: predicate_data

    // ANCHOR: state_mutations
    let state_mutations: Vec<essential_types::solution::Mutation> = storage::mutations()
        .s_x(7)
        .s_y(true)
        .s_z(|tup| tup._0(8)._1([0x2222222200000000; 4]))
        .s_a(|arr| {
            arr.entry(0, |tup| tup._0(false)._1(3))
                .entry(1, |tup| tup._0(true)._1(4))
        })
        .s_u(MyUnion::B)
        .m1(|map| map.entry(42, [0x3333333300000000; 4]))
        .m2(|map| map.entry(5, |map| map.entry(6, |tup| tup._0(true)._1(69))))
        .into();
    // ANCHOR_END: state_mutations

    // ANCHOR: keys
    let keys: Vec<essential_types::Key> = storage::keys()
        .s_x()
        .s_y()
        .s_z(|tup| tup._0()._1())
        .s_a(|arr| {
            arr.entry(0, |tup| tup._0()._1())
                .entry(1, |tup| tup._0()._1())
        })
        .s_u()
        .m1(|map| map.entry(42))
        .m2(|map| map.entry(5, |map| map.entry(6, |tup| tup._0()._1())))
        .into();
    // ANCHOR_END: keys

    for (key, mutation) in keys.iter().zip(&state_mutations) {
        assert_eq!(key, &mutation.key);
    }

    // ANCHOR: solution
    let solution_set = essential_types::solution::SolutionSet {
        solutions: vec![essential_types::solution::Solution {
            predicate_to_solve: MyPredicate::ADDRESS,
            predicate_data: predicate_data.into(),
            state_mutations,
        }],
    };
    // ANCHOR_END: solution

    let solution_set = Arc::new(solution_set);

    // Check the solution is valid.
    essential_check::solution::check_set(&solution_set).unwrap();

    // Start with an empty pre-state.
    let pre_state = State::new(vec![(ADDRESS, vec![])]);

    // Create the post-state by applying the mutations.
    let mut post_state = pre_state.clone();
    post_state.apply_mutations(&solution_set);

    // Our `get_predicate` function can only return `MyPredicate`.
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
