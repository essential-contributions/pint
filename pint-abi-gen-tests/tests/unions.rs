use pint_abi::types::essential::{
    solution::{Mutation, Solution, SolutionData},
    Key, PredicateAddress, Value,
};
use pint_abi_gen_tests::unions;
use std::sync::Arc;
use util::State;

mod util;

// Test that we can trivially create a full `SolutionData` for the unions
// contract's `Foo` predicate. Set the values to adhere to the arbitrary
// constraints of the predicate.
#[tokio::test]
async fn test_solution_foo() {
    tracing_subscriber::fmt::init();

    // Construct the package path.
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let manifest_dir_path = std::path::Path::new(&manifest_dir);
    let pkg_dir = manifest_dir_path.join("test-pkgs/unions");

    // Determine the content address of the contract.
    let contract_path = pkg_dir.join("out/debug/unions.json");
    let contract = pint_abi::contract_from_path(&contract_path).unwrap();

    // Determine the predicate address by loading the ABI and finding the matching predicate.
    let abi_path = pkg_dir.join("out/debug/unions-abi.json");
    let abi = pint_abi::from_path(&abi_path).unwrap();
    let (pred, _pred_abi) = pint_abi::find_predicate(&contract, &abi, "Foo").unwrap();

    // Check the generated addresses are correct.
    let contract_ca = essential_hash::contract_addr::from_contract(&contract);
    let pred_addr = PredicateAddress {
        contract: contract_ca.clone(),
        predicate: essential_hash::content_addr(pred),
    };
    assert_eq!(contract_ca, unions::ADDRESS);
    assert_eq!(pred_addr, unions::Foo::ADDRESS);

    // Decision variables.
    let vars = unions::Foo::Vars {
        v_u1: unions::UU::A(69),
        v_u2: unions::UU::B,
        v_u3: unions::UU::C([0x6969696969696969; 4]),
        v_w1: unions::WW::E(unions::UU::A(69)),
        v_pp1: unions::lib::PP::T(unions::lib::QQ::M),
        v_pp2: unions::lib::PP::JJ([0x1111111111111111; 4]),
        v_aa: unions::lib2::A::C,
        v_dd: unions::lib2::D::E(unions::lib3::foo::TT::B),
        v_tt: unions::lib3::foo::TT::B,
        v_rr: unions::RR::A(unions::lib::PP::T(unions::lib::QQ::N(42))),
        v_oo: unions::lib3::foo::OO::B((unions::lib3::foo::TT::A, unions::lib3::foo::TT::B)),
        v_array: [
            unions::RR::B,
            unions::RR::A(unions::lib::PP::T(unions::lib::QQ::M)),
        ],
        v_tuple: (unions::lib2::A::B, unions::lib3::foo::TT::A),
    };

    // State mutations.
    let state_mutations: Vec<Mutation> = unions::storage::mutations()
        .u1(unions::UU::A(69))
        .u2(unions::UU::B)
        .u3(unions::UU::C([0x6969696969696969; 4]))
        .w1(unions::WW::E(unions::UU::A(69)))
        .pp1(unions::lib::PP::T(unions::lib::QQ::M))
        .pp2(unions::lib::PP::JJ([0x1111111111111111; 4]))
        .aa(unions::lib2::A::C)
        .dd(unions::lib2::D::E(unions::lib3::foo::TT::B))
        .tt(unions::lib3::foo::TT::B)
        .rr(unions::RR::A(unions::lib::PP::T(unions::lib::QQ::N(42))))
        .oo(unions::lib3::foo::OO::B((
            unions::lib3::foo::TT::A,
            unions::lib3::foo::TT::B,
        )))
        .array(|arr| {
            arr.entry(0, unions::RR::B)
                .entry(1, unions::RR::A(unions::lib::PP::T(unions::lib::QQ::M)))
        })
        .tuple(|tup| tup._0(unions::lib2::A::B)._1(unions::lib3::foo::TT::A))
        .map(|map| map.entry([0x2222222222222222; 4], unions::WW::D))
        .map(|map| map.entry([0x3333333333333333; 4], unions::WW::E(unions::UU::A(55))))
        .into();

    // Build the same set of keys, so we can ensure they match the mutations.
    let keys: Vec<Key> = unions::storage::keys()
        .u1()
        .u2()
        .u3()
        .w1()
        .pp1()
        .pp2()
        .aa()
        .dd()
        .tt()
        .rr()
        .oo()
        .array(|arr| arr.entry(0).entry(1))
        .tuple(|tup| tup._0()._1())
        .map(|map| map.entry([0x2222222222222222; 4]))
        .map(|map| map.entry([0x3333333333333333; 4]))
        .into();

    // Check keys match the mutation keys.
    for (key, mutation) in keys.iter().zip(&state_mutations) {
        assert_eq!(key, &mutation.key);
    }

    // Check Encoding/Decoding roundtrip for decision vars.
    let words = pint_abi::encode(&vars);
    let vars2: unions::Foo::Vars = pint_abi::decode(&words[..]).unwrap();
    assert_eq!(&vars, &vars2);

    // Check To/From Vec<Value> roundtrip.
    let values: Vec<Value> = vars.clone().into();
    let vars3 = unions::Foo::Vars::try_from(&values[..]).unwrap();
    assert_eq!(&vars, &vars3);

    // Create the solution data.
    let solution_data = SolutionData {
        predicate_to_solve: unions::Foo::ADDRESS,
        decision_variables: vars.into(),
        state_mutations,
    };

    // Create the solution.
    let solution = Arc::new(Solution {
        data: vec![solution_data],
    });

    // Check the solution is valid.
    essential_check::solution::check(&solution).unwrap();

    // Start with an empty pre-state.
    let pre_state = State::new(vec![(unions::ADDRESS, vec![])]);

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
