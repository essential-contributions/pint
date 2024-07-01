use pint_abi::types::essential::{
    solution::{Mutation, SolutionData},
    PredicateAddress,
};
use pint_abi_gen_tests::counter;

#[test]
fn test_solution_init() {
    // Construct the package path.
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let manifest_dir_path = std::path::Path::new(&manifest_dir);
    let pkg_dir = manifest_dir_path.join("test-pkgs/counter");

    // Determine the content address of the contract.
    let contract_path = pkg_dir.join("out/debug/counter.json");
    let contract = pint_abi::contract_from_path(&contract_path).unwrap();
    let contract_ca = essential_hash::contract_addr::from_contract(&contract);

    // Determine the predicate address by loading the ABI and finding the matching predicate.
    let abi_path = pkg_dir.join("out/debug/counter-abi.json");
    let abi = pint_abi::from_path(&abi_path).unwrap();
    let (_pred_abi, pred) = pint_abi::find_predicate(&contract, &abi, "Init").unwrap();
    let pred_ca = essential_hash::content_addr(pred);
    let pred_addr = PredicateAddress {
        contract: contract_ca,
        predicate: pred_ca,
    };

    // Initialise the value to `0`.
    const INIT_VALUE: i64 = 0;
    let vars = counter::Init::Vars { value: INIT_VALUE };
    let state_mutations: Vec<Mutation> = counter::storage::mutations().counter(INIT_VALUE).into();

    // Create the solution data.
    let _solution_data = SolutionData {
        predicate_to_solve: pred_addr,
        decision_variables: vars.into(),
        transient_data: vec![],
        state_mutations,
    };
}
