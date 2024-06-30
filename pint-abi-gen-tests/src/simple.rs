//! All items generated from `simple-abi.json`.

pint_abi_gen::from_file!("../pintc/tests/abi/simple-abi.json");

#[cfg(test)]
mod tests {
    use crate::simple;
    use pint_abi::types::essential::{
        solution::{Mutation, SolutionData},
        PredicateAddress,
    };

    // Test that we can trivially create a full `SolutionData` for the simple
    // contract's `Foo` intent.
    #[test]
    fn test_solution_foo() {
        // Decision variables.
        let vars = simple::Foo::Vars {
            v0: 42,
            v1: 42,
            v2: [42; 4],
            v3_0: 42,
            v3_1: 42,
            v4_0: 42,
            v4_1: 42,
            v4_2_0: 42,
            v4_2_1: 42,
        };

        // Public decision variables (i.e. transient data).
        let pub_vars: Vec<Mutation> = simple::Foo::pub_vars::mutations()
            .t0(42)
            .t1(42)
            .t2([42; 4])
            .into();

        // State mutations.
        let state_mutations: Vec<Mutation> = simple::storage::mutations()
            .s0(true)
            .s1(42)
            .s2([42; 4])
            .s3(|tup| tup._0(1)._1(2))
            .s4(|tup| tup._0(6)._1(7)._2(|tup| tup._0(11)._1(22)))
            .my_map(|map| map.entry(1, |tup| tup._0(2)._1(|tup| tup._0([1, 2, 3, 4])._1(5))))
            .my_nested_map(|map| {
                map.entry(2, |map| {
                    map.entry([4, 3, 2, 1], |tup| {
                        tup._0(0x42)._1(|tup| tup._0([0xFF; 4])._1(0xAA))
                    })
                })
            })
            .into();

        // TODO: Use the proper `PredicateAddress`.
        let predicate_to_solve = PredicateAddress {
            contract: [0; 32].into(),
            predicate: [0; 32].into(),
        };

        // Create the solution data.
        let solution_data = SolutionData {
            predicate_to_solve,
            decision_variables: vars.into(),
            transient_data: pub_vars,
            state_mutations,
        };

        dbg!(solution_data);
    }
}
