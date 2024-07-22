//! All items generated from `counter-abi.json`.

pint_abi::gen_from_file! {
    abi: "test-pkgs/counter/out/debug/counter-abi.json",
    contract: "test-pkgs/counter/out/debug/counter.json",
}

mod counter_from_str {
    // Just check that this doesn't fail - the implementation almost entirely
    // matches `from_file!` so no need to test much further.
    pint_abi::gen_from_str! {
      r#"{
        "predicates": [
          {
            "name": "",
            "vars": [],
            "pub_vars": []
          },
          {
            "name": "::Increment",
            "vars": [],
            "pub_vars": []
          },
          {
            "name": "::Init",
            "vars": [
              {
                "name": "::value",
                "ty": "Int"
              }
            ],
            "pub_vars": []
          }
        ],
        "storage": [
          {
            "name": "counter",
            "ty": "Int"
          }
        ]
      }"#
    }
}
