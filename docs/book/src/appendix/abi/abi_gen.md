## Constructing Solutions using the ABI

The Application Binary Interface (ABI) of a contract provides all the essential information needed
to construct a solution for one or more predicates within the contract. Although you could perform
this manually, the [`pint-abi`](https://docs.rs/pint-abi/latest/pint_abi/) crate makes it much more
ergonomic in Rust. The `pint-abi` crate includes the `gen_from_file` macro, which automatically
generates the modules, types, and builder methods required to accomplish this.

### The `gen_from_file` Macro

Consider the following simple contract in Pint:

```pint
{{#include ../../../../../examples/abi_gen_example/src/contract.pnt}}
```

If you have `pint-abi` added as dependency in your Rust project, you'll have access to the
`gen_from_file` macro which can be called as follows:

```rust
{{#include ../../../../../examples/abi_gen_example.rs:gen_from_file}}
```

The macro takes two arguments:

1. `abi`: a path to the ABI of the contract in JSON format
1. `contract`: a path to the bytecode of the contract in JSON format

The macro above expands to a set of modules, types, and builder methods that allow creating data for
solutions **directly using Rust types** without having to do the encoding manually.

In order to construct a solution to a predicate, you need construct two vectors:

1. A vector of all the predicate arguments.
1. A vector of all the state mutations.

### Predicate Arguments

In order to construct a vector of arguments for the predicate above, you can use the following
syntax:

```rust
{{#include ../../../../../examples/abi_gen_example.rs:arguments}}
```

The module `MyPredicate` (which clearly corresponds to the predicate `MyPredicate` in our contract
above) and the struct `Vars` are readily available to us from the expansion of the `gen_from_file`
macro. The fields of the struct `Vars` exactly match the list of parameters of `MyPredicate`, both
in name and type.

Each predicate parameter Pint type has a corresponding Rust type as follows:

| Pint Type | Rust Type  |
| --------- | ---------- |
| `int`     | `i64`      |
| `bool`    | `bool`     |
| `b256`    | `[i64, 4]` |
| Union     | Enum       |
| Tuple     | Tuple      |
| Array     | Array      |

### State Mutations

In order to construct a vector of proposed state mutations for the contract above, you can use the
following syntax:

```rust
{{#include ../../../../../examples/abi_gen_example.rs:state_mutations}}
```

The module `storage` and the function `mutations()` are readily available for us from the expansion
of the `gen_from_file` macro. The resulting object is of type
`Vec<essential_types::solution::Mutation>` from the
[`essential_types`](https://docs.rs/essential-types/latest/essential_types/index.html) crate.

Because all state mutations are optional, they need to be set individually using their own builder
methods (unlike predicate arguments). For simple types like `int`, `bool`, `b256`, and unions, the
syntax is self explanatory. Those types have corresponding Rust types as for predicate arguments:

| Pint Type | Rust Type  |
| --------- | ---------- |
| `int`     | `i64`      |
| `bool`    | `bool`     |
| `b256`    | `[i64, 4]` |
| Union     | Enum       |

For more complex types like tuples, arrays, and storage maps, you need to provide _closures_ that
specify how to set each part of the compound type:

1. For tuples, you have available the builder methods `_0(..)`, `_1(..)`, etc. that specify a value
   for each individual entry of the tuple. you may skip some of these methods if you do not want to
   propose any mutations to the corresponding tuple field.
1. For arrays, you have the builder method `entry(..)` which takes an integer index and a value to
   set the array value at that particular index. Not all array indices need to be set.
1. For storage maps, you also have the builder method `entry(..)` which takes a key and a value to
   set the map value at that particularly key. The types of the key and the value must match the
   types of the key and the value of the map.

### Contract and Predicate Addresses

The expansion of the macro `gen_from_file` also provides the address of the contract and the address
of each predicate as constants:

```rust
{{#include ../../../../../examples/abi_gen_example.rs:addresses}}
```

These constants can be used to construct solutions when specifying what predicate is being solved.

### Producing Solutions

We now have everything we need to produce a solution. When working with the EssentialVM, a
`Solution` object can be constructed as follows:

```rust
{{#include ../../../../../examples/abi_gen_example.rs:solution}}
```

where we have used the address of the `MyPredicate` to specify which predicate to solve, the vector
`arguments` to specify values for `decision_variables` (recall that we sometimes refer to predicate
parameters as decision variables), and the vector `state_mutations` to specify the proposed state
mutations.

Note that this solutions only has a single `SolutionsData`. In general, solutions may contain
multiple `SolutionData` objects which can all be produced by following the steps above.

### Storage Keys

It is sometimes useful to know the storage keys where a particular storage variable (or some of its
parts) are stored. The expansion of the `gen_from_file` macro also provides the builder method
`storage::keys()` which can be used as follows:

```rust
{{#include ../../../../../examples/abi_gen_example.rs:keys}}
```

The method `keys()` is readily available in the module `storage`. Similarly to `mutations()`, the
key(s) for each storage variable must be appended using the corresponding builder method and the
syntax is fairly similarly to the builder methods for state mutations. The result is a vector of
`Key`s which can be used to query the state for example.
