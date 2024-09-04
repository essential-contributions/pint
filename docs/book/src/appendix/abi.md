## Appendix C: Application Binary Interface (ABI) Spec

The Application Binary Interface (ABI) is a condensed representation of a smart contract that
exposes enough information about the contract to allow external contexts to interact with it. The
ABI does not contain any contract _logic_ but only its data such as its storage variables, its
predicates, its decision variables, and so on. The ABI is serialized in JSON format, making it both
human readable and easily parsable by relevant tools.

> **Note** This particular ABI specification is mostly relevant for the EssentialVM. Other virtual
> machines may have different architectures, requiring a completely different ABI format.

### JSON ABI Specification

The ABI of a contract is represented as a JSON object containing the following properties:

#### `"storage"`

This is an array that describes every storage variable in the contract, i.e., every variable
declared in the `storage { .. }` block. Each entry in this array is a JSON object that contains the
following properties:

- `"name"`: a string representing the name of the storage variable.
- `"ty"`: a JSON object representing the type of the storage variable. This is further explained in
  [JSON Representation of Types](#json-representation-of-types).

#### `"predicates"`

This is an array that describes every predicate in the contract. Each entry in this array is a JSON
object that contains the following properties:

- `"name"`: a string representing the name of the predicate.
- `"vars"`: an array that contains every private (i.e. non-`pub`) decision variable in the contract.
  Each entry in this array is a JSON object that contains the following properties:
  - `"name"`: a string representing the name of the decision variable.
  - `"ty"`: a JSON object representing the type of the decision variable. This is further explained
    in [JSON Representation of Types](#json-representation-of-types).
- `"pub_vars"`: an array that contains every public decision variable in the contract. Each entry in
  this array is a JSON object that contains the following properties:
  - `"name"`: a string representing the name of the public decision variable.
  - `"ty"`: a JSON object representing the type of the public decision variable. This is further
    explained in [JSON Representation of Types](#json-representation-of-types).

> **Note**: The order in which private decision variables show up in the JSON is important and must
> match the order in which they are declared in the Pint code. When constructing a solution, that
> same order should also be respected.

### JSON Representation of Types

Each possible Pint type is represented in the ABI as a JSON object with properties that depend on
the type. Below is a list of the JSON objects for each possible type:

#### `int`

```json
"Int"
```

#### `bool`

```json
"Bool"
```

#### `b256`

```json
"B256"
```

#### Tuple

```json
{
  "Tuple": [
    {
      "name": <field1_name>,
      "ty": <field1_ty>
    }
    {
      "name": <field2_name>,
      "ty": <field2_ty>
    }
    ...
  ]
}
```

In the above, `<field1_name>`, `<field2_name>`, ... are strings representing the names of the tuple
fields. These are optional, that is, they can be set to `null` if the tuple field has no name.
`<field1_ty.`, `<field2_ty>`, ... are JSON objects representing the types of the tuple fields,
formatted according to the rules of this section.

#### Array

```json
{
  "Array": {
    "ty": <element_ty>,
    "size": <array_size>
  }
}
```

In the above, `<element_ty>` is a JSON object representing the type of each element in the array,
formatted according to the rules of this section. `<array_size>` is an integer representing the size
of the array.

#### Storage Map

```json
{
  "Map": {
    "ty_from": <ty_from>,
    "ty_to": <ty_to>,
  }
}
```

In the above, `<ty_from>` and `<ty_to>` are JSON objects representing the "from" type and the "to"
type in the map, formatted according to the rules of this section.

### Example

Here's an example contract and its corresponding JSON ABI:

```pint
{{#include ../../../../examples/appendix_c/src/contract.pnt}}
```

```json
{{#include ../../../../examples/appendix_c/out/debug/appendix_c-abi.json}}
```

Here's how we would interpret this JSON ABI:

- This contract has a single predicate called `::Foo`, which is the _full path_ of the `Foo`
  predicate in the contract above.
- Predicate `::Foo` has two private decision variables:
  - At index 0, we have `::v0` of type `int`.
  - At index 1, we have `::v1` of type `bool[5]`.
- Predicate `::Foo` has two public decision variables:
  - The first is called `::t0`. Its an array of 5 tuples, where each tuple contains two `int`s
    with no field names.
  - The second is called `::t1` and is an array of 3 `b256`s.
- The contract also has three storage variables:
  - The first is called `s0` and is of type `b256`.
  - The second is called `s1` and is a tuple of two `int`s.
  - The third is called `my_map` and is a storage map from `int` to a tuple of two `int`s.
