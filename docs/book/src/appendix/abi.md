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
- `"ty"`: a JSON object representing the type of the storage variable and the _storage key(s)_ used
  to store the variable. This is further explained in [JSON Representation of Types With
  Keys](#json-representation-of-types-with-keys).

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
  - `"ty"`: a JSON object representing the type of the public decision variable and the _transient
    storage key(s)_ used to storage the variable. This is further explained in [JSON Representation
    of Types With Keys](#json-representation-of-types-with-keys).

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

#### `string`

```json
"String"
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

### JSON Representation of Types With Keys

Storage variables and public decision variables are stored in key-value data structures where each
key is a vector of integers of arbitrary length. The keys assigned to those variables by the
compiler are included in the JSON ABI as part of their types.

Below is a list of the JSON objects for each possible type when a key is required.

#### `int`

```json
{
  "Int": <key>
}
```

`<key>` is an array of integers representing the key.

#### `bool`

```json
"Bool": <key>
```

`<key>` is an array of integers representing the key.

#### `string`

```json
"String": <key>
```

`<key>` is an array of integers representing the key.

#### `b256`

```json
"B256": <key>
```

`<key>` is an array of integers representing the key.

#### Tuple

```json
{
  "Tuple": {
    "fields": [
      {
        "name": <field1_name>,
        "ty": <field1_keyed_ty>
      }
      {
        "name": <field2_name>,
        "ty": <field2_keyed_ty>
      }
      ...
    ],
  }
}
```

In the above, `<field1_name>`, `<field2_name>`, ... are strings representing the names of the tuple
fields. These are optional, that is, they can be set to `null` if the tuple field has no name.
`<field1_keyed_ty.`, `<field2_keyed_ty>`, ... are JSON objects representing the types of the tuple
fields along with their keys, formatted according to the rules of this section.

#### Array

```json
{
  "Array": {
    "ty": <element_keyed_ty>,
    "size": <array_size>,
  }
}
```

In the above, `<element_keyed_ty>` is a JSON object representing the type of each element in the
array, formatted according to the rules of this section. `<array_size>` is an integer representing
the size of the array.

#### Storage Map

```json
{
  "Map": {
    "ty_from": <ty_from>,
    "ty_to": <keyed_ty_to>,
  }
}
```

In the above, `<ty_from>` is a JSON object representing the "from" type in the map, formatted
according to the rules of the previous section (i.e. without a key). `<keyed_ty_to>` is the JSON
object representing the "to" type in the map, formatted according to the rules of this section.

> **Note**: Keys to values in a storage map contain 1 or more `null`. These `null` values are
> placeholders; this is where the "from" value of the map is inserted to obtain the full concrete
> key.

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
  - The first is called `::t0`. Its an array of 5 tuples, where each tuple contains two integers
    with no field names.
    - The key for the first field of the _first tuple_ in the array is `[0, 0]`.
    - The key for the second field of the _first tuple_ in the array is `[0, 1]`.
    - Keys for the other tuples in the array can be inferred to be:
      - `[0, 2]` and `[0, 3]` for the second tuple.
      - `[0, 4]` and `[0, 5]` for the third tuple.
      - ... and so on.
  - The second is called `::t1` and is an array of 3 `b256`s. The key for the first `b256` is `[1,
0]`. The keys for the second and third `b256` are therefore `[1, 1]` and `[1, 2]` respectively.
- The contract also has three storage variables:
  - The first is called `s0` and is of type `b256` and its key is `[0]`.
  - The second is called `s1` and is a tuple of two integers. The key for the first integer is `[1,
0]` and the key for the second integer is `[1, 1]`.
  - The third is called `my_map` and is a storage map from `int` to a tuple of two integers.
    - The key for the first integer in the tuple is `[2, null, 0]`. The `null` here is a
      placeholder; this is where the "from" value, in this case an integer, is inserted to obtain
      the full concrete key. For example, the key for `my_map[69]` is `[2, 69, 0]`. Similarly, the
      key for the second integer in the tuple is `[2, null, 1]`.
