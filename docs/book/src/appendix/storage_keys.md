## Appendix D: Storage Keys Assignment

In the Essential VM, contract storage is organized as a key-value map, where both keys and values
can consist of an arbitrary number of 64-bit words. In Pint, storage variables declared within
`storage { .. }` blocks are implemented using this key-value structure. This appendix explains how the
Pint compiler determines storage keys based on the types of storage variables.

### Primitive Types and Unions

Primitive types and unions are always stored in a single storage slot with a single key. The key
chosen has a single word which is equal to the index of the variable in the storage block.

### Arrays and Tuples

Arrays and tuples are flattened and spread out across multiple consecutive slots such that each slot
contains a primitive type or a union. The key for each slot consists of two words. The first word is
the index of the tuple or array variable in the storage block and the second word is the index of
the tuple field or array element in the flattened list.

### Storage Maps

Entries of storage maps are stored in one or more storage slots depending on their types:

#### Primitive Types and Unions in Storage Maps

Primitive types and unions in a storage map are stored in a single storage slot with a single key.
The key chosen has two components. The first component (1 word) is the index of the storage map in
the storage block and the second component is storage map key (which can have multiple words).

#### Arrays and Tuples in Storage Maps

Arrays and tuples in storage maps are flattened and spread out across multiple consecutive slots
such that each slot contains a primitive type or a union. The key for each slot consists of three
components. The first component (1 word) is the index of the storage map in the storage block, the
second component is storage map key (which can have multiple words), and the third component (1
word) is the tuple field or array element in the flattened list.

### Nested Types

Nested types in storage adhere to the outlined rules above recursively. For instance, in the case of
nested maps, the storage key is constructed by appending map keys at each level, followed by
handling the inner type according to the same rules. This process is illustrated more clearly in the
example below.

### Example

Consider the following storage block:

```pint
union MyUnion = A | B({ int, b256 });

storage {
    x: int,
    y: bool,
    z: b256,
    u: MyUnion,
    c: { int[3], { b256, bool }},
    m1: ( int => int ),
    m2: (int => { b256, ({ int, int } => { int, b256 }) }),
}
```

In the above, here are some storage accesses and the corresponding storage keys according to the
rules laid out above.

| Storage Access                  | Key(s)                                           |
| ------------------------------- | ------------------------------------------------ |
| `storage::x`                    | `[0]`                                            |
| `storage::y`                    | `[1]`                                            |
| `storage::z`                    | `[2]`                                            |
| `storage::u`                    | `[3]`                                            |
| `storage::c.0[0]`               | `[4, 0]`                                         |
| `storage::c.0[2]`               | `[4, 2]`                                         |
| `storage::c.0`                  | `[4, 0]`,`[4, 1]`,`[4, 2]`,                      |
| `storage::c.1.0`                | `[4, 3]`                                         |
| `storage::c.1.2`                | `[4, 4]`                                         |
| `storage::m1[42]`               | `[5, 42]`                                        |
| `storage::m1[51]`               | `[5, 51]`                                        |
| `storage::m2[69].0`             | `[6, 69, 0]`                                     |
| `storage::m2[69].1[{41, 42}].0` | `[6, 69, 1, 41, 42, 0]`                          |
| `storage::m2[69].1[{41, 42}].1` | `[6, 69, 1, 41, 42, 1]`                          |
| `storage::m2[69].1[{41, 42}]`   | `[6, 69, 1, 41, 42, 0]`, `[6, 69, 1, 41, 42, 0]` |

Note that the keys produced by the `storage::keys()` abstraction explained in the [Storage
Keys](abi/abi_gen.md#storage-keys) from the [Appendix C.2](abi/abi_gen.md) follow the same rules
above and will produce the exact same keys.
