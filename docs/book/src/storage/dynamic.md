## Dynamically-sized Storage Types

Pint includes a number of very useful data structures called _storage collections_. Most other data
types represent one specific value, but storage collections can contain multiple values. Unlike the
built-in array and tuple types, the amount of data that these collections hold does not need to be
known at compile time and can grow or shrink as the blockchain state evolve. Each kind of collection
has different capabilities and costs, and choosing an appropriate one highly depends on the
situation at hand. In this chapter, we'll discuss two collections that are used very often in Pint
contracts:

- **Storage Map**: allows you to associated a value with a particular key.
- **Storage Vector**: allows you to store a variable number of values.

We'll discuss how to create and update storage maps and storage vectors.

### Storage Map

The first collection we will look at is the storage map. A storage map stores a mapping of keys of
some type `K` to values of some type `V`. Storage maps are useful when you want to look up data by
using a key that can be of any type. For example, in the [Subcurrency](../examples/subcurrency.md)
contract, we kept track of each user’s token balance in a map in which each key is a user’s address
and the values are each user’s balance. Given a user address, you can retrieve their balance.

#### Creating a New Storage Map

Because storage maps are a storage type, they must always be declared inside a `storage` block. The
storage map type is a built-in type with a specific syntax:

```pint
{{#include ../../../../examples/storage_dynamic_a.pnt:storage_block}}
```

Here, a new storage map, called `balances`, is declared that maps `b256` keys to `int` values. The
storage map type is always declared using parentheses that contain two types separated by a `=>`.
It should be clear that storage maps are _homogeneous_, that is, all of the keys must have the same
type as each other, and all of the values must have the same type as well.

#### Accessing Values in a Storage Map

We can get a value out of the storage map by providing its key in between square brackets, similar
to how arrays are accessed:

```pint
{{#include ../../../../examples/storage_dynamic_a.pnt:storage_access_1}}
```

Here, `from_balance` will have the value that's associated with `from_address` and
`receiver_balance` will have the value that's associated with `receiver_address`. Because the values
returned are storage values, they must be used in the initializers of some local variables. Using a
storage map access expression in any other context results in a compile error.

#### "Updating" a Storage Map

As we've mentioned a few times already, explicitly "updating" _anything_ in Pint is not a valid
operation because Pint has no sequential execution. However, as in the case with statically-sized
storage types, we can _require_ that the next value of a specific entry in a storage map satisfy
some constraint(s):

```pint
{{#include ../../../../examples/storage_dynamic_a.pnt:next_state}}
```

Here, we are requiring that our balance go up by 1 million, by applying the "prime" operator on the
state variable that holds our current balance. Of course, requiring a change in state does not mean
it will actually happen! Otherwise, we can all become instantly rich by deploying predicates like
this. Unless a solution that does not violate any of the deployed rules (i.e. constraints) is
submitted by a solver, the desired state change will never be satisfied.

#### "Missing" Keys

Now, you may be wondering what happens if a key is missing from a storage map and we try to access
it anyways. In Pint, a [`nil`](static.md#empty-state) is returned. In the previous example, if the
balance of `my_address` was never actually modified in the past, then `my_bal` would be equal to
`nil` and therefore, the expression `my_bal + 1000000` would panic. To avoid this problem, we can
first check if `my_bal` is `nil` before trying to use it in an arithmetic operation:

```pint
{{#include ../../../../examples/storage_dynamic_a.pnt:next_state_with_check}}
```

Here, if `my_bal` is not `nil`, then the constraint remains the same as before. Otherwise, we simply
update `my_bal` to `1000000` (as if `my_bal` was previously 0!).

#### Complex Maps

Storage maps can be declared to be arbitrarily complex. They can also be nested!

```pint
{{#include ../../../../examples/storage_dynamic_b.pnt:storage_block}}
```

In the example above, the fist storage map maps a tuple to another tuple. The second storage map
maps a `b256` to another map! The way to access entries in these maps is fairly intuitive and is
exactly what you'd expect:

```pint
{{#include ../../../../examples/storage_dynamic_b.pnt:storage_access}}
```

The first storage access reads a tuple value using a key that itself is a tuple, and then accesses
its second field. The second storage access is a nested map access using two index operators. Note
that the first index operator accesses the first key (`b256` in this case) and the second index
operator accesses the second key (`int` in this case).

#### Illegal Uses of the Storage Map Type

It may be tempting to write code like this:

```pint
storage {
    // ...
    nested_map: (b256 => (int => bool)),
    // ...
}

predicate test(addr: b256, my_map: (int => bool)) {
    let nested_map: (b256 => (int => bool)) = storage::nested_map;
    let nested_map_inner: (int => bool) = storage::nested_map[addr];
}
```

However, the compiler will disallow this by emitting the following errors:

```console
Error: predicate parameters cannot have storage types
   ╭─[bad.pnt:7:28]
   │
 7 │ predicate test(addr: b256, my_map: (int => bool)) {
   │                            ──────────┬──────────
   │                                      ╰──────────── found parameter of storage type ( int => bool ) here
───╯
Error: local variables cannot have storage types
   ╭─[bad.pnt:8:5]
   │
 8 │     let nested_map: (b256 => (int => bool)) = storage::nested_map;
   │     ──────────────────────────────┬──────────────────────────────
   │                                   ╰──────────────────────────────── found local variable of storage type ( b256 => ( int => bool ) ) here
───╯
Error: local variables cannot have storage types
   ╭─[bad.pnt:9:5]
   │
 9 │     let nested_map_inner: (int => bool) = storage::nested_map[addr];
   │     ───────────────────────────────┬───────────────────────────────
   │                                    ╰───────────────────────────────── found local variable of storage type ( int => bool ) here
───╯
```

Hopefully the error messages are clear enough. What the compiler is telling us here is that we
cannot have predicate parameters or local variables that hold entire storage maps. A storage map is
not exactly an object that we can store a reference to or copy/move around.

### Storage Vector

> **Note**: Storage vectors are work-in-progress
