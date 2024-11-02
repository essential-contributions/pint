## Bringing Paths into Scope with the `use` Keyword

Having to write out the paths to access items can feel inconvenient and repetitive. In the previous
chapter, whether we chose the absolute or relative path to `Asparagus`, every time we wanted to use
`Asparagus` we had to specify the modules `garden` and `vegetables`. Fortunately, there’s a way to
simplify this process: we can create a shortcut to a path with the `use` keyword once, and then use
the shorter name everywhere else in the scope.

In the example below, we bring the `::garden::vegetables` module into the scope of the root file to
use the `Asparagus` enum:

```pint
{{#include ../../../../examples/backyard/src/contract.pnt:baz}}
```

Adding `use` and a path in a scope is similar to creating a symbolic link in the filesystem. By
adding use `::garden::vegetables` in the root file, `vegetables` is now a valid name in that scope.

### Handling conflicting imports

Pint does not allow bringing two items with the same name into scope with `use`. This name clash
makes it impossible for the compiler to distinguish between the two items in the scope. There are
two ways to avoid this problem. The first, is to only import the names of the parent modules.

Consider the following two libraries:

```pint
{{#include ../../../../examples/use_1/src/data/contract_lib.pnt}}
```

```pint
{{#include ../../../../examples/use_1/src/data/predicate_lib.pnt}}
```

Both libraries use the name `Data` to describe a types. The example below shows how to bring the two
`Data` types into scope and how to refer to them without having a conflict.

```pint
{{#include ../../../../examples/use_1/src/contract.pnt:solution}}
```

As you can see, using the parent module distinguishes the two `Data` types. If instead we specified
`use data::contract_lib::Data` and `use data::predicate_lib::Data`, we'd have two `Data` types in
the same scope and Pint wouldn't know which one we meant when we used `Data`.

There’s another solution to the problem of bringing two types of the same name into the same scope
with `use`: after the path, we can specify `as` and a new local name, or _alias_, for the type. The
example below shows another way to write the code in the previous example by renaming the two `Data`
types using `as`.

```pint
{{#include ../../../../examples/use_2/src/contract.pnt:solution}}
```

In each `use` statement, we choose a new name for `Data`. That guarantees that no conflicts arise.
This also has the side benefit of giving `Data` a more meaningful name in the current context.

### Using Nested Paths to Clean Up Large `use` Lists

If we’re using multiple items defined in the same module, listing each item on its own line can take
up a lot of vertical space in our files. For example, these two `use` statements we had in the
previous example bring two items into scope:

```pint
{{#include ../../../../examples/use_2/src/contract.pnt:imports}}
```

Instead, we can use nested paths to bring the same items into scope in one line. We do this by
specifying the common part of the path, followed by two colons, and then curly brackets around a
list of the parts of the paths that differ, as shown below.

```pint
{{#include ../../../../examples/use_3/src/contract.pnt:imports_short}}
```

In bigger programs, bringing many items into scope from the same module using nested paths can
reduce the number of separate `use` statements needed by a lot!

We can use a nested path at any level in a path, which is useful when combining two `use` statements
that share a subpath. For example, the code snippet below shows two `use` statements: one that
brings `data::contract_lib` into scope and one that brings `data::contract_lib::Data` into scope.

```pint
{{#include ../../../../examples/use_4/src/contract.pnt:imports_share}}
```

The common part of these two paths is `data::contract_lib`, and that’s the complete first path. To
merge these two paths into one `use` statement, we can use `self` in the nested path, as shown
below.

```pint
{{#include ../../../../examples/use_5/src/contract.pnt:imports_self}}
```

This line brings `data::contract_lib` and `data::contract_lib::Data` into scope.
