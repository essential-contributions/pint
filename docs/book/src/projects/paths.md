## Paths for Referring to an item in a Module Tree

To show Pint where to find an item in a module tree, we use a path in the same way we use a path
when navigating a filesystem. To use a custom type for example, we need to know its path.

A path can take two forms:

- An **absolute path** is the full path starting from a package root; for code from an external
  package, the absolute path begins with the package name, and for code from the current package, it
  starts with double colons (`::`).
- A **relative path** starts from the current module and uses an identifier in the current
  module.

Both absolute and relative paths are followed by one or more identifiers separated by double colons
(`::`).

Returning to the "backyard" example from the previous chapter, say we want to access the enum
`Asparagus`. This is the same as asking: what's the path of the `Asparagus` enum. We'll show two
ways to access `Asparagus` from the root file:

```pint
{{#include ../../../../examples/backyard/src/contract.pnt:bar}}
```

The first time we refer to the enum `Asparagus` we use an absolute path. The enum `Asparagus` is defined
in the same package as our root module above, which means we can use `::` to start an absolute path.
We then include each of the successive modules until we make our way to `Asparagus`.

The second time we refer to the enum `Asparagus`, we use a relative path. The path starts with
`garden`, the name of the module defined at the same level of the module tree as the root module.

Choosing whether to use a relative or absolute path is a decision you’ll make based on your project,
and depends on whether you’re more likely to move item definition code separately from or together
with the code that uses the item. For example, if we move the `garden` module to a module named
`estate`, we’d need to update the absolute path to `Asparagus`, but the relative path would still be
valid. However, if we moved the parameter `first_asparagus` into a module named `salad`, the
absolute path to `Asparagus` would stay the same, but the relative path would need to be updated.
Our preference in general is to specify absolute paths because it’s more likely we’ll want to move
code definitions and item calls independently of each other.
