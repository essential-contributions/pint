## Bringing Paths into Scope with the `use` Keyword

Having to write out the paths to access items can feel inconvenient and repetitive.
In the previous chapter, whether we chose the absolute or relative path to `Asparagus`, every time
we wanted to use `Asparagus` we had to specify the modules `garden` and `vegetables`. Fortunately,
there’s a way to simplify this process: we can create a shortcut to a path with the `use` keyword
once, and then use the shorter name everywhere else in the scope.

In the example below, we bring the `::garden::vegetables` module into the scope of the root file to
use the `Asparagus` enum:

```pint
{{#include ../../../../examples/backyard/src/contract.pnt:baz}}
```

Adding `use` and a path in a scope is similar to creating a symbolic link in the filesystem. By
adding use `::garden::vegetables` in the root file, `vegetables` is now a valid name in that scope.

### Importing multiple items from the same module

### Providing New Names with the `as` Keyword
