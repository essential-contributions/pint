## Constants

Sometimes it may be desirable to use a common constant value for re-use throughout a program which
is not a decision variable to be solved. These may be declared in Pint using the `const` keyword.

`const` declarations resemble `var` declarations in that they name an optionally typed value with an
initializer.

```pint
{{#include ../../../../examples/ch_3_7.pnt:consts}}
```

Like `var` declarations the type may be omitted and will be inferred by the Pint compiler, but the
`const` initializer is required and must a constant expression which does not refer to decision
variables nor other non-constant values such as state.

### Constants of Compound Types

`const` declarations may refer to values with compound types as long as every element within is a
constant value. Constant value initializers may also dereference other array or tuple `const`
declarations or even array or tuple literals.

```pint
{{#include ../../../../examples/ch_3_7.pnt:const_compound_types}}
```

In the above example `next_count` is evaluated **at compile time** to be fixed as 40.

The `min_size` tuple is adding a flag to a value to mark whether it should be used or not in a
constraint. This may be convenient during development for turning the `min_size.size` constraint on
or off.
