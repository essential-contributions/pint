## Decision Variables

A decision variable is a named variable that every solution is required to assign a value for.
Decision variables are quite different from "regular" variables that you might be used to in
imperative programming languages. Decision variables do not get "computed" or "assigned to" in a
Pint program since a Pint program is **not actually executed but solved** (and later validated
against a solution).

Decision variables can be declared using the `var` keyword and may be annotated with a type. We will
go over the available data types in Pint, in detail, in a later chapter.

Here's an example that shows how to declared a decision variable named `foo` of type `int`:

```pint
{{#include ../../../../examples/ch_3_1_a.pnt:annotated}}
```

You can actually think of the type annotation as a "constraint" on the decision variable: this
decision variable can only take values in the set of signed integers (64-bit signed integers when
targeting the EssentialVM). Any solution that proposes a value of `foo` must satisfy that
constraint.

A decision variable can also be "initialized". Initializing a decision variable may seem like an odd
concept because decision variables are declared to be solved for. However, an initialized decision
variable is simply a decision variable with an extra implicit constraint. Here's how an initialized
decision variable can be declared:

```pint
{{#include ../../../../examples/ch_3_1_a.pnt:initialized}}
```

The above is equivalent to:

```pint
{{#include ../../../../examples/ch_3_1_b.pnt:initialized}}
```

We will go over `constraint` statements in more details later but it should hopefully be intuitive
that this statement enforces `bar` to be equal to `42`. Therefore, every proposed solution must also
set `bar` to `42`. Otherwise, this particular constraint will fail and the whole solution will be
deemed invalid.

Skipping the type annotation is only allowed if the decision variable is "initialized":

```pint
{{#include ../../../../examples/ch_3_1_a.pnt:initialized_unannotated}}
```

In this case, the compiler is able to _infer_ the type of `baz` by realizing that the initializer
`42` is of type `int`.

Again, the above is equivalent to:

```pint
{{#include ../../../../examples/ch_3_1_b.pnt:initialized_unannotated}}
```
