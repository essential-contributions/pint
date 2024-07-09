## Macros

Macros are a way of writing code that writes other code, which is known as _metaprogramming_.
Metaprogramming is useful for reducing the amount of code you have to write and maintain. Most
programming languages have "functions" which somewhat serve a similar purpose. Macros, however, are
a lot more powerful. While Pint does not have "functions", its macro system is powerful enough to
cover their use case.

Macro _expansion_ is the very first operation performed by the compiler. This property implies that
macros can contain _anything_ as long as the code they produce is parsable, i.e. does not violate
the _grammar_ of Pint. Later stages of the compiler will then validate the semantics of the code.

Macros in Pint have two main forms:

- **Simple Macros**: these are macros that accept a fixed number of parameters and do not support
  recursion.
- **Variadic Macros**: these are macros that accept a variable number of parameters and can support
  recursion.

### Simple Macros

Simple macros take a fixed number of parameters, each starting with `$`. Consider the following
simple macro:

```pint
{{#include ../../../../examples/ch_7_2_a.pnt:in_range}}
```

Macro definitions always start with the keyword `macro` followed by the name of the macro, which
must start with `@`. Then, a list of named parameters is provided in between parentheses. The body
of the macro is provided in between curly braces and can contain any code that uses the macro
parameters.

The macro above is named `@in_range` and takes 2 parameters named `$v` and `$num`. In the body of
this macro, these parameters are used as expressions but this is not always necessarily the case!
When this macro is used, two constraints are always produced. Let's use this macro as follows:

```pint
{{#include ../../../../examples/ch_7_2_a.pnt:macro_use}}
```

To call the macro, we write its name followed by a list of arguments, separated by a semicolon
(`;`), in between parentheses. The number of arguments must match the number of parameters that the
macro expects.

> **Note**: yes, the arguments are separated by a semicolon (`;`) and not a comma (`,`).

After the macro gets expanded, the compiler will produce code that is equivalent to the following:

```pint
{{#include ../../../../examples/ch_7_2_b.pnt:expanded}}
```

It should hopefully be quite clear to you how this substitution happened. The compiler simply
rewrote the body of the macro by replacing `$v` with `x` and `$num` with `10`. The resulting code is
then _pasted_ exactly where the macro was called.

#### Arbitrary Tokens as Macro Arguments

The arguments to a macro call may be collections of tokens which do not necessarily parse to a
proper expression, as in the previous example. For example, an operator like `+` or a type name such
as `int` are valid!. If the token is an identifier, then it may be used as a name, such as the name
of a decision variable or a new type. Here's an example:

```pint
{{#include ../../../../examples/ch_7_2_a.pnt:ops}}
```

The author of this macro likely expects:

- `$a` and `$b` to be identifiers.
- `$ty` to be a type.
- `op` to be a binary operator such as `+`, `-`, etc.

In fact, if the rules above are not respected when calling the macro, the program will likely fail
to parse correctly resulting in a failed compilation.

If we call the macro above with:

```pint
{{#include ../../../../examples/ch_7_2_a.pnt:ops_call}}
```

the compiler will expand the macro call to:

```pint
{{#include ../../../../examples/ch_7_2_b.pnt:ops_call_expanded}}
```

Hopefully this gives you an idea of how powerful macros can be.

#### Macro Expressions

So far, we've only looked at example macros where the body is a list of declarations (such as `var`
declarations or constraints). Macros are even more versatile than that! Macros can, in fact, produce
an expression. This would be akin to functions that return values in other programming languages.

The expression that you want produced by the macro must always be the last statement in the macro
body. For example:

```pint
{{#include ../../../../examples/ch_7_2_a.pnt:expr}}
```

Because this macro produces an expression, a call to it can be used as an expression as well. For
example:

```pint
{{#include ../../../../examples/ch_7_2_a.pnt:expr_call}}
```

As a result, the compiler will expand the macro call to:

```pint
{{#include ../../../../examples/ch_7_2_b.pnt:expr_call_expanded}}
```

Note that the expression is always inserted at the exact location where the macro was called, but
any declaration items in the macro body are inserted _before_ the call.

#### Declaring Variables in Macro Bodies

Earlier, we looked at an example macro that uses some of its parameters as identifiers to declare
some decision variables. When that macro is called multiple times with different arguments, the
resulting `var` declarations will not cause any name conflicts. Now, what happens if, instead, we
were to directly use an identifier in a macro body when declaring new variables? Here's an example:

```pint
{{#include ../../../../examples/ch_7_2_a.pnt:is_even}}
```

In a naive macro system, if `@is_even` was called more than once within the same module, then after
expansion there would be multiple `half` variable declarations, resulting a name clash error.

To avoid this problem Pint's macro expansion aims to be **hygienic** and places newly declared
symbols into a unique anonymous namespace. Note that this is only done for symbols which are **not**
macro parameters. To illustrate this, consider the following:

```pint
{{#include ../../../../examples/ch_7_2_a.pnt:var_decls}}
```

If we call the macro above using `@let_decls(foo)` there would not be an error as the expansion
would be equivalent to:

```pint
var anon_0::foo: int;
var foo: int;
```

And even when called multiple times with different arguments there would be no error:

```pint
{{#include ../../../../examples/ch_7_2_a.pnt:var_decls_call}}
```

Becomes equivalent to:

```pint
var anon_0::foo: int;
var foo: int;
var anon_1::foo: int;
var bar: int;
```

Of course, if `@let_decls` was called with the argument `foo` multiple times there would be an
error!

### Recursion and Variadic Macros

The second type of macros we will look at is "Variadic Macros". Variadic macros allow a special type
of recursion via _variadic parameters_. Such special parameters allow macros to call themselves,
essentially entering an _expansion loop_. Think of this as **recursive code generation**.

The term "variadic" means that the macro accepts a variable number of parameters. In the parameter
list of the macro definition this is indicated using a special parameter whose name starts with an
`&`. Recursion may be performed via one or more recursing macros and at least one non-recursing, or
_terminating_ macros. The recursing macros call other versions of itself but with a different number
of - usually fewer - arguments. The terminating macros do not call another version of itself. This
is best explained with an example, so let's consider the following macro which implements a sum
operation over an arbitrary number of named variables:

```pint
{{#include ../../../../examples/ch_7_2_a.pnt:sum}}
```

We have two versions of the `@sum` macro. Despite the apparent name clash, this is actually okay
because the two macros accept a different number of parameters. The first version of `@sum` accepts
`$x`, `$y`, and `&rest` while the second version accepts only `$x` and `$y`. The parameter `&rest`
is special and is referred to as a "parameter pack". The parameter pack is never empty, therefore in
order to call the first version of `@sum`, we must pass 3 or more arguments.

> **Note**: The parameter pack is not addressable in any way. It may only be used as an argument to
> another macro call.

Now let's walk through how the compiler would expand a call to `@sum`. You will notice that the
compiler will always try to match the number of arguments provided to the number of parameters in
each macro, and the best match will be selected.

Calling `@sum(a; b)` will expand directly using the terminating macro to the expression `a + b`.

Calling `@sum(a; b; c; d)` will expand as follows:

- `@sum(a; b; c; d)` calls the recursive macro as `@sum(a; b; [c, d])` where `[c, d]` is `&rest`.
- `@sum(a; b; [c, d])` expands to `@sum(a + b; c; d)`.
- `@sum(a + b; c; d)` calls the recursive macro again, as `@sum(a + b; c; [d])`.
- `@sum(a + b; c; [d])` expands to `@sum(a + b + c; d)`.
- `@sum(a + b + c; d)` calls the terminating macro.
- `@sum(a + b + c; d)` expands to `a + b + c + d`, which is the final expanded form of the macro
  call.

Note that, since the `&rest` parameter pack is passed in its expanded form, the above `@sum` macros
could instead be rewritten as follows, to the same effect:

```pint
{{#include ../../../../examples/ch_7_2_a.pnt:sum_simple}}
```

Parameter packs can also be used by non-recursive macros which wish to call other recursive macros.
A more interesting use of variadic macros might be to chain variables together in relative
constraints:

```pint
{{#include ../../../../examples/ch_7_2_a.pnt:chain}}
```

When called as `var r = @chain(m; n; p)`, the following code would be produced:

```pint
{{#include ../../../../examples/ch_7_2_b.pnt:chain_expanded}}
```

#### Array Argument Splicing

An extension to macro argument packing the the concept of _array splicing_. Array splicing allows
passing the elements of an array variable, in place, as the arguments to a macro call. This is done
by prefixing the array name with a tilde `~`.

Say we want to find the sum of the elements of some array of integers. If we were to use the
variadic macro `@sum` from earlier, we would have to write something like:

```pint
{{#include ../../../../examples/ch_7_2_a.pnt:sum_array}}
```

The issue with the above is that it's quite verbose, especially when the array size is large.
Instead, array splicing allows us to write this instead:

```pint
{{#include ../../../../examples/ch_7_2_a.pnt:sum_splicing}}
```

The compiler will then split `array` into its individual elements and pass them as separate
arguments to `@sum`, so that the resulting expansion is

```pint
{{#include ../../../../examples/ch_7_2_b.pnt:sum_splicing_expanded}}
```

Array splicing is usually only useful with variadic macros which can handle arrays of different
sizes, though a non-variadic macro may be called with array splicing if the array size exactly
matches the number of required arguments.

An important property of array splicing is that the array element accesses are expanded in place and
the argument separators are only placed between them and not at their ends! The following:

```pint
{{#include ../../../../examples/ch_7_2_a.pnt:splicing_2}}
```

expands to:

```pint
{{#include ../../../../examples/ch_7_2_a.pnt:splicing_2_expanded}}
```

This may be a bit surprising at first, but what is really happening here is that each `~two` gets
replaced _verbatim_ with `two[0]; two[1]` and the `+` signs stay exact where they were. So, the
three spliced arrays make up a total of 4 separate arguments in this specific case.

Similarly,

```pint
{{#include ../../../../examples/ch_7_2_a.pnt:splicing_3}}
```

expands to:

```pint
{{#include ../../../../examples/ch_7_2_a.pnt:splicing_3_expanded}}
```

The arithmetic add and multiply are applied to the first and last elements of the array in this
example.
