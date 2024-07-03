## Macros

Macros are a way of writing code that writes other code, which is known as _metaprogramming_.
Metaprogramming is useful for reducing the amount of code you have to write and maintain. Most
programming language have "functions" which somewhat serve a similar purpose. Macros, however, are a
lot more powerful. While Pint does not have "functions", its macro system is powerful enough to
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
When this macro is used, two constraints are always produced. Let's use the macro as follows:

```pint
{{#include ../../../../examples/ch_7_2_a.pnt:macro_use}}
```

To call the macro, we write its name followed by a list of arguments, separated by a `;`, in between
parentheses. The number of arguments must match the number of parameters that the macro expects.

> **Note**: yes, the arguments are separated by a `;` and not a `,`!

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
as `int` are valid!. If the token is an identifier, then it may be used to name as declaration such
as with `var`. Here's an example:

```pint
{{#include ../../../../examples/ch_7_2_a.pnt:ops}}
```

The author of this macro likely expects:

- `$a` and `$b` to be identifiers.
- `$ty` to be a type.
- `op` to be a binary operator such as `+`, `-`, etc.

In fact, if the rules above are not respected when calling the macro, the program will likely fail
to parse correctly resulting a failed compilation.

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

So far, we've only looked at examples macros where the body is a list of declarations (such as `var`
declarations) or constraints. Macros are even more versatile than that! Macros can, in fact, produce
an expression. This would be akin to functions that return values in other programming languages.

The expression that you want produced by the macro is always the last statement in the macro body.
For example:

```pint
{{#include ../../../../examples/ch_7_2_a.pnt:expr}}
```

Because this macro produces an expression, a call to it can be used as an expression. For example:

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

To avoid this problem Pint's macro expansion aims to be **hygienic** and place newly declared
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
