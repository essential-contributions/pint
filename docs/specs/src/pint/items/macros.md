### Macro Items

Macro items describe reusable operations.

Regular macros have the following syntax:

```bnf
<macro-name> ::= @[A-Za-z_][A-Za-z0-9_]*

<macro-param> ::= $[A-Za-z0-9]+
<macro-param-pack> ::= &[A-Za-z0-9]+

<macro-body-item> ::= <tok> | <macro-param>     % where <tok> is any valid parsable source token.

<macro-item> ::= "macro" <macro-name> "(" [ <macro-param> "," ... ] ("," macro-param-pack)? ")" "{" <macro-body-item>* "}"
```

Macro expansion is the very first operation performed by the compiler. Any macro call expression is expanded in-place, replaced by the contents of the macro as parameterized by the call arguments.

Macros are intended to be a generalized method for code re-use. The macro parameters are special identifiers starting with a `$`, e.g., `$max` or `$0`.

#### Expansion

The macro body is only semi-parsed by the compiler prior to macro expansion and may contain macro identifiers. During expansion macro identifiers are replaced by the corresponding call arguments and the body is then fully parsed. The parsed items are then added to the program as if they were parsed where the macro is called.

For example, a simple macro may introduce some constraints:

```pint
macro @in_range($var, $num) {
    constraint $var >= $num;
    constraint $var < ($num * $num);
}
```

It may be used as such:

```pint
let x: int;
@in_range(x; 10);  // Constrain `x` to [10, 100).

```

After macro expansion the above becomes:

```pint
let x: int;
constraint x >= 10;
constraint x < (10 * 10);
```

The arguments to a macro call may be collections of tokens which do not necessarily parse to a proper expression. For example, an operator like `+` or a type name such as `real` are valid. If the token is an identifier then it may be used to name a declaration such as with `let`.

```pint
macro @do_decls($a, $b, $ty, $op) {
    let $a: $ty;
    let $b: $ty;
    constraint $b $op $a;
}
```

If called as `@do_decls(foo; bar; real; >)` it would expand to:

```pint
let foo: real;
let bar: real;
constraint bar > foo;
```

#### Macro Expressions

Macro bodies may have an expression rather than a declaration as its final item after expansion. In this special case the expression is inserted at the macro call, but any prior declaration items are inserted before the call.

```pint
macro @inverse_of($a) {
    constraint $a > 0.0;  // Declaration.
    1.0 / $a              // Final expression.
}
```

When called:

```pint
let foo: real;
let bar: real = @inverse_of(foo);
```

will expand to:

```pint
let foo: real;
constraint foo > 0.0;
let bar: real = 1.0 / foo;
```

#### Hygiene

Macro bodies also treat `let` declarations specially. It may be desirable to use a local variable within a macro. For example:

```pint
macro @is_even($a) {
    let half: int;
    constraint $a == half * 2;
}
```

In a naive macro system if `@is_even` was called more than once within the same module then after expansion there would be multiple `half` variable declarations, resulting in a name clash error.

To avoid this problem Pint's macro expansion aims to be 'hygienic' and place newly declared symbols into a unique anonymous namespace. Note that this is only done for symbols which are not macro parameters.

```pint
macro @let_decls($a) {
    let foo: int;       // Hygienic anonymous binding for `foo`.
    let $a: bool;       // Lexical binding for `$a`.
}
```

If called using `@let_decls(foo)` there would not be an error as the expansion would be equivalent to:

```pint
let anon_0::foo: int;
let foo: int;
```

And even when called multiple times with different arguments there would be no error:

```pint
@let_decls(foo);
@let_decls(bar);
```

Becomes equivalent to:

```pint
let anon_0::foo: int;
let foo: int;
let anon_1::foo: int;
let bar: int;
```

Of course if `@let_decls` was called with the argument `foo` multiple times there _would_ be an error.

#### Recursion And Variadic Macros

Macros may call other macros and a special type of recursion via variadic parameters is allowed. Sometimes it may be desirable to do repeated expansions using a single macro, essentially entering an expansion loop.

To support this macros may be declared to accept a variable number of parameters. It is valid to declare multiple macros _with the same name_ but different number of parameters and the correct one to expand will be chosen based on the number of arguments passed to the call.

Recursion may be performed via one or more recursing macros and one or more non-recursing, or 'terminating' macros. The recursing macros call other versions of itself but with a different number of -- usually fewer -- arguments. The terminating macros do not call another version of itself.

The recursing macros may have a parameter 'pack' as its final parameter, denoted using an `&`, e.g., `macro @foo($a, $b, &rest) { ... }`. The parameter pack is never empty, therefore a macro declaration with a corresponding signature minus the parameter pack is required to avoid a pattern match failure.

The parameter pack is not addressable in any way. It may only be used as arguments to another macro call.

This example performs a naive sum of named variables:

```pint
macro @sum($x, $y, &rest) {
    // Called only when `rest` is not empty.  We recurse by adding $x and $y and using &rest as the second argument.
    @sum($x + $y, &rest)
}

macro @sum($x, $y) {
    // Called only when the number of arguments is exactly 2.
    $x + $y
}
```

Calling `@sum(a; b)` will expand directly using the terminating macro to the expression `a + b`.

Calling `@sum(a; b; c; d)` will expand as follows:

- `@sum(a; b; c; d)` calls the _recursive_ macro as `@sum(a; b; [c, d])` where `[c, d]` is `&rest`.
- `@sum(a; b; [c, d])` expands to `@sum(a + b; c; d)`.
- `@sum(a + b; c; d)` calls the recursive macro again, as `@sum(a + b; c; [d])`.
- `@sum(a + b; c; [d])` expands to `@sum(a + b + c; d)`.
- `@sum(a + b + c; d)` calls the _terminating_ macro.
- `@sum(a + b + c; d)` expands to `a + b + c + d`.

Note that as the `&rest` parameter pack is passed in its expanded form, the above `@sum` macros could instead be the following, to the same effect:

```pint
macro @sum($x, &rest) {
    @sum($x + &rest)
}

macro @sum($x) {
    $x
}
```

The `&pack` parameter may be used by non recursive macros which wish to call recursive macros. A more realistic use of variadic macros might be to chain variables together in relative constraints:

```pint
macro @chain($a, &rest) {
    // Add the first link in the chain, then move to the rest.
    let $a: int;
    @chain_next($a; &rest);
}

macro @chain_next($prev, $next, &rest) {
    // Add the next link, constrain based on the previous link and continue.
    let $next: int;
    constraint $next > $prev + 10;
    @chain_next($next; &rest)
}

macro @chain_next($prev) {
    // Just expand to the final link.
    $prev
}

```

When called as `@chain(x; y; z)` it would expand to:

```pint
let x: int;
let y: int;
constraint y > x + 10;
let z: int;
constraint z > y + 10;
z
```

#### Array Argument Splicing

An extension to macro argument packing is array splicing which expands all the elements of an array variable in place as the arguments to a macro call. This is done by prefixing the array name with a tilde `~`.

```pint
let num_array: int[4];

constraint @sum(~num_array) < 8;
```

This is equivalent to:

```pint
let num_array: int[4];

constraint @sum(num_array[0]; num_array[1]; num_array[2]; num_array[3]) < 8;
```

Array splicing is usually only useful with variadic macros which can handle arrays of different sizes, though a non-variadic macro may be called with array splicing if the array size exactly matches the number of required arguments.

The array element accesses are expanded in place and the argument separators are only placed _between_ them and not at their ends. The following:

```pint
let two: int[2];

let x = @foo(~two + ~two + ~two);
```

expands to:

```pint
let two: int[2];

let x = @foo(two[0]; two[1] + two[0]; two[1] + two[0]; two[1]);
```

The three spliced arrays make up a total of 4 separate arguments in this specific case.

Similarly:

```pint
let nums = [1, 2, 3];

let x = @sum(100 + ~nums * 200);
```

expands to:

```pint
let nums = [1, 2, 3];

let x = @sum(100 + nums[0]; nums[1]; nums[2] * 200);
```

The arithmetic add and multiply are applied to the first and last elements of the array in this example.
