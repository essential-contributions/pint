# Specification of Yurt

## Introduction

This document defines the Yurt programming language and its primitives. It serves as a reference for the syntax and the semantics for each of its elements.

Yurt is a high-level, typed, intent expression and modeling language. It provides:

- mathematical notation-like syntax;
- expressive constraints;
- support for different kinds of problems (satisfaction, explicit optimization);
- extensibility (user-defined macros);
- reliability (type checking);
- solver-independent modeling;
- simple, declarative semantics.

Yurt's design is inspired by both [MiniZinc](https://www.minizinc.org/) and [Rust](https://www.rust-lang.org/).

This document has the following structure. [Notation](#notation) introduces the syntax notation used throughout the specification. [Overview of an Intent Model](#overview-of-an-intent-model) provides a high-level overview of Yurt intent models. [Syntax Overview](#syntax-overview) covers syntax basics. [High-level Intent Structure](#high-level-intent-structure) covers high-level structure: items, multi-file models, namespaces, and scopes. [Types](#types) introduces available types. [Expressions](#expressions) covers expressions. [Items](#items) describes the top-level items in detail.

## Notation

The specification of the Yurt programming language presented in this document follows the [EBNF format](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form). The basics of the EBNF used are as follows:

- Non-terminals are written between angle brackets, e.g. `<item>`.
- Terminals are written in double quotes, e.g. `"solve"`. A double quote terminal is written as a sequence of three double quotes: `"""`.
- Optional items are written in square bracket, e.g. `[ "," ]`
- Sequences of zero or more items are written with parentheses and a star, e.g. `( "," <ident> )*`
- Sequences of one or more items are written with parentheses and a plus, e.g. `( "," <ident> )+`.
- Non-empty lists are written with an item, a separator/terminator terminal, and three dots. For example, `<expr> "," ...` is short for `<expr> ( "," <expr> )* [ "," ]`. The final terminal is always optional in non-empty lists.

## Overview of an Intent Model

Conceptually, a Yurt problem specification has two parts:

1. The _model_: the main part of the problem specification, which describes the structure of a particular class of problems.
1. The data: the input data for the model, which specifies one particular problem with this class of problems.

The pairing of a model with a particular data set is a _model instance_.

The model and data may be separated, or the data may be "hard-wired" into the model.

There are two broad classes of problems: satisfaction and optimization. In satisfaction problems all solutions are considered equally good, whereas in optimization problems the solutions are ordered according to an objective and the ai is to find a solution whose objective is optimal. [Solve Items](#solve-items) specifies how the class of problem is chosen.

## Syntax Overview

### Character set

Yurt input files must be encoded as UTF-8 and may have the extension `*.yrt`.

Yurt is case sensitive and has no layout restrictions. That is, pieces of whitespace containing spaces, tabs, and newlines are all equivalent to each other.

### Comments

A `//` indicates that the rest of the line is a comment.

### Identifiers

Identifiers have the following syntax:

```ebnf
<ident> ::= _?[A-Za-z][A-Za-z0-9]*     % excluding keywords
```

A number of keywords are reserved and cannot be used as identifiers. The keywords are: `as`, `bool`, `constraint`, `contract`, `else`, `enum`, `extern`, `false`, `fn`, `if`, `implements`, `interface`, `int`, `let`, `maximize`, `minimize`, `real`, `satisfy`, `solve`, `state`, `string`, `true`, `type`, `use`.

### Paths

A path is a sequence of one ore more path segments logically separated by a namespace qualifier (`::`). If a path consists of only one segment, it refers to either an item or a variable in a local control scope. If a path has multiple segments, it always refers to an item.

Paths have the following syntax:

```ebnf
<path> ::= [ "::" ] <ident> ( "::" <ident> )*
```

For example:

```rust
::x;
x::y::z;
x;
```

Paths can be used in [Import Items](#import-items). They can also be used in expressions to directly refer to an item in a different module or [contract](#contract-items).

Paths that start with `::` are absolute paths from the root of the project. Paths that don't start with `::` are relative paths.

## High-level Intent Structure

A Yurt intent consists of one or more semicolon separated `items`:

```ebnf
<intent> ::= ( <item> ";" )*
```

Items can occur in any order; identifiers need not be declared before they are used. Items have the following top-level syntax:

```ebnf
<item> ::= <import-item>
         | <let-item>
         | <state-item>
         | <constraint-item>
         | <macro-item>
         | <enum-decl-item>
         | <solve-item>
         | <interface-item>
         | <contract-item>
         | <extern-item>
         | <new-type-item>
```

Import items (`<import-item>`) import new items from a module/submodule or external library into the current module ([Import Items](#import-items)).

Let declaration items introduce decision variables and optionally constrain them to values ([Let Declaration Items](#let-declaration-items)).

State declaration items (`<state-item>`) introduce _state_ variables and constrain them to values ([State Declaration Items](#state-declaration-items)).

Constraint items describe intent constraints ([Constraint Items](#constraint-items)).

Macro items introduce new parameterized blocks of declarations or expressions which can be used to facilitate code re-use ([Macro Items](#macro-items)).

Enum declaration items describe C-style enumerations ([Enum Declaration Items](#enum-declaration-items)).

Solve items specify exact what kind of solution the user is interested in: plain satisfaction, or the minimization/maximization of an expression. Each intent must have at most one solve item ([Solve Items](#solve-items)).

Interface items contain lists of smart contract methods that a [contract](#contract-items) can have ([Interface Items](#interface-items)).

Contract items describe actual deployed contracts with a known contract ID and a list of available methods ([contract Items](#contract-items)).

"Extern" items contain lists of external functions that allow accessing data on a blockchain (["Extern" Items](#extern-items)).

New Type items let you assign a new name to an existing type, simplifying complex type definitions or providing more context for certain types (["New Type" Items](#new-type-items)).

### Multi-file Intents

An intent can be spread across multiple files. Each file implicitly declares a local module or submodule dependency.

#### Entry File

Each multi-file Yurt project has an entry file. Compiling the entry file (e.g. using `yurtc <filename>`) should also compile all local modules and submodules that the entry file directly or indirectly depends on.

#### Declaring modules

In the project root directory, new modules can be created as follows:

- A single-file module must be defined in a file that has the same name as the module itself. For example, module `single_mod` must be defined in `src/single_mod.yrt`. The absolute [path](#paths) to items in `single_mod.yrt` is `::single_mod::<item>`.
- A multi-file module must be defined inside a directory that has the same name as the module itself. Moreover, the entry file of the module must also have the same name as the module. For example, multi-file module `multi_mod` must be defined in `src/multi_mod/multi_mod.yrt` and its submodule dependencies must live in `src/multi_mod/`. The absolute [path](#paths) to items in `multi_mod.yrt` is `::multi_mod::<item>`.

#### Declaring submodules

In any directory other than the project root directory, new submodules can be created as follows:

- A single-file submodule must be defined in a file that has the same name as the submodule itself. For example, submodule `single_submod` of module `multi_mod` must be defined in `src/multi_mod/single_submod.yrt`. The absolute [path](#paths) to items in `single_submod.yrt` is `::multi_mod::single_submod::<item>`.
- A multi-file submodule must be defined inside a directory that has the same name as the submodule itself. Moreover, the entry file of the submodule must also have the same name as the submodule. For example, multi-file submodule `multi_submod` must be defined in `src/multi_mod/multi_submod/multi_submod.yrt` and its own submodule dependencies must live in `src/multi_mod/multi_submod/`. The absolute [path](#paths) to items in `multi_submod.yrt` is `::multi_mod::multi_submod::<item>`.

Note that it is not allowed to have a file and a folder with the same name in any of the project's subdirectories. For example, a project that contains both `src/my_mod.yrt` and `src/my_mod/...` should not compile. Moreover, having a single file in a subdirectory is allowed even though subdirectories are typically used for multi-file modules. For example, having a single file in `src/my_mod/` is allowed as long as the name of that file is `my_mod.yrt`. This is equivalent to having the module live in `src/my_mod.yrt` and skipping the subdirectory `src/my_mod/` altogether.

### Namespaces and Scopes

TODO

## Types

Yurt provides 4 scalar types built-in: Booleans, integers, reals and strings. Yurt also provides tuples and arrays as a compound built-int type.

The syntax for types is as follows:

```ebnf
<ty> ::= "bool"
       | "int"
       | "real"
       | "string"
       | <tuple-ty>
       | <array-ty>
       | <custom-ty>
```

### Tuple Type

A tuple type represents a collection of items that may have different types. Tuple types have the following syntax:

```ebnf
<tuple-ty> ::= "{" ( [ <ident> ":" ] <ty> "," ... ) "}"
```

For example, in `let t: { int, real, string };`, `{ int, real, string }` is a tuple type. `{x: int, y: real, string }` is also a tuple type where some of the fields are named.

Names of tuple fields modify the type of the tuple. That is, `{ x: int }` and `{ y: int }` are different types. However they both coerce to `{ int }`.

Note that the grammar disallows empty tuple types `{ }`.

### Array Type

An array type represents a collection of items that share the same type. Arrays can be multi-dimensional and have the following syntax:

```ebnf
<array-ty> ::= <ty> ( "[" <expr> | <custom-ty> "]" )+
```

An array dimension can be indexed using non-negative integers. It can also be indexed using enum variants of some enum type or some [new type](#new-type-items) that resolves to an enum type. The allowed type of the index depends on how the dimension is specified in the array type definition.

- An array dimension that can be indexed using an integer requires that the corresponding dimension size is specified in between brackets as an expression that is evaluatable, **at compile-time**, to a **strictly positive** integer. Otherwise, the compiler should emit an error.

- An array dimension that can be indexed using an enum variant requires that the corresponding dimension size is specified in between brackets as the appropriate enum type.

For example, in:

```rust
let N = 5;
enum Colour = Red | Green | Blue;
let a: real[N][Colour];`
```

`a` is a two dimensional array that contains `N` arrays of size 3 each (because `Colour` is an enum that has 3 variants). An element of `a` can be [accessed](#array-expressions-and-array-element-access-expressions) using `a[3][Colour::Green]`, which accesses the second element of the fourth array in `a`.

### Enum Type

An enum type refers to an [enum declaration](#enum-declaration-items) using its path and has the following syntax:

```ebnf
<enum-ty> ::= <path>
```

## Expressions

### Expressions Overview

Expressions represent values and have the following syntax:

```ebnf
<expr> ::= <un-op> <expr>
         | <expr> <bin-op> <expr>
         | <block-expr>
         | "(" <expr> ")"
         | <path>
         | <bool-literal>
         | <int-literal>
         | <real-literal>
         | <string-literal>
         | <tuple-expr>
         | <tuple-field-access-expr>
         | <array-expr>
         | <array-element-access-expr>
         | <if-expr>
         | <cond-expr>
         | <call-expr>
         | <cast-expr>
         | <in-expr>
         | <range-expr>
         | <prime-expr>
```

Expressions can be composed from sub-expressions with operators. All unary and binary operators are described in [Operators
](#operators). All unary operators bind more tightly than all binary operators. Expressions can also be contained within parentheses.

### Operators

Operators are functions that are distinguished by their syntax:

1. They mostly contain non-alphanumeric characters that normal functions do not (e.g. `+`).
1. Their application may be written differently than normal functions.

There are two kinds of operators:

1. Unary operators which can be applied in a prefix manner without parentheses, e.g. `-x`.
1. Binary operator which can be applied in an infix manner, e.g. `3 + 4`.

```ebnf
<un-op> ::= "+" | "-" | "!"

<bin-op> ::= "<" | ">" | "<=" | ">=" | "==" | "!="
           | "+" | "-" | "*" | "/" | "%"
           | "&&" | "||"
```

### Expression Atoms

#### Block Expressions

Block expressions are expressions that contains a list of _statements_ followed by an expression within curly bracket `{ .. }`. Formally:

```ebnf
<block-expr> ::= "{" ( <block-statement> ";" )* <expr> "}"

<block-statement> ::= <let-item>
                    | <state-item>
                    | <constraint-item>
```

The type of the block expression is the type of the final expression. For example:

```rust
let x: int = {
    let y: int = 2;
    y + 1
}
```

#### Boolean Literals

Boolean literals have this syntax:

```ebnf
<bool-literal> ::= "false" | "true"
```

#### Integer and Real literals

There are three forms of integer literals: decimal, hexadecimal, and binary:

```ebnf
<int-literal> ::= [0-9]+
                | 0x[0-9A-Fa-f]+
                | 0b[0-1]+
```

For example: `1`, `0030`, `0x333`, `0b1010`.

Real literals have the following form:

```ebnf
<real-literal> ::= [0-9]+"."[0-9]+
                  | [0-9]+"."[0-9]+[Ee][-+]?[0-9]+
                  | [0-9]+[Ee][-+]?[0-9]+
```

For example: `1.05`, `2.5e-4`, `1.3E5`.

A `-` preceding an integer or real literal is parsed as a unary minus, not as part of the literal.

#### String Literals

String literals are written as:

```ebnf
<string-literal> ::= "\"" ([^"\n] | "\\" ("x" [0-9a-fA-F][0-9a-fA-F] | "n" | "t" | "\"" | "\\")) "\""
```

For example: `"Hello, world!\n"`.

String literals can be broken across multiple lines by escaping the newline and leading whitespace with a `\`. For example:

```rust
let string = "first line\
             second line\
             third line";
```

#### Tuple Expressions and Tuple Field Access Expressions

Tuple expressions are written as:

```ebnf
<tuple-expr> ::= "{" ( [ <ident> ":" ] <expr> "," ... ) "}"
```

For example: `let t = { x: 5, 3, "foo" };`. The type of this tuple can be inferred by the compiler to be `{ x: int, int, string }`.

The following is another example:

```rust
let t: { x: int, real } = { 6, 5.0 }
```

where the type of the tuple is indicated by the type annotation and has a named field `x`, but that named field is not actually used in the tuple expression. This is allowed because `{ x: int, real }` and `{ int, real }` coerce into each other.

Tuple fields can be initialized out of order only if all the fields have names and their names are used in the tuple expression. For example, the following is allowed:

```rust
let t: { x: int, y: real } = { y: 5.0, x: 6 };
```

while the following are not:

```rust
let t: { x: int, real } = { 5.0, x: 6 };
let t: { x: int, y: real } = { 5.0, x: 6 };
let t: { x: int, y: real } = { 5.0, 6 }; // This is a type mismatch!
```

Tuple expressions that contain a single _unnamed_ field require the trailing `,` as in `let t = { 4.0, };`. Otherwise, the expression becomes a code block that simply evaluates to its contained expression. Tuple expressions that contain a single _named_ field do not require the trailing `,`.

Note that the grammar disallows empty tuple expressions `{ }`.

Tuple field access expressions are written as:

```ebnf
<tuple-field-access-expr> ::= <expr> "." ( [0-9]+ | <ident> )
```

For example, `t.1;` refers to the second field of tuple `t`. Named field can be accessed using their names or their index. For example, if `x` is the third field of tuple `t`, then `t.2` and `t.x` are equivalent.

#### Array Expressions and Array Element Access Expressions

Array expressions are written as:

```ebnf
<array-expr> ::= "[" ( <expr> "," ... ) "]"
```

For example: `let a = [ 1, 2, 3 ];`. The type of this array can be inferred by the compiler to be `int[3]`.

The following is another example:

```rust
let b: real[2][3] = [ [ 1.0, 2.0, 3.0], [4.0, 5.0, 6.0] ];
```

where the type of the array is indicated by the type annotation. Note that the initializers of `b` is an array expression that contains other array expressions to reflect the fact that `b` is two dimensional. Also note that `real[2][3]` is an array that contains 2 elements where each element is of size 3, and not the other way around.

The grammar disallows empty array expressions `[ ]` because arrays of size 0 are not allowed.

All element expressions used in an array expression must have the exact same type and that type must match the type indicated in the array type annotation, if available.

Array element access expressions are written as:

```ebnf
<array-element-access-expr> ::= <expr> ( "[" expr "]" )+
```

For example, `a[1];` refers to the second element of array `a` in the example above. Therefore, `a[1]` should be equal to `2`. Similarly, `b[0][2]` in the example above refers to the third elements from the first inner array of `b`. That is, `b[0][2]` should be equal to `3.0`.

Yurt requires that the expression used to index into is an array. For example, in `foo()[5]`, `foo()` must return array of the appropriate size.

Yurt also requires that each array index is known (i.e. evaluatable) at compile-time. In addition, Yurt requires that each index evaluates to:

1. A **non-negative** integer that is **strictly smaller** than the corresponding dimension (i.e. within bounds), if the dimension has a size that is specified using an integer in the array type definition.
1. A path to an enum variant of some enum type, if the dimension size is specified using that enum type in the array type definition.

Below is an example where both an integer and an enum variant are used to index into an two-dimensional array:

```rust
let N = 5;
enum Colour = Red | Green | Blue;
let a: real[N][Colour];`

let a_3_g = a[3][Colour::Green];
```

#### "If" Expressions

Yurt has `if` expressions which provide selection from two alternatives based on a condition. They have this syntax:

```ebnf
<if-expr> ::= "if" <expr> <block-expr> "else" <block-expr>
```

The condition `<expr>` above must be of type `bool`. The "then" and "else" block expressions must have the same type or have types that are coercible to the same type, which determines the type of the whole `if` expression.

Note that the `else` block is not optional and the `else if { .. }` syntax is not supported.

#### "Cond" Expressions

Yurt provides `cond` expressions which are generalized `if` expressions with more than two branches. That is, they provide selection from multiple alternatives, each based on some condition. They have the following syntax:

```ebnf
<cond-branch> ::= <expr> "=>" <expr>

<else-branch> ::= "else" "=>" <expr>

<cond-expr> ::= cond "{" ( <cond-branch> "," )* <else-branch> [ "," ] "}"
```

The first `<expr>` in `<cond-branch>` must be of type `bool`. If it evaluates to `true`, then the branch is active which means that the whole `cond` expression takes the value of the second `<expr>` in `<cond-branch>`.

The branches are evaluated in order. The first one to become active determines the value of the `cond` expression. If all branches fail, then the `cond` expression takes the value of the `<expr>` in the `<else-branch>`.

Similarly to `if` expressions, all candidate expressions must have the same type or have types that are coercible to the same type, which determines the type of the whole `cond` expression.

#### Call Expressions

Call expressions are used to call macros or functions and have the following syntax:

```ebnf
<func-call-expr> ::= <path> "(" [ <expr> "," ... ] ")"
<macro-call-expr> ::= <path> "(" [ <tok>+ ";" ... ] ")"
```

For example: `let x = foo(5, 2);` or `constraint bar@(5; [a, b])`.

For function-style macros or extern function calls the type of the expressions passed as arguments must match the argument types of the called function. For all call expressions the number of passed arguments must match the called item, though this is flexible due to Yurt's variadic macro support. The return type of the function must also be appropriate for the calling context.

See [Macro Items](#macro-items) for the distinction between regular and function-style macros. When calling regular macros the call arguments may be a collection of source tokens, excluding semi-colons, delimited by semi-colons.

#### Cast Expressions

Cast expressions are used to cast one value to a different type. They have the following syntax:

```ebnf
<cast-expr> ::= <expr> "as" <ty>
```

Executing an `as` expression casts the value on the left-hand side to the type on the right-hand side.

For example: `let x: real = 5 as real;`.

Note that there is no implicit casting in Yurt, hence the need for an explicit casting mechanism using `as`.

Any cast that does not fit an entry in the table below is a compiler error:

| Type of LHS | RHS    | Cast performed                      |
| ----------- | ------ | ----------------------------------- |
| `int`       | `int`  | No-op                               |
| `int`       | `real` | Produce the closest possible `real` |
| `real`      | `real` | No-op                               |
| `enum`      | `int`  | Enum cast                           |
| `bool`      | `int`  | Boolean to integer cast             |

##### Enum Cast

Casts an enum to its discriminant. For example:

```rust
enum MyEnum = V0 | V1 | V2;

let d = MyEnum::V1 as int; // `d` is equal to `1`.
```

##### Boolean to Integer Cast

- `false` casts to `0`.
- `true` casts to `1`.

#### "In" Expressions

"In" expressions are used to detect whether a value belongs to an array. They have the following syntax:

```ebnf
<in-expr> ::= <expr> "in" <expr>
```

An "in" expression returns a `bool` that indicates whether the left-hand side belongs to the right-hand side. For example, in `let x: bool = 5 in [3, 4, 5];`, `x` should be `true`.

The right-hand side of an "in" expression must be an array and the type of the left-hand side must match the array elements type. Otherwise, a compiler error should be emitted.

A value belongs to an array if and only if it is "equal" to one of its entries. Equality for various types is defined as follows:

| Type     | Equality Criterion                                       |
| -------- | -------------------------------------------------------- |
| `int`    | Identical values                                         |
| `real`   | Identical values                                         |
| `bool`   | Identical values                                         |
| `string` | Identical lengths and characters in the same order       |
| `enum`   | Identical variants                                       |
| array    | Identical lengths and _equal_ elements in the same order |
| array    | Identical lengths and _equal_ elements in the same order |

Note that two values of different types cannot be compared and should result in a compile error.

#### Range Expressions

Range expressions are used to refer to ranges between a lower bound value and an upper bound value:

```ebnf
<range-expr> ::= <expr> ".." <expr>
```

Range expressions require that both the lower bound and the upper bound expressions have the same type. The type of a range expression is identical to the type of its bounds.

The only allowed types for the bounds of a range expression are `int` and `real`.

Range expressions can only be used in two contexts:

1. As the expression on the right-hand side of a `<let-item>`.
1. As the expression on the right-hand side of an `<in-expr>`

For example,

```rust
let x: int = 3..5;
```

is equivalent to;

```rust
let x: int;
constraint x in 3..5;
```

which is equivalent to:

```rust
let x: int;
constraint x >= 3;
constraint x <= 5;
```

#### Prime Expressions

Prime expressions are used to refer to the _future_ value of a [state variable](#state-declaration-items). They have the following syntax:

```ebnf
<prime-expr> ::= <expr> "'"
```

For example:

```rust
state u = MyContract::foo();
constraint u' - u > 10;
```

The above enforces that the blockchain state value returned by `MyContract::foo()` increases by at least 10 after the execution of a valid solution for the intent.

#### Expression Precedence

The precedence of Yurt operators and expressions is ordered as follows, going from strong to weak. Binary Operators at the same precedence level are grouped in the order given by their associativity.

| Operator                         | Associativity        |
| -------------------------------- | -------------------- |
| Paths                            |                      |
| Tuple field access expressions   | left to right        |
| Call expressions, array indexing |                      |
| Unary `-`, Unary `+`, `!`        |                      |
| `as`                             | left to right        |
| `in`                             | left to right        |
| `*`, `/`, `%`                    | left to right        |
| Binary `+`, Binary `-`           | left to right        |
| `==`, `!=`, `<`, `>`, `<=`, `>=` | Requires parentheses |
| `&&`                             | left to right        |
| `\|\|`                           | left to right        |
| `..`                             | Requires parentheses |

## Items

This section describes the top-level program items.

### Import Items

Within a scope, import items create shortcuts to items defined in other files. Import items have the following syntax:

```ebnf
<use-tree> ::= [ [ <path> ] "::" ] "*"
             | [ [ <path> ] "::" ] "{" [ <use-tree> "," ... ] "}"
             | <path> [ "as" <ident> ]

<import-item> ::= "use" <use-tree>
```

An import item creates one or more local name bindings synonymous with some other path. Usually a `use` item is used to shorten the path required to refer to a module item. These items may appear in modules and blocks, usually at the top.

Use declarations support a number of convenient shortcuts:

- Simultaneously binding a list of paths with a common prefix, using the brace syntax `use a::b::{c, d, e::f, g::h::i};`
- Simultaneously binding a list of paths with a common prefix and their common parent module, using the `self` keyword, such as `use a::b::{self, c, d::e};`.
- Rebinding the target name as a new local name, using the syntax `use p::q::r as x;`. This can also be used with the last two features: `use a::b::{self as ab, c as abc};`.
- Nesting groups of the previous features multiple times, such as `use a::b::{self as ab, c, d::{e, f::g}};`.

### Let Declaration Items

These are variables whose values may or may not be unknown for a given _instance_ for an intent. Solvers are required to find appropriate values for those variables with unknown values at compile-time.

Variable declaration items have the following syntax:

```ebnf
<let-item> ::= "let" <ident> ( ( ":" <ty> ) | ("=" <expr> ) | ( ":" <ty> "=" <expr> ) )
```

For example:

```rust
let x: int;
let y = 5;
```

Note that at least one of the type annotation and the initializing expression has to be present so that the type of the variable can be determined. This implies that `let x;` is not a valid variable declaration.

### State Declaration Items

These are variables that represent blockchain _state_ and require an initializer in the form of a [contract](#contract-items) method call or a call an [`extern` function](#extern-items). State variables are _not_ decision variables and the solver is not required to find values for them as their true value is determined by the blockchain. That being said, state variables can still be used in [constraint items](#constraint-items) to enforce various restrictions on the current and future state values.

State declaration items have the following syntax:

```ebnf
<state-item> ::= "state" <ident> [ ":" <ty> ] "=" <expr>
```

For example:

```rust
state x: int = MyContract::foo();
state y = MyContract::bar();
```

Note that, unlike [let declarations](#let-declaration-items), the initializer of a state declaration is not optional.

### Constraint Items

Constraint items represent the core of any intent. Any solution to the intent must satisfy all of its constraints.

Constraint items have this syntax:

```ebnf
<constraint-item> ::= "constraint" <expr>
```

For example:

```rust
constraint a + b <= c
```

The expression in a constraint item must be of type `bool`.

### Macro Items

Macro items describe user defined operations. They can take the form of a 'regular' macro, or of a specialized 'function-style' macro.

Regular macros have the following syntax:

```ebnf
<macro-name> ::= @[A-Za-z_][A-Za-z0-9_]*

<macro-param> ::= $[A-Za-z0-9]+

<macro-body-item> ::= <tok> | <macro-param>     % where <tok> is any valid parsable source token.

<macro-item> ::= "macro" <macro-name> "(" [ <macro-param> "," ... ] ")" "{" <macro-body-item>* "}"
```

Macro expansion is the very first operation performed by the compiler. Any macro call expression is expanded in-place, replaced by the contents of the macro as parameterized by the call arguments.

Macros are intended to be a generalized method for code re-use. The macro parameters are special identifiers starting with a `$`, e.g., `$max` or `$0`.

#### Expansion

The macro body is only semi-parsed by the compiler prior to macro expansion and may contain macro identifiers. During expansion macro identifiers are replaced by the corresponding call arguments and the body is then fully parsed. The parsed items are then added to the program as if they were parsed where the macro is called.

For example, a simple macro may introduce some constraints:

```yurt
macro @in_range($var, $num) {
    constraint $var >= $num;
    constraint $var < ($num * $num);
}
```

It may be used as such:

```yurt
let x: int;
@in_range(x; 10);  // Constrain `x` to [10, 100).

```

After macro expansion the above becomes:

```yurt
let x: int;
constraint x >= 10;
constraint x < (10 * 10);
```

The arguments to a macro call may be collections of tokens which do not necessarily parse to a proper expression. For example, an operator like `+` or a type name such as `real` are valid. If the token is an identifier then it may be used to name a declaration such as with `let`.

```yurt
macro @do_decls($a, $b, $ty, $op) {
    let $a: $ty;
    let $b: $ty;
    constraint $b $op $a;
}
```

If called as `@do_decls(foo; bar; real; >)` it would expand to:

```yurt
let foo: real;
let bar: real;
constraint bar > foo;
```

#### Macro Expressions

Macro bodies may have an expression rather than a declaration as its final item after expansion. In this special case the expression is inserted at the macro call, but any prior declaration items are inserted before the call.

```yurt
macro @inverse_of($a) {
    constraint $a > 0.0;  // Declaration.
    1.0 / $a              // Final expression.
}
```

When called:

```yurt
let foo: real;
let bar: real = @inverse_of(foo);
```

will expand to:

```yurt
let foo: real;
constraint foo > 0.0;
let bar: real = 1.0 / foo;
```

#### Hygiene

Macro bodies also treat `let` declarations specially. It may be desirable to use a local variable within a macro. For example:

```yurt
macro @is_even($a) {
    let half: int;
    constraint $a == half * 2;
}
```

In a naive macro system if `@is_even` was called more than once within the same module then after expansion there would be multiple `half` variable declarations, resulting in a name clash error.

To avoid this problem Yurt's macro expansion aims to be 'hygienic' and place newly declared symbols into a unique anonymous namespace. Note that this is only done for symbols which are not macro parameters.

```yurt
macro @let_decls($a) {
    let foo: int;       // Hygienic anonymous binding for `foo`.
    let $a: bool;       // Lexical binding for `$a`.
}
```

If called using `@let_decls(foo)` there would not be an error as the expansion would be equivalent to:

```yurt
let anon_0::foo: int;
let foo: int;
```

And even when called multiple times with different arguments there would be no error:

```yurt
@let_decls(foo);
@let_decls(bar);
```

Becomes equivalent to:

```yurt
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

```yurt
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

```yurt
macro @sum($x, &rest) {
    @sum($x + &rest)
}

macro @sum($x) {
    $x
}
```

The `&pack` parameter may be used by non recursive macros which wish to call recursive macros. A more realistic use of variadic macros might be to chain variables together in relative constraints:

```yurt
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

```yurt
let x: int;
let y: int;
constraint y > x + 10;
let z: int;
constraint z > y + 10;
z
```

#### Function-style Macros

Yurt also supports 'function-style' macros which are more constrained but also stricter in their use. Function-style macros look like function declarations and are called with standard expression arguments.

Function-style macros must have a final expression in their body and they may only be called as a sub-expression. Each parameter is a typed identifier and a return type (i.e., the type of the final body expression) must be specified.

```ebnf
<function-sig> ::= "fn" <ident> "(" [ <param> "," ... ] ")" "->" <ty>

<function-item> ::= <function-sig> <block-expr>

<param> ::= <ident> ":" <ty>
```

For example, the following macro expands to a boolean expression which tests if the parameter is an even number:

```yurt
fn is_even(x: int) -> bool {
    x % 2 == 0
}
```

The extra type information is used to confirm correct use, which is not directly possible with regular macros. Expansion otherwise follows the regular macro procedures.

### Enum Declaration Items

In Yurt, an enum type is a named enumeration of integer constants. Unlike sum types found in some functional languages, each member of an enum in Yurt is associated with an integer discriminant, making it similar to C-style enums. The syntax for declaring an enum is:

```ebnf
<enum-decl-item> ::= "enum" <ident> "=" <ident> ( "|" <ident> )*
```

For example, `enum Colour = Red | Green | Blue;` declares an enum with three variants. An instantiation of a `Colour` can be created using a path that includes the name of the enum, as in `Colour::Green;`.

Each enum variant must be assigned a discriminant that matches the index of its location, starting with `0`, in the sequence of variants as they appear in the enum declaration. An enum variant can be converted to an integer that is equal to its assigned discriminant using `as`.

### Solve Items

Every intent must have at most one solve item. Solve items have the following syntax:

```ebnf
<solve-item> ::= "solve" "satisfy"
               | "solve" "minimize" <expr>
               | "solve" "maximize" <expr>
```

Example solve items:

```rust
solve satisfy;
solve maximize a + b - c;
```

The solve item determines whether the intent represents a constraint satisfaction problem or an optimization problem. If a solve item is not present, the intent is assumed to be a satisfaction problem. For optimization problems, the given expression is the one to be minimized/maximized.

### Interface Items

Interface items contain lists of smart contract functions that a smart [contract](#contract-items) can have ([Interface Items](#interface-items))

Interface items describe lists of smart contract functions, in the form of function signatures, that a contract can have. An interface item has the following syntax:

```ebnf
<interface-item> ::= "interface" <ident> "{" ( <function-sig> ";" )* "}"
```

For example, the following is a simple interface with 3 functions:

```rust
interface IERC20 {
    fn totalSupply() -> int;
    fn balanceOf(account: int) -> int;
    fn allowance(owner: int, spender: int) -> int;
}
```

Interface functions are not callable directly. Instead, they have to be called through a [contract](#contract-items).

### Contract Items

Contract items describe actual deployed contracts with a known contract ID and a list of available functions. Contract items require a known integer ID and a list of function signatures. Contract can also "inherit" functions from [interfaces](#interface-items). Contract items have the following syntax:

```ebnf
<contract-item> ::= "contract" <ident> "(" <expr> ")"
                    [ "implements" <path> ( "," <path> )* ]
                    "{" ( <function-sig> ";" )* "}"
```

For example, consider the contract item below:

```rust
contract MyToken(0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48) implements IERC20, Ownable {
    fn foo() -> int;
    fn bar() -> int;
}
```

This contract is called `MyToken` and has an integer ID of `0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48`. The contract has the following functions:

1. All the functions declared in the `IERC20` interface.
1. All the functions declared in the `Ownable` interface.
1. All the functions declared in the body of the contract, namely `foo()` and `bar()`.

A call to any of these functions can be made using a `<call-expr>` with the name of the contract used in `<path>`. For example, `MyToken::foo()`. Contract function calls _always_ return values which must be bound to [state variables](#state-declaration-items), such as `state u = MyToken::foo()`.

### "Extern" Items

"Extern" items contain lists of function signatures that represent external APIs that can access data on a blockchain. More specifically, for the Ethereum blockchain, the functions should match the [JSON-RPC methods](https://ethereum.github.io/execution-apis/api-documentation/) that all Ethereum clients must implement.

The syntax for extern items is as follows:

```ebnf
<extern-item> ::= "extern" "{" ( <function-sig> ";" )* "}"
```

For example:

```rust
extern {
    fn eth_getBalance(address: string) -> string;

    fn eth_gasPrice() -> string;
}
```

The types used in the signature of `extern` functions depend on the types used by the external APIs. In the case of Ethereum JSON-RPC, a string is used to encode all values, hence a `string` type must be used in the `extern` block.

Extern functions are available directly without any special scoping. The only requirement is that the functions are called in the same file where the `extern` block is declared or that the functions are imported using an [import item](#import-items), similarly to regular functions.

### "New Type" Items

New Type items introduce a distinct type that is not directly interchangeable with its underlying type or other new types based on the same underlying type.

The syntax for declaring a new type is:

```ebnf
<new-type-item> ::= "type" <ident> "=" <ty>
```

For example:

```rust
type AccountTuple = { id: int, balance: real, address: string };

type IdArray = int[5];

type Address = string;
```

In the above declarations:

`AccountTuple` is a new type for a `tuple` type to represent the account's ID, balance, and address. `IdArray` is a new type for an `array` type to represent a list of account ids. `Address` is an new type for the `string` type to represent blockchain addresses or other specific string-based identifiers.

New type values may be initialized through:

- Expressions that conform to the new type's structure (such as tuple and array expressions). For instance, `{ x: .. }` for tuples or `[1, y, .. ]` for arrays.

- Literals of primitive types (`int`, `real`, `bool`, `string`) as long as they are compatible with the new type's underlying definition.

```rust
let walletDetails: AccountTuple = {id: 1, balance: 2.0, address: "0x1234...ABCD"};

let ids: IdArray = [1, 2, 3, 4, 5];

let myAddress: Address = "0x1234567890abcdef";
```

## Language Backend

The backend of the DSL (or API) that we're building is effectively a Constraint Programming Solver, simply referred to as a "solver" in this documentation. Note, this should not be confused with the "solver" agent in the network. There are numerous commercial solvers that can be targeted, such as [OR-Tools](https://developers.google.com/optimization/), [Geocode](https://www.gecode.org/), and [Chuffed](https://github.com/chuffed/chuffed). We could decide on a standardized JSON output that is generated by the compiler and that can be read and subsequently _solved_ by network participants using their solver of choice (not all solvers are created equal!).
