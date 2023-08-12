# Specification of Yurt

## Introduction

This document defines the Yurt programming language and its primitives. It serves as a reference for the syntax and the semantics for each of its elements.

Yurt is a high-level, typed, intent expression and modeling language. It provides:

- mathematical notation-like syntax;
- expressive constraints;
- support for different kinds of problems (satisfaction, explicit optimization);
- extensibility (user-defined functions);
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

A number of keywords are reserved and cannot be used as identifiers. The keywords are: `as`, `bool`, `constraint`, `contract`, `else`, `enum`, `false`, `fn`, `if`, `implements`, `interface`, `int`, `let`, `maximize`, `minimize`, `real`, `satisfy`, `solve`, `string`, `true`, `use`.

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
         | <constraint-item>
         | <function-item>
         | <enum-decl-item>
         | <solve-item>
         | <transition-item>
         | <interface-item>
         | <contract-item>
```

Import items (`<import-item>`) import new items from a module/submodule or external library into the current module ([Import Items](#import-items)).

Variable declaration items (`<let-item>`) introduce variables and optionally bind them to a value ([Variable Declaration Items](#variable-declaration-items)).

Constraint items describe intent constraints ([Constraint Items](#constraint-items)).

Function items introduce new user-defined functions which can be called in expressions ([Function Items](#function-items)).

Enum declaration items describe C-style enumerations ([Enum Declaration Items](#enum-declaration-items)).

Solve items specify exact what kind of solution the user is interested in: plain satisfaction, or the minimization/maximization of an expression. Each intent must have at most one solve item ([Solve Items](#solve-items)).

Interface items contain lists of smart contract methods that a [contract](#contract-items) can have ([Interface Items](#interface-items)).

Contract items describe actual deployed contracts with a known contract ID and a list of available methods ([contract Items](#contract-items)).

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
       | <enum-ty>
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
<array-ty> ::= <ty> ( "[" <expr> | <enum-ty> "]" )+
```

An array dimension can be indexed using integers or using enum variants of a single enum type, depending on how the dimension is specified in the array type.

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

Call expressions are used to call functions and have the following syntax:

```ebnf
<call-expr> ::= <path> "(" [ <expr> "," ... ] ")"
```

For example: `x = foo(5, 2);`.

The type of the expressions passed as arguments must match the argument types of the called function. The return type of the function must also be appropriate for the calling context.

The order of argument evaluation is not specified.

#### Expression Precedence

The precedence of Yurt operators and expressions is ordered as follows, going from strong to weak. Binary Operators at the same precedence level are grouped in the order given by their associativity.

| Operator                         | Associativity        |
| -------------------------------- | -------------------- |
| Tuple field access expressions   | left to right        |
| Unary `-`, Unary `+`, `!`        |                      |
| `*`, `/`, `%`                    | left to right        |
| Binary `+`, Binary `-`           | left to right        |
| `==`, `!=`, `<`, `>`, `<=`, `>=` | Requires parentheses |
| `&&`                             | left to right        |
| `\|\|`                           | left to right        |

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

- Simultaneously binding a list of paths with a common prefix, using the glob-like brace syntax `use a::b::{c, d, e::f, g::h::i};`
- Simultaneously binding a list of paths with a common prefix and their common parent module, using the `self` keyword, such as use `a::b::{self, c, d::e};`.
- Rebinding the target name as a new local name, using the syntax `use p::q::r as x;`. This can also be used with the last two features: `use a::b::{self as ab, c as abc};`.
- Binding all paths matching a given prefix, using the asterisk wildcard syntax `use a::b::*;`.
- Nesting groups of the previous features multiple times, such as use `a::b::{self as ab, c, d::{*, e::f}};`.

### Variable Declaration Items

These are variables whose values may or may not be unknown for a given _instance_ for an intent. Solvers are required to find appropriate values for those variables with unknown values at compile-time.

Variable declaration items have the following syntax:

```ebnf
<let-item> ::= "let" <ident> ( ( ":" <ty> ) | ("=" <expr> ) | ( ":" <ty> "=" <expr> )
```

For example:

```rust
let x: int;
let y = 5;
```

Note that at least one of the type annotation and the initializing expression has to be present so that the type of the variable can be determined. This implies that `let x;` is not a valid variable declaration.

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

### Function Items

Function items describe user defined operations. They have the following syntax:

```ebnf
<function-sig> ::= "fn" <ident> "(" [ <param> "," ... ] ")" "->" <ty>

<function-item> ::= <function-sig> <block-expr>

<param> ::= <ident> ":" <ty>
```

For example, the following function checks that its argument is an even number:

```rust
fn even(x: int) -> bool {
    x % 2 == 0
}
```

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

A call to any of these functions can be made using a `<call-expr>` with the name of the contract used in `<path>`. For example, `MyToken::foo()`.

## Language Backend

The backend of the DSL (or API) that we're building is effectively a Constraint Programming Solver, simply referred to as a "solver" in this documentation. Note, this should not be confused with the "solver" agent in the network. There are numerous commercial solvers that can be targeted, such as [OR-Tools](https://developers.google.com/optimization/), [Geocode](https://www.gecode.org/), and [Chuffed](https://github.com/chuffed/chuffed). We could decide on a standardized JSON output that is generated by the compiler and that can be read and subsequently _solved_ by network participants using their solver of choice (not all solvers are created equal!).
