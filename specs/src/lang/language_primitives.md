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
- Non-empty lists are written with an item, a separator/terminator terminal, and three dots. For example, `<expr> "," ...` is short for `<expr> ( "," <expr> )* [ "," ]`.

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

A number of keywords are reserved and cannot be used as identifiers. The keywords are: `bool`, `constraint`, `else`, `false`, `real`, `fn`, `if`, `int`, `let`, `maximize`, `minimize`, `satisfy`, `solve`, `true`.

## High-level Intent Structure

A Yurt intent consists of one or more semicolon separated `items`:

```ebnf
<intent> ::= [ <item> ";" ... ]
```

Items can occur in any order; identifiers need not be declared before they are used. Items have the following top-level syntax:

```ebnf
<item> ::= <let-item>
         | <var-item>
         | <constraint-item>
         | <function-item>
         | <solve-item>
         | <transition-item>
```

Variable declaration items (`<let-item>` and `<var-item`) introduce variables and optionally bind them to a value ([Variable Declaration Items](#variable-declaration-items)).

Constraint items describe intent constraints ([Constraint Items](#constraint-items)).

Function items introduce new user-defined functions which can be called in expressions ([Function Items](#function-items)).

Solve items specify exact what kind of solution the user is interested in: plain satisfaction, or the minimization/maximization of an expression. Each intent must have at most one solve item ([Solve Items](#solve-items)).

Transition items describe the state transition function of a blockchain ([Transition Items](#transition-items))

### Multi-file Intents

TODO

### Namespaces and Scopes

TODO

## Types

Yurt provides 4 scalar types built-in: Booleans, integers, reals and strings. Yurt also provides tuples as a compound built-int type.

The syntax for a types is as follows:

```ebnf
<ty> ::= "bool"
       | "int"
       | "real"
       | "string"
       | <tuple-ty>

<tuple-ty> ::= "(" ( <ty> "," ... ) ")"
```

For example, in `let t: (int, real, string) = (5, 3.0, "foo")`, `(int, real, string)` is a tuple type.

## Expressions

### Expressions Overview

Expressions represent values and have the following syntax:

```ebnf
<expr> ::= <expr-atom> <expr-binop-tail>

<expr-binop-tail> ::= [ <bin-op> <expr> ]

<expr-atom> ::= <un-op> <expr-atom>
              | <block-expr>
              | "(" <expr> ")"
              | <ident>
              | <bool-literal>
              | <int-literal>
              | <real-literal>
              | <string-literal>
              | <tuple-expr>
              | <tuple-index-expr>
              | <if-expr>
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
```

#### Operator Precedence

The operators have the following precedence, from highest to lowest.

| Class          | Operators                        |
| -------------- | -------------------------------- |
| Multiplicative | `*`, `/`, `%`                    |
| Additive       | `+` (binary), `-` (binary)       |
| Comparison     | `<`, `>`, `<=`, `>=`, `==`, `!=` |

### Expression Atoms

#### Block Expressions

Block expressions are expressions that contains a list of _statements_ followed by an expression within curly bracket `{ .. }`. Formally:

```ebnf
<block-expr> ::= "{" [ <block-statement> ";" ... ] <expr> "}"

<block-statement> ::= <let-item>
                    | <var-item>
                    | <constraint-item>
                    | <if-expr>
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

#### Tuple Expressions and Tuple Indexing Expressions

Tuple Expressions are written as:

```ebnf
<tuple-expr> ::= "(" <expr> "," [ <expr> "," ... ] ")"
```

For example: `let t = (5, 3, "foo");`.

Tuple indexing expressions are written as:

```ebnf
<tuple-index-expr> ::= <expr-atom> "." [0-9]+
```

For example: `let second = t.1;` which extracts the second element of tuple `t` and stores it into `second`.

#### "If" Expressions

Yurt provides `if` expressions which provide selection from two alternatives based on a condition. They have this syntax:

```ebnf
<if-expr> ::= "if" <expr> <block-expr> "else" <block-expr>
```

The condition `<expr>` above must be of type `bool`. The "then" and "else" block expressions must have the same type or be coercible to the same type, which is also the type of the whole `if` expression.

Note that the `else` block is not optional and the `else if { .. }` syntax is not supported.

#### Call Expressions

Call expressions are used to call functions and have the following syntax:

```ebnf
<call-expr> ::= <ident> "(" ( <expr> "," ... ) ")"
```

For example: `x = foo(5, 2);`.

The type of the expressions passed as arguments must match the argument types of the called function. The return type of the function must also be appropriate for the calling context.

The order of argument evaluation is not specified.

## Items

This section describes the top-level program items.

### Variable Declaration Items

There are two types of variable declarations:

#### Configuration variables

These are variables whose values are fixed for each given _instance_ of an intent.

Configuration variables have the following syntax:

```ebnf
<let-item> ::= "let" <ident> [ ":" <ty> ] "=" <expr>
```

For example:

```rust
let a:int = 10;
let b = 5;
```

#### Decision variables

These are variables whose values can be unknown for a given _instance_ for an intent. Solver are required to find appropriate values for these variables.

Decision variables have the following syntax:

```ebnf
<var-item> ::= "var" <ident> ( ( ":" <ty> ) | ("=" <expr> ) | ( ":" <ty> "=" <expr> )
```

For example:

```rust
var x: int;
let y = 5;
```

The optional value used for initializing a decision variable enforce an equality constraint on the variable. For example, the following:

```rust
var x: int = 5;
```

is equivalent to

```rust
var x: int;
constraint x == 5;
```

Note that at least one of the type annotation and the initializing expression has to be present so that the type of the variable can be determined. This implies that `var x;` is not a valid variable declaration.

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

### Function Items

Function items describe user defined operations. They have the following syntax:

```ebnf
<function-item> ::= "fn" <ident> "(" ( <param> "," ... ) ")" "->" <ty> <block-expr>

<param> ::= <ident> ":" <ty>
```

For example, the following function checks that its argument is an even number:

```rust
fn even(x: int) -> bool {
    x % 2 == 0
}
```

### Transition Items

Transition items represent a relationship between two decision variables that represent the state of a blockchain such as balances. Transition items have the following syntax:

```ebnf
<transition-item> ::= <ident> "~>" <ident>
```

For example:

```rust
bal0 ~> bal1
```

Here, `bal1` represents the _next_ value of `bal0` based on the state transition function of the blockchain where `bal0` lives.

## Language Backend

The backend of the DSL (or API) that we're building is effectively a Constraint Programming Solver, simply referred to as a "solver" in this documentation. Note, this should not be confused with the "solver" agent in the network. There are numerous commercial solvers that can be targeted, such as [OR-Tools](https://developers.google.com/optimization/), [Geocode](https://www.gecode.org/), and [Chuffed](https://github.com/chuffed/chuffed). We could decide on a standardized JSON output that is generated by the compiler and that can be read and subsequently _solved_ by network participants using their solver of choice (not all solvers are created equal!).
