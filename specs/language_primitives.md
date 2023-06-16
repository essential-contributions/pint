# Specification of Yurt

## Introduction

This document defines the Yurt programming language and its primitives. It serves as a reference for the syntax and the semantics for each of its elements.

TODO

## Notation

TODO - EBNF

## Overview of an Intent

TODO

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

A number of keywords are reserved and cannot be used as identifiers. The keywords are: `bool`, `constraint`, `else`, `enum`, `false`, `float`, `fn`, `if`, `int`, `let`, `maximize`, `minimize`, `satisfy`, `solve`, `true`, `type`.

## High-level Intent Structure

A Yurt intent consists of one or more semicolon separated `items`:

```ebnf
<intent> ::= [ <item> ";" ... ]
```

Items can occur in any order; identifiers need not be declared before they are used. Items have the following top-level syntax:

```ebnf
<item> ::= <var-decl-item>
         | <assign-item>
         | <constraint-item>
         | <function-item>
         | <solve-item>
         | <transition-item>
```

Variable declaration items introduce new decision variables and possible bind them to a value ([Variable Declaration Items](#variable-declaration-items)).

Assignment items bind values to variables ([Assignment Items](#assignment-items)).

Constraint items describe intent constraints ([Constraint Items](#constraint-items)).

Function items introduce new user-defined functions which can be called in expressions ([Function Items](#function-items)).

Solve items specify exact what kind of solution the user is interested in: plain satisfaction, or the minimization/maximization of an expression. Each intent must have at most one solve item ([Solve Items](#solve-items)).

Transition items describe the state transition function of a blockchain ([Transition Items](#transition-items))

TODO - more items

### Multi-file Intents

TODO

### Namespaces and Scopes

TODO

## Types

Yurt provies 4 scalar types built-in: Booleans, integers, floats, and strings. Yurt also provides

- Two compound built-in types: arrays and tuples
- Structs (`struct`)
- Enumerated types (`enum`)
- Type aliases (`type`)

TODO - lots more details

## Expressions

Expressions represent values and have the following syntax:

```ebnf
<expr> ::= <expr-atom> <expr-binop-tail>

<expr-binop-tail> ::= [ <bin-op> <expr> ]

<expr-atom> ::= "(" <expr> ")"
              | <ident>
              | <bool-literal>
              | <int-literal>
              | <float-literal>
              | <string-literal>
              | <tuple-literal>
              | <call-expr>

<bin-op> ::= "<" | ">" | "<=" | ">=" | "==" | "!="
           | "+" | "-" | "*" | "/" | "%"

<bool-literal> ::= "false" | "true"

<int-literal> ::= [0-9]+
                | 0x[0-9A-Fa-f]+

<float-literal> ::= [0-9]+"."[0-9]+
                  | [0-9]+"."[0-9]+[Ee][-+]?[0-9]+
                  | [0-9]+[Ee][-+]?[0-9]+

<string-literal> ::= "\"" ([^"\n] | "\\" (x[0-9a-fA-F][0-9a-fA-F] | "n" | "t" | "\"" | "\\")) "\""

<tuple-literal> ::= "(" <expr> "," [ <expr> "," ... ] ")"

<call-expr> ::= <ident> "(" [ <expr>, "," ... ] ")"
```

TODO - more expressions

### Operator Precedence

The operators have the following precedence, from highest to lowest.

| Class          | Operators                        |
| -------------- | -------------------------------- |
| Multiplicative | `*`, `/`, `%`                    |
| Additive       | `+`, `-`                         |
| Comparison     | `<`, `>`, `<=`, `>=`, `==`, `!=` |

### Variable Declaration Items

Variable declarations have the following syntax:

```ebnf
<var-decl-item> ::= "let" <ident> [ ":" <ty> ] [ "=" <expr> ]
```

For example:

```rust
let a:int = 10;
let b = 5;
```

A variable whose declaration does not include an assignment can be initialized by a separate assignment item ([Assignment Items](#assignment-items)). For example, the above items can be separated into the following items:

```rust
let a:int;
a = 10;
let b;
b = 5;
```

Variables can only be assigned once in an intent.

### Assignment Items

Assignments have this syntax:

```ebnf
<assign-item> ::= <ident> "=" <expr>
```

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
<function-item> ::= "fn" <ident> "(" ( <param> "," ... ) ")" "->" <ty> <block-exp>

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
