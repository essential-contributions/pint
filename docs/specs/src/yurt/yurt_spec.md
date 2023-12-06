# Specification of Yurt

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

This document has the following structure. [Notation](notation.md) introduces the syntax notation used throughout the specification. [Overview of an Intent Model](overview.md) provides a high-level overview of Yurt intent models. [Syntax Overview](syntax.md) covers syntax basics. [High-level Intent Structure](structure.md) covers high-level structure: items, multi-file intents, namespaces, and name shadowing. [Types](types.md) introduces available types. [Expressions](expressions.md) covers expressions. [Items](items.md) describes the top-level items in detail.
