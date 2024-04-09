## Syntax Overview

### Character set

Pint input files must be encoded as UTF-8 and may have the extension `*.pnt`.

Pint is case sensitive and has no layout restrictions. That is, pieces of whitespace containing spaces, tabs, and newlines are all equivalent to each other.

### Comments

A `//` indicates that the rest of the line is a comment.

### Identifiers

Identifiers have the following syntax:

```bnf
<ident> ::= [A-Za-z_][A-Za-z_0-9]*     % excluding keywords
```

For example, `___j_Bond_007_` is a valid identifier.

A number of keywords are reserved and cannot be used as identifiers. The keywords are: `as`, `bool`, `cond`, `constraint`, `else`, `enum`, `false`, `fn`, `if`, `in`, `int`, `let`, `macro`, `maximize`, `minimize`, `real`, `satisfy`, `self`, `solve`, `state`, `string`, `true`, `type`, `use`.

### Paths

A path is a sequence of one ore more path segments logically separated by a namespace qualifier (`::`). If a path consists of only one segment, it refers to either an item or a variable in a local control scope. If a path has multiple segments, it always refers to an item.

Paths have the following syntax:

```bnf
<path> ::= [ "::" ] <ident> ( "::" <ident> )*
```

For example, the following are all valid paths:

```pint
::x
x::y::z
x
```

Paths can be used in [Import Items](items/imports.md). They can also be used in expressions to directly refer to an item in a different module.

Paths that start with `::` are absolute paths from the root of the project. Paths that don't start with `::` are relative paths.
