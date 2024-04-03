## High-level Intent Structure

A Pint intent consists of one or more Pint files according to the rules described in [Multi-File Intents](structure/multi_file.md). Each Pint file contains one or more `<item>`s:

```bnf
<pint-file> ::= <item>*
```

Items can occur in any order; identifiers need not be declared before they are used. Items have the following top-level syntax:

```bnf
<item> ::= <import-item>
         | <let-item>
         | <state-item>
         | <constraint-item>
         | <macro-item>
         | <enum-decl-item>
         | <solve-item>
         | <new-type-item>
```

Import items (`<import-item>`) import new items from a module/submodule or external library into the current module ([Import Items](items/imports.md)).

Let declaration items introduce decision variables and optionally constrain them to values ([Let Declaration Items](items/lets.md)).

State declaration items (`<state-item>`) introduce _state_ variables and constrain them to values ([State Declaration Items](items/states.md)).

Constraint items describe intent constraints ([Constraint Items](items/constraints.md)).

Macro items introduce new parameterized blocks of declarations or expressions which can be used to facilitate code re-use ([Macro Items](items/macros.md)).

Enum declaration items describe C-style enumerations ([Enum Declaration Items](items/enums.md)).

New Type items let you assign a new name to an existing type, simplifying complex type definitions or providing more context for certain types (["New Type" Items](items/new_types.md)).

Solve items specify exact what kind of solution the user is interested in: plain satisfaction, or the minimization/maximization of an expression. Each intent must have at most one solve item ([Solve Items](items/solve.md)).
