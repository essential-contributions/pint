## High-level Intent Structure

A Yurt intent consists of one or more Yurt files according to the rules described in [Multi-File Intents](structure/multi_file.md). Each Yurt file contains one or more `<item>`s:

```bnf
<yurt-file> ::= <item>*
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
         | <interface-item>
         | <contract-item>
         | <extern-item>
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

Interface items contain lists of smart contract methods that a [contract](items/contracts.md) can have ([Interface Items](items/interfaces.md)).

Contract items describe actual deployed contracts with a known contract ID and a list of available methods ([contract Items](items/contracts.md)).

"Extern" items contain lists of external functions that allow accessing data on a blockchain (["Extern" Items](items/externs.md)).
