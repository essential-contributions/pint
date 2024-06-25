## Appendix A: Keywords

The following list contains keywords that are reserved for current use by the Pint language. As
such, they cannot be used as identifiers. Identifiers are names of macros, variables, tuple fields,
modules, or types.

- `as` - perform primitive casting or rename items in `use` statements
- `bool` - the Boolean type
- `b256` - the 256-bit hash type
- `cond` - select between multiple expressions based on some conditions
- `constraint` - define a Boolean constraint that a proposed solution must satisfy
- `else` - fallback for `if` and `cond` conditionals
- `enum` - define an enumeration
- `exists` - existential quantification: checks whether a statements is `true` for at least one
  element in a domain.
- `forall` - universal quantification: checks whether a statement is `true` for all elements in a
  domain
- `if` - branch based on the result of a conditional expression
- `in` - checks if an element belongs to a range or to an array
- `int` - basic integer type
- `intent` - define an intent
- `interface` - declare an external interface
- `macro` - define a macro
- `maximize` - part of a `solve` statement: maximizes an objective function
- `minimize` - part of a `solve` statement: minimizes an objective function
- `real` - basic real type
- `satisfy` - part of a `solve` statement: satisfy the constraints in the predicate
- `self` - used in `use` statements
- `solve` - define what solve directive to use
- `state` - bind a state variable
- `storage` - declare a storage block
- `string` - basic string type
- `type` - define a new type
- `use` - bring symbols into scope
- `var` - bind a decision variable
- `where` - denote clauses that constraint generator indices
