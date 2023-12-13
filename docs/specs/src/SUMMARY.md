# Summary

[Yurt Specification](./yurt-spec.md)

- [Introduction](intro/introduction.md)
  - [State Transition Function](intro/state_transition_function.md)
  - [Transactions](intro/transactions.md)
- [Specification of Yurt](yurt/yurt_spec.md)
  - [Notation](yurt/notation.md)
  - [Overview of an Intent Model](yurt/overview.md)
  - [Syntax Overview](yurt/syntax.md)
  - [High-level Intent Structure](yurt/structure.md)
    - [Multi-File Intents](yurt/structure/multi_file.md)
    - [Namespaces and Shadowing](yurt/structure/namespaces.md)
  - [Types](yurt/types.md)
    - [Tuple Type](yurt/types/tuple.md)
    - [Array Type](yurt/types/array.md)
    - [Enum Type](yurt/types/enum.md)
    - [Custom Type](yurt/types/custom.md)
  - [Items](yurt/items.md)
    - [Imports](yurt/items/imports.md)
    - [Let Declarations](yurt/items/lets.md)
    - [State Declarations](yurt/items/states.md)
    - [Constraints](yurt/items/constraints.md)
    - [Macros](yurt/items/macros.md)
    - [Enum Declarations](yurt/items/enums.md)
    - ["New Type" Declarations](yurt/items/new_types.md)
    - [Solve Items](yurt/items/solve.md)
    - [Interfaces](yurt/items/interfaces.md)
    - [Contracts](yurt/items/contracts.md)
    - [Extern Items](yurt/items/externs.md)
  - [Expressions](yurt/expressions.md)
    - [Operators](yurt/expressions/operators.md)
    - [Expression Atoms](yurt/expressions/atoms.md)
      - [Blocks](yurt/expressions/atoms/block.md)
      - [Literals](yurt/expressions/atoms/literals.md)
      - [Tuples and Tuple Accesses](yurt/expressions/atoms/tuples.md)
      - [Arrays and Array Accesses](yurt/expressions/atoms/arrays.md)
      - [Ranges](yurt/expressions/atoms/ranges.md)
      - [Conditionals](yurt/expressions/atoms/conditionals.md)
      - [Casts](yurt/expressions/atoms/casts.md)
      - [Calls](yurt/expressions/atoms/calls.md)
      - ["In" Expressions](yurt/expressions/atoms/in.md)
      - [Prime Expressions](yurt/expressions/atoms/prime.md)
      - [Expression Precedence](yurt/expressions/atoms/precedence.md)
  - [Backend](yurt/backend.md)
- [JSON Representation of Intents](json/json_intents.md)