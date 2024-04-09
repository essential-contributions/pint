### State Declaration Items

These are variables that represent blockchain _state_. State variables are _not_ decision variables and the solver is not required to find values for them as their true value is determined by the blockchain. That being said, state variables can still be used in [constraint items](./constraints.md) to enforce various restrictions on the current and [future state values](../expressions/atoms/prime.md).

State declaration items have the following syntax:

```bnf
<state-item> ::= "state" <ident> [ ":" <ty> ] "=" <expr> ";"
```

For example:

```pint
state x: int = MyPath::foo();
state y = MyPath::bar();
```

Note that, unlike [let declarations](./lets.md), the initializer of a state declaration is not optional.
