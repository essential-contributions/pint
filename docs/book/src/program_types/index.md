# Program Structure

There are two kinds of Pint programs that can be written:

- **Stateless Programs**: These are programs that do not own state and only reason about the state
  of other programs. These can be useful, for example, for users who wish to express their intents
  regarding some on-chain assets.
- **Smart Contracts**: These are programs that own state and manage it using a collection of
  _predicates_. Each predicate is a collection of constraints and a valid solution must satisfy all
  the constraints in one of the predicates in the contract.

In this chapter, we will go over both types of programs in detail.
