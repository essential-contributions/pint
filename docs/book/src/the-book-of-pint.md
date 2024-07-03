# The Book of Pint

Welcome to _The Book of Pint_, an introductory book about **Pint**. Pint is a **declarative**
programming language for building blockchain applications. The word declarative means that code
written in Pint is focused on the _logic_ and not the _execution_. In Pint, you can describe _what_
your program must accomplish rather than describing _how_ to accomplish it. This is in contrast with
_imperative_ blockchain languages, like [Solidity](https://soliditylang.org/), which requires
implementing algorithms in explicit steps.

In particular, Pint is a constraint-based language. A Pint program is essentially a collection of
predicates and each predicate is a collection of constraints. Because blockchain applications are
all about state transitions, constraints allow you to restrict how the state is allowed to change.
As a result, you do not have to explicitly express _how to_ change the state, but only _what state
changes are allowed_.

In general, this book assumes that you’re reading it in sequence from front to back. Later chapters
build on concepts in earlier chapters, and earlier chapters might not delve into details on a
particular topic but will revisit the topic in a later chapter. The book can also be used as a
reference, though it is not entirely comprehensive.
