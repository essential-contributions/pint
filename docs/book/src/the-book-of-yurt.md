# The Book of Yurt

Welcome to _The Book of Yurt_, an introductory book about the programming language **Yurt**. The Yurt programming language is a free and open-source **constraint modeling language**. Yurt can be used to model constraint satisfaction and optimization problems in a **high-level**, **solver-independent** way. What differentiates Yurt from other constraint modeling languages is that Yurt provides native syntax for reading and constraining **blockchain state** elements. Yurt also provides a suite of features you'd expect from a modern programming language, such as type safety, user-defined types, extensibility via user-defined macros and libraries, functional elements (`fold`, `map`, etc.), list comprehensions, and many others.

A Yurt program is a **collection of constraints** on the solution space for a set of decision variables. Some of these decision variables represent the state of a blockchain at different points in time, allowing the constraints to restrict how the state is allowed to flow from one timestamp to the next.

Constraint-based languages differ from imperative programming languages in that they do not specify explicit steps or instructions to be executed, but rather the properties of a solution to be found.

## Who This Book Is For

This book assumes that you’ve written code in another programming language but doesn’t make any assumptions about which one. More specifically, the book **does not** assume that you've used any constraint-based programming languages in the past. Constraint programming requires a different thinking process than imperative programming, which is something that this book tries to help you with.
