## Invoking Predicates

If you've worked with imperative smart contract languages like [Solidity](https://soliditylang.org/)
in the past, you know that interactions between smart contracts are essential for many applications.
One way to do this is by _calling_ one contract from another contract. Pint has a similar concept
where one predicate can _invoke_ another predicate with some arguments. We use the word "invoke"
here instead of "call" because, unlike imperative languages, predicates in Pint are not called in
the traditional sense since that would imply the existence of a control flow (a "call" followed by a
"return"). Instead, in Pint, predicate `A` can invoke predicate `B` with some arguments to indicate
that **`B` has to be present in the solution with this particular set of arguments, if `A` is also
present in the solution**. If this sounds confusing, don't worry. It will all be clear by the end of
this chapter.

We cover two types of predicate invocations:

1. External predicate invocation.
1. Sibling predicate invocation.

### Invoking External Predicates

Similar to accessing external storage, a predicate in an external contract can be invoked through an
`interface` declaration corresponding to the external contract. When invoking the external
predicate, the following are needed:

1. The address of the external contract.
1. The address of the predicate we want to access in that contract.
1. A list of arguments.

Consider the following external smart contract that we would like interact with:

```pint
{{#include ../../../../examples/invoking_predicates_1.pnt}}
```

To interact with this contract, we first need to generate its interface:

```pint
{{#include ../../../../examples/invoking_predicates_2.pnt:interface}}
```

Now, we can invoke predicate `foo` as follows:

```pint
{{#include ../../../../examples/invoking_predicates_2.pnt:invoking}}
```

The predicate invocation expression has three parts separated by `::`:

1. The name of the interface `MyInterface` followed by the address of the corresponding deployed
   contract in between `@[..]`. This is quite similar to what we had to do to access external
   storage variables.
1. The name of the predicate `foo` followed by the address of the corresponding predicate in the
   deployed contract.
1. A list of arguments that must match the list parameters of `foo` in the `interface`.

In the above, the constraint:

```pint
{{#include ../../../../examples/invoking_predicates_2.pnt:constraint}}
```

can be interpreted as follows: any solution that solves predicate `bar` must also contain a solution
for predicate `foo` such that:

1. The address of `foo` is `FOO_ADDR`.
1. The address of the contract that contains `foo` is `CONTRACT_ADDR`.
1. The arguments provided for `foo` in the solution are equal to `a`, `true`, and `tuple`
   respectively.

Of course, for `foo` to be actually satisfied, the containing contract must exist and be deployed,
the addresses must be correct, and the arguments must match (in number and types) what the deployed
bytecode of `foo` actually expects.

### Invoking Sibling Predicates

A special case for invoking predicates is when the invoked predicate is in the same contract as the
predicate that is invoking it. There is no conceptual difference between this special case and the
generalized case above expect that, because we are working in the same contract, there is no need to
declare an interface for the contract. There is also no need to specify the address of the invoked
predicate because the compiler can figure that out on its own! Here's an example:

```pint
{{#include ../../../../examples/invoking_predicates_2.pnt:sibling}}
```

Here, predicate `B` invokes _sibling_ predicate `A` by using its path (just `A`) and empty square
brackets `@[]` to indicate that the address is to be computed by the compiler. The constraint
`constraint A@[](..)` should be interpreted exactly as in the case of external predicate
invocation: any solution that solves `B` should also contain a solution for `A` with the provided
listed arguments.

One important caveat of the above is that you are not allowed to have _cyclical dependencies_
between predicates. For example:

```pint
{{#include ../../../../examples/invoking_predicates_3.pnt:cycle}}
```

Here, predicate `A` invokes predicate `B` and predicate `B` invokes predicate `A`. This causes a
cycle where the address of a predicate cannot be determined and used by the other. The compiler will
emit the following error to prevent you from proceeding:

```console
Error: dependency cycle detected between predicates
   ╭─[invoking_predicates_3.pnt:1:1]
   │
 1 │ predicate A(x: int) {
   │ ─────┬─────
   │      ╰─────── this predicate is on the dependency cycle
   │
 7 │ predicate B(y: int) {
   │ ─────┬─────
   │      ╰─────── this predicate is on the dependency cycle
   │
   │ Note: dependency between predicates is typically created via predicate instances
───╯
```

Another caveat is that a predicate cannot reference itself:

```pint
{{#include ../../../../examples/invoking_predicates_4.pnt:self}}
```

The compiler will complain as follows:

```console
Error: self referential predicate `::C`
   ╭─[invoking_predicates_4.pnt:2:16]
   │
 2 │     constraint C@[](x);
   │                ───┬───
   │                   ╰───── this predicate call references the predicate it's declared in
───╯
```
