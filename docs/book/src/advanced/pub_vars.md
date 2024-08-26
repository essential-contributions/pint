## Public Decision Variables

By default, all decision variables are private. That is, they are only accessible from within the
predicate that declares them. There is no way to reason about the value of a private decision
variable from _outside_ the predicate. This, however, may not be sufficient to build certain
applications. It also imposes limitations on how contracts are written: if a predicate requires data
from another predicate, then the two predicates must be combined.

We've already looked at one class of variables that are always public: **storage variables**. While
storage variables allow us to share data between predicates, they are the wrong tool for our use
case because they are persistent. What we want is a class of variables that can be shared between
predicates but that are _temporary_. That is, their value should no longer be relevant after a
solution is submitted and validated. Pint solves this problem by allow decision variables to be
**public** via the `pub` keyword.

### Declaring Public Decision Variables

Declaring public decision variables is fairly simple. All you have to do is prefix the `var`
declaration with the keyword `pub`:

```pint
{{#include ../../../../examples/ch_7_1.pnt:predicate_foo}}
```

In the predicate above, we are declaring 3 public decision variables and 3 private decision
variables.

Accessing public decision variables within the predicate that defines them is no different from
accessing private decision variables. The example above also declares 3 constraints that access both
types of decision variables in various ways.

### Public Decision Variables in Interfaces

Recall that for each contract, an interface can be produced that exposes all public elements of a
contract: the storage block as well as all the predicates and their public decision variables. For
example, the following interface can be produced from the contract above:

```pint
{{#include ../../../../examples/ch_7_1.pnt:interface}}
```

Note that everything in predicate `Foo` was dropped except for `pub var` declarations.

Next, we will show how to use an interface to access public decision variables of an external
predicate.

### External Access to Public Decision Variables

In [Chapter 5.3](../storage/external.md) we covered how to use interfaces to access storage
variables from an external context. Accessing public decision variables from an external context
requires similar steps.

First, we need to declare an interface instance using the syntax we learned in [Chapter
5.3](../storage/external.md)

```pint
{{#include ../../../../examples/ch_7_1.pnt:interface_instance}}
```

where `ContractID` is the address of the contract where the public decision variables are declared.
Then, and because the public decision variables live inside a predicate, we need to declare a
**predicate instance** as follows:

```pint
{{#include ../../../../examples/ch_7_1.pnt:predicate_instance}}
```

Similarly to interface instance declarations, a predicate instance declaration requires an address.
Each predicate has a 256-bit address that identifies it on the blockchain. The above declares an
interface instance called `FooInstance` of predicate `Foo` and with address `PredicateID`. Note that
we refer to the predicate `Foo` in the declaration using the interface instance name followed by
`::Foo`.

Now that we have an instance of `Foo`, we are able to access its public decision variables using a
path that contains the predicate instance name and the name of the variable:

```pint
{{#include ../../../../examples/ch_7_1.pnt:access}}
```

### Public Decision Variables in Solutions

Sharing data between two predicates using public decision variables only makes sense if the solution
that we are trying to validate actually solves both predicates. For example, if predicate `Foo`
exposes `pub var x;` and predicate `Bar` uses `x`, then a solution that solves `Bar` must also solve
`Foo` since `Bar` requires data from `Foo`, namely `x`.

Every solution contains a list of the predicates it solves. Some predicates might even be solved
multiple times in the same solution! This may be surprising to you but you can imagine multiple
instances of the same predicate being solved with different values. After all, predicates can
generally be solved in many different ways depending on how restrictive their constraints are.

As a result, each predicate instance declaration must somehow specify which part of the solution it
is referring to. The Pint compiler accomplishes this by adding an _implicit_ decision variable that
represents the **index** of the solved predicate in the solution. An implicit decision variable is
added for each predicate instance declaration. Therefore, in the example below:

```pint
{{#include ../../../../examples/ch_7_1.pnt:multiple_instances}}
```

the variables `x1` and `x2` may not be the same since they may refer to two different values of `x`
in the solution (if `Foo` is solved multiple times).

### Accessing Public Decision Variables From a Sibling Contract

Public decision variables, being _public_, are also visible from predicates in the _same_ contract
as the predicate that declares them. While working within a single contract, you may want one
predicate to access another predicate's public decision variables. This can be done, again, using
predicate instances. The difference here is that, because we're working in the same contract, there
is no need to declare an interface for that contract and an interface instance. There is also no
need to specify an address for the predicate instance because the compiler can figure that out on
its own! Here's an example:

```pint
{{#include ../../../../examples/ch_7_1.pnt:siblings}}
```

Here, `AI1` and `AI2` are two instances of predicate `A` which exposes a single public decision
variable called `x`. We are then requiring that the value of `x` in the first instance is double the
value of `x` in the second instance.

One important caveat of the above is that you are not allowed to have _cyclical dependencies_
between predicates. For example:

```pint
{{#include ../../../../examples/ch_7_1_b.pnt:cycle}}
```

Here, predicate `A` requires predicate `B` and predicate `B` requires predicate `A`. This causes a
cycle where the addresses of a predicate cannot be determined and used by the other. The compiler
will emit the following error to prevent you from proceeding:

```console
Error: dependency cycle detected between predicates
   ╭─[ch_7_1_b.pnt:3:1]
   │
 3 │ predicate A {
   │ ─────┬─────
   │      ╰─────── this predicate is on the dependency cycle
   │
10 │ predicate B {
   │ ─────┬─────
   │      ╰─────── this predicate is on the dependency cycle
   │
   │ Note: dependency between predicates is typically created via predicate instances
───╯
```

Another caveat is that a predicate cannot reference itself:

```pint
{{#include ../../../../examples/ch_7_1_c.pnt:self_ref}}
```

The compiler will complain as follows:

```console
Error: self referential predicate `C`
   ╭─[ch_7_1_c.pnt:6:5]
   │
 6 │     predicate CI = C();
   │     ─────────┬────────
   │              ╰────────── this predicate instance references the predicate it's declared in
───╯
```
