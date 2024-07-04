## Quickstart Guide

Now that you’ve installed Pint, it’s time to write your first Pint contract. The first contract we
will implement is a counter.

### Creating a Project Directory

You'll start by making a directory to store your Pint code. Open a terminal and enter the following
commands to make a `projects` directory:

```console
mkdir ~/projects
cd ~/projects
```

Now, we will use the `pint` tool to create a new project in the `projects` directory:

```console
pint new counter
cd counter
```

The `pint new` commands creates a new Pint project with the provided name. In the newly created
directory `counter`, you should find a `pint.toml` file as well as a `src/` directory with a single
file in it called `contract.pnt`:

```console
projects
└── counter
    ├── pint.toml
    └── src
        └── contract.pnt
```

The auto-generated `pint.toml` file describes how to build the project. The `src/contract.pnt` file is
the root file of a contract project, i.e., this is where the compilation starts when we build the
project.

Open the `pint.toml` and inspect its content:

```toml
{{#include ../../../../examples/ch_1_2/pint.toml}}
```

The one thing to note is that the `kind` field in this `pint.toml` is set to `contract`. This is the
default project kind. Alternatively, `kind` can be a _library_.

### Writing a Pint Program

Next, open the `src/contract.pnt` file and erase its content. Then, paste the following:

```pint
{{#include ../../../../examples/ch_1_2/src/contract.pnt}}
```

This is a contract with a single predicate and a single storage variable. The storage variable
`counter`, of type `int` (i.e. integer), is declared in a `storage` block. The only predicate in
this contract is called `Increment` and contains two statements:

1. A `state` declaration which reads the variable `counter`.
1. A `constraint` statement which enforces some logic on the state of the contract.

The above constraint is essentially saying: "if `counter` hasn't been set yet (i.e. is `nil`), set
the new value of `counter` to `1` (`counter' == 1`). Otherwise, increment `counter` by `1`".

Don't worry if this looks a bit overwhelming! We will later dive into each feature in this contract
separately.

### Building the Project

To build the project above, simply run the following command in the `counter` directory:

```console
pint build
```

You should get something like this:

```console
   Compiling counter [contract] (/path/to/counter)
    Finished build [debug] in 7.727125ms
    contract counter            6B81816FAF32033F860130547AB53F5BB61FB7820B17E3363D794D78C654283D
         └── counter::Increment 865DFE8E296A1E0F94EBB627EA89E7608F83C442D3A01BC5C2F271A972164904
```

Note the two 256-bit numbers in the output. These represent the _content hash_ (i.e. the `sha256` of
the bytecode) of the `counter` contract (as a whole) and the `Increment` predicate, respectively.
These "addresses" will be used when referring to the contract or the predicate (in a proposed
solution for example).

In the `counter` directory, you will also notice a new directory called `out`. Navigate to
`out/debug` and inspect the two `json` files that you see.

- `counter.json` represents the compiled bytecode in JSON format, which is the most important
  artifact produced by the compiler. This file is used when validating a solution. That is, when a
  solution is submitted, this file contains the bytecode that "runs" and decides whether all the
  relevant constraints are valid.
- `counter-abi.json` is the Application Binary Interface (ABI) of the contract. It basically
  describes how to interact with the contract from an external application or another contract. For
  example, while crafting a solution, the ABI can be used to figure out where the various storage
  variables are stored (i.e. their keys) and their types. This information is crucial to form
  correct solutions.

> **Note**: [Appendix C](../appendix/abi.md) contains the ABI spec.

Now that we have built and inspected the artifacts of our project, we can proceed to build an
application that interacts with this contract. We won't cover this topic here, but you can check out
this [Getting Started with Essential
Application](https://essential-contributions.github.io/essential-integration/index.html) book, which
covers this exact same "counter" example and how to build a Rust application that interacts with it
using the Essential VM.
