## Pint Packages

The first part of the module system we’ll cover are packages.

A package is a bundle of one or more modules that provides a set of functionality. A package
contains a `pint.toml` file that describes how to build it.

Packages can come in one of two forms: a "contract" package and a "library" package. _Contract
packages_ represent smart contracts that can be compiled to bytecode and deployed to a blockchain.
Each contract package must have a `contract.pnt` file in its `src` directory, which represents the
entry point of the contract. _Library packages_ don't compile to bytecode. Instead, they define
functionality intended to be shared with multiple projects. The entry point of a library crate is
always a `lib.pnt` file in the `src` directory.

Let's walk through what happens when we create a package. First, we enter the command `pint new
my-project`:

```console
$ pint new my-project
     Created my-project [contract] (/path/to/my-project)
$ ls my-project
pint.toml src
$ ls my-project/src
contract.pnt
```

After we run `pint new`, we use `ls` to see what the pint tool creates. In the project directory,
there's a `pint.toml` file, giving us a package. There's also an `src` directory that contains
`contract.pnt`. Open `pint.toml` in your text editor, and note there's no mention of
`src/contract.pnt`. The pint tool follows a convention that `src/contract.pnt` is the root file of a
contract package. Likewise, `src/lib.pnt` is the root file of a library package.

Note that the `pint.toml` file does contain a field called `kind`, which is set to `contract` in the
example above, to indicate that this particular package is a contract. In contrast, if we were to
create a library package using `pint new my-library --lib`, then we would find that the `kind` field
in the generated `pint.toml` is set to `library`.
