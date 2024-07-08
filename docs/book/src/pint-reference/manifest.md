# Manifest

The `pint.toml` manifest provides a high-level description of a package and its
dependencies. All projects built with the `pint` tool must have a `pint.toml`
manifest file.

The following provides a description of each of the tables and fields within
the manifest.

## `[package]`

The package table declares high-level information about the pint package. It includes the following entries:

### `name`

The name of the package.

```toml
name = "foo"
```

### `license`

Optionally specify the license for the package.

```toml
license = "MIT"
```

###  `kind`

Describes whether the package is a `"contract"` (the default) or a `"library"`.

- **library** packages allow for sharing types, macros and constants between
multiple different packages.
- **contract** packages describe a top-level contract that may be deployed.

```toml
kind = "contract"
```

### `entry-point`

Optionally specify the path to the entry-point module for the package relative
to the `src/` directory.

By default this is:

- `"contract.pnt"` for contracts and
- `"library.pnt"` for libraries.

```toml
entry-point = "path/to/my/contract.pnt"
```

## `[dependencies]`

Describes the list of external library packages that the package depends on.

Library packages allow for sharing types, macros and constants between
multiple different packages.

Dependencies are declared as follows:

```toml
[dependencies]
<dependency-name-1> = { <source-type> = "<source-value>" }
<dependency-name-2> = { <source-type> = "<source-value>" }
# etc
```

For example:

```toml
[dependencies]
bar = { path = "path/to/bar" }
```

### Source types

Currently only `path` dependencies are supported, though we plan to support more
source types in the future (e.g. `git`).

### `package` field

By default, the dependency name is assumed to match the name of the package that
we're depending on.

In some cases we may want to give a dependency a name that differs from its
package name (i.e. if multiple packages have the same name).

We can do this using the `package` field. In the following, we depend on a
package named `barney`, but give it the dependency name `bar`:

```toml
[dependencies]
bar = { path = "../path/to/bar", package = "barney" }
```

## `[contract-dependencies]`

Describes the list of external contract packages that the package depends on.

These are similar to `[dependencies]`, however rather than providing a
module of items like library dependencies do, contract dependencies only provide
the contract's address, along with the address of each of its predicates. E.g.

```toml
[contract-dependencies]
baz = { path = "path/to/baz" }
```

Now in our package, we can refer to baz's content address with the
`baz::ADDRESS` constant. Similarly, if `baz` has a predicate called `Foo`, we
can access `Foo`'s predicate address with `baz::Foo::ADDRESS`.

## Full Example

The following is an example of a Pint package manifest:

```toml
[package]
name = "foo"
license = "MIT"
kind = "contract"
entry-point = "path/to/my/contract.pnt"

[dependencies]
bar = { path = "../relative/path/to/bar", package = "barney" }

[contract-dependencies]
baz = { path = "/absolute/path/to/baz" }
```

## Developer Notes

The `pint.toml` manifest is implemented in the `pint-manifest` crate within the
`pint` repostiory. Rust developers looking to build `pint`-package aware tools
or plugins downstream might find this library useful.
