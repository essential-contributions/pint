## Installation

### Using Nix (Recommended)

The easiest way to get started with Pint and the Essential ecosystem is to use
[Nix](https://nixos.org/). You first need to install Nix using:

```console
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
```

or using one of [these other alternatives](https://nixos.org/download/).

Then, enter a Nix shell as follows:

```console
nix develop github:essential-contributions/essential-integration#dev
```

This will make all the tools you need available in your terminal.

### Installing from `Cargo`

#### Dependencies

A prerequisite for installing `Pint` from `Cargo` is the Rust toolchain. Platform-specific
instructions for installing `rustup` can be found [here](https://www.rust-lang.org/tools/install).
Then, install the Rust toolchain with:

```console
$ rustup install stable
```

The Pint toolchain is built and tested against the latest [`stable` Rust toolchain
version](https://github.com/rust-lang/rust/releases/latest). Ensure you are using the latest
`stable` version of Rust with:

```console
$ rustup update && rustup default stable
```

#### Installation Steps

The Pint toolchain can be installed using `Cargo` with:

```console
$ cargo install pint-cli
```

You can update the toolchain with `Cargo` using:

```console
$ cargo update pint-cli
```

### Syntax Highlighting

Currently, Pint only has syntax highlighting support in Visual Studio Code. We are, however, in the
process of adding support for other editors.

#### Visual Studio Code

To install the Pint plugin, Search the Visual Studio Code market place for `pint syntax`.
Alternatively, use [this
link](https://marketplace.visualstudio.com/items?itemName=essential-contributions.pint-lang).
