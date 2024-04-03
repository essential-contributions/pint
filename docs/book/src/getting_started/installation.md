## Installation

### Dependencies

A prerequisite for installing and using `Pint` is the Rust toolchain. Platform-specific instructions for installing `rustup` can be found [here](https://www.rust-lang.org/tools/install). Then, install the Rust toolchain with:

```console
$ rustup install stable
```

The Pint toolchain is built and tested against the latest [`stable` Rust toolchain version](https://github.com/rust-lang/rust/releases/latest). Ensure you are using the latest `stable` version of Rust with:

```console
$ rustup update && rustup default stable
```

### Installing from `Cargo`

The Pint toolchain can be installed using `Cargo` with:

```console
$ cargo install pint
```

You can update the toolchain with `Cargo` using:

```console
$ cargo update pint
```
