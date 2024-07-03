# The Pint Programming Language

[![build](https://github.com/essential-contributions/DSL/actions/workflows/ci.yml/badge.svg)](https://github.com/essential-contributions/DSL/actions/workflows/ci.yml)

Pint is a **Declarative** constraint-based Domain Specific Language (DSL) for intent expression.
An introduction to the Pint language can be found in [**The Book of Pint**](https://essential-contributions.github.io/pint/book/).

## Dependencies

### Rust

Pint is built in Rust. To begin, install the Rust toolchain following instructions at <https://www.rust-lang.org/tools/install>. Then configure your Rust toolchain to use Rust `stable`:

```sh
rustup default stable
```

If not already done, add the Cargo bin directory to your `PATH` by adding the following line to `~/.profile` and restarting the shell session.

```sh
export PATH="${HOME}/.cargo/bin:${PATH}"
```

## Building

Clone the repository and build the Pint compiler and tooling:

```sh
git clone git@github.com:essential-contributions/pint.git
cd pint
cargo build
```

Confirm that `pintc` and `pintfmt` built successfully:

```sh
cargo run --bin pintc -- --help
cargo run --bin pintfmt -- --help
```

## Testing

### Running Unit Tests

Unit tests can be run using `cargo test` in the `pint` directory. However, it is recommended that the tests are run using the [`cargo-nextest`](https://nexte.st/) package instead. To install `cargo-nextest`:

```sh
cargo install cargo-nextest
```

To run all unit tests using `cargo-nextest`:

```sh
cargo nextest run
```

### Updating Unit Tests

Most unit tests are written with the help of the [`expect_test`](https://docs.rs/expect-test/latest/expect_test/) crate. The following command can be used to automatically update all unit tests that use the `expect_test::expect!` macro such that they all pass.

```sh
env UPDATE_EXPECT=1 cargo nextest run
```

For compiler changes that affect many unit tests, the command above allows updating all affected tests in one go. The command also helps with writing new tests: simply write your test by passing an empty string argument to the `expect!` macro (i.e. `expect![""]`) and then run the command above.
