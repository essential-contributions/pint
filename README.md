# The Yurt Programming Language

[![build](https://github.com/essential-contributions/DSL/actions/workflows/ci.yml/badge.svg)](https://github.com/essential-contributions/DSL/actions/workflows/ci.yml)

Yurt is a Domain Specific Language (DSL) for intent expression using constraints on state transitions on arbitrary blockchain architecture.

## Dependencies

### Rust

Yurt is built in Rust. To begin, install the Rust toolchain following instructions at <https://www.rust-lang.org/tools/install>. Then configure your Rust toolchain to use Rust `stable`:

```sh
rustup default stable
```

If not already done, add the Cargo bin directory to your `PATH` by adding the following line to `~/.profile` and restarting the shell session.

```sh
export PATH="${HOME}/.cargo/bin:${PATH}"
```

### SCIP

Yurt uses [SCIP](https://www.scipopt.org/) as its default solver. To install SCIP, use your favourite package manager and, if needed, set the environment variable `SCIPOPTDIR` to where SCIP is installed.

For example, Homebrew can be used to install SCIP on MacOS:

```sh
brew install scip
export SCIPOPTDIR=<scip-install-dir> # Example: /opt/homebrew/Cellar/scip/8.1.0
```

If SCIP is not available with your package manager, you may use [Miniconda](https://docs.conda.io/projects/miniconda/en/latest/).

## Building

Clone the repository and build the Yurt compiler and tooling:

```sh
git clone git@github.com:essential-contributions/yurt.git
cd yurt
cargo build
```

Confirm that `yurtc` and `yurtfmt` built successfully:

```sh
cargo run --bin yurtc -- --help
cargo run --bin yurtfmt -- --help
```

## Testing

### Running Unit Tests

Unit tests can be run using `cargo test` in the `yurt` directory. However, it is recommended that the tests are run using the [`cargo-nextest`](https://nexte.st/) package instead. To install `cargo-nextest`:

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
