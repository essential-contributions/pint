# The Yurt Compiler

## Dependencies

Yurt is built in Rust. To begin, install the Rust toolchain following instructions at <https://www.rust-lang.org/tools/install>. Then configure your Rust toolchain to use Rust `stable`:

```sh
rustup default stable
```

If not already done, add the Cargo bin directory to your `PATH` by adding the following line to `~/.profile` and restarting the shell session.

```sh
export PATH="${HOME}/.cargo/bin:${PATH}"
```

## Building `yurtc`

Clone the repository and build the Yurt compiler:

```sh
git clone git@github.com:essential-contributions/yurt.git
cd yurt/yurtc
cargo build
```

Confirm that `yurtc` built successfully:

```sh
cargo run --bin yurtc -- --help
```

## Testing

### Running Unit Tests

Unit tests can be run using `cargo test` in the `yurt/yurtc` directory. However, it is recommended that the tests are run using the [`cargo-nextest`](https://nexte.st/) package instead. To install `cargo-nextest`:

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
