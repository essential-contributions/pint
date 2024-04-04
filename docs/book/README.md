# The Book of Pint

## Building From Source

Install `mdbook` and `mdbook-katex`:

```sh
cargo install mdbook
cargo install mdbook-katex
```

To build the book from the `docs/book` directory:

```sh
mdbook build
```

To serve locally from the `docs/book` directory:

```sh
mdbook serve
```

The book will then be accessible via the following URL:

```console
http://localhost:3000/
```
