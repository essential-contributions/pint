# Commands

The following inlines the `--help` output for each command so that you can get
an idea of what `pint` is capable of prior to downloading the tool and running
it yourself.

| Command                         | Short Description              |
| ------------------------------- | ------------------------------ |
| [`pint build`](#pint-build)     | Build a package.               |
| [`pint new`](#pint-new)         | Create a new package.          |
| [`pint plugins`](#pint-plugins) | List all pint plugins on path. |

## Overview

```console
$ pint --help
Pint's package manager and CLI plugin host

Usage: pint <COMMAND>

Commands:
  build    Build a package, writing the generated artifacts to `out/`
  new      Create a new package
  plugins  Print all pint plugins found in `PATH`
  help     Print this message or the help of the given subcommand(s)

Options:
  -h, --help     Print help
  -V, --version  Print version
```

## `pint build`

```console
$ pint build --help
Build a package, writing the generated artifacts to `out/`

Usage: pint build [OPTIONS]

Options:
      --manifest-path <MANIFEST_PATH>
          The path to the package manifest.

          If not provided, the current directory is checked and then each parent recursively until a manifest is found.

      --salt <SALT>
          A 256-bit unsigned integer in hexadeciaml format that represents the contract "salt". The value is left padded with zeros if it has less than 64 hexadecimal digits.

          The "salt" is hashed along with the contract's bytecode in order to make the address of the contract unique.

          If "salt" is provided for a library package, an error is emitted.

      --print-parsed
          Print the parsed package

      --print-flat
          Print the flattened package

      --print-optimized
          Print the optimized package

      --print-asm
          Print the assembly generated for the package

      --silent
          Don't print anything that wasn't explicitly requested

  -h, --help
          Print help (see a summary with '-h')
```

## `pint new`

```console
$ pint new --help
Create a new package

Usage: pint new [OPTIONS] <PATH>

Arguments:
  <PATH>
          The directory path in which the package should be created

Options:
      --contract
          Specify the "contract" package kind.

          This is the default behaviour.

      --lib
          Specify the "library" package kind.

          By default, new packages are created with the "contract" kind.

      --name <NAME>
          Optionally provide a name.

          By default, the package name is the last directory in the canonicalized representation of the given path.

  -h, --help
          Print help (see a summary with '-h')
```

## `pint plugins`

```console
$ pint plugins --help
Print all pint plugins found in `PATH`

Usage: pint plugins

Options:
  -h, --help  Print help
```
