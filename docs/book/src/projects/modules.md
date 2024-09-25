## Defining Modules

In this section, we’ll talk about modules and other parts of the module system, namely paths that
allow you to name items and the `use` keyword that brings a path into scope.

### Modules Cheat Sheet

Here we provide a quick reference on how modules, paths, and the `use` keyword work in the compiler,
and how most developers organize their code. We’ll be going through examples of each of these rules
throughout this chapter, but this is a great place to refer to as a reminder of how modules work.

- **Start from the package root**: When compiling a package, the compiler first looks for code to
  compile in the package root file, which is ` src/lib.pnt` for a library package or
  `src/contract.rs` for a contract package.
- **Declaring modules**: You can declare new modules by creating files for them in the appropriate
  directories. Say you want to declare a new `garden` module. You have two options:
  - You can create the file `src/garden.pnt` if you want the `garden` module to be a _single file_
    module. That is, if you don't want the module `garden` to have submodules.
  - You can create the file `src/garden/garden.pnt` if you want the module to be a _multi-file
    module_. That is, if you want the module `garden` to have submodules. The submodules of `garden`
    would then live in the `src/garden` directory.
- **Declaring submodules**: In any directory other than the package root directory, you can create
  new _submodules_. For example, say you want to declare a submodule of `garden` named `vegetables`.
  You have two options:
  - You can create the file `src/garden/vegetables.pnt` if you want the `vegetables` submodule to be
    a _single file_ submodule. That is, if you don't want the submodule `vegetables` to have its own
    submodules.
  - You can create the file `src/garden/vegetables/vegetables.pnt` if you want the `vegetables`
    submodules to be a _multi-file submodule_. That is, if you want the submodule `vegetables` to
    have its own submodules.
- **Paths to code in modules**: Once a module is part of your package, you can refer to code in that
  module from anywhere else in the same package. For example, an enum `Asparagus` in the garden
  vegetables module would be found at `::garden::vegetables::Asparagus`.
- **The `use` keyword**: Within a Pint file, the `use` keyword creates shortcuts to items to reduce
  repetition of long paths. For example, you can create a shortcut to
  `::garden::vegetables::Asparagus` using the statement `use ::garden::vegetables::Asparagus;`
  declared at global scope in a Pint file. From then on, you only need to write `Asparagus` to make
  use of that enum in this file.

Here, we create a contract package named `backyard` that illustrates these rules. The package's
directory, also named `backyard`, contains these files and directories:

```console
backyard
├── pint.toml
└── src
    ├── contract.pnt
    └── garden
        ├── garden.rs
        └── vegetables.rs
```

The package root file is `src/contract.rs` since this is a contract package. It contains:

```pint
{{#include ../../../../examples/backyard/src/contract.pnt:foo}}
```

The submodule `vegetables` which is defined in `src/garden/vegetables.pnt`, contains:

```pint
{{#include ../../../../examples/backyard/src/garden/vegetables.pnt}}
```
