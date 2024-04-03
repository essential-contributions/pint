### Multi-File Intents

An intent can be spread across multiple files. Each file implicitly declares a local module or submodule dependency.

#### Entry File

Each multi-file Pint project has an entry file. Compiling the entry file (e.g. using `pintc <filename>`) should also compile all local modules and submodules that the entry file directly or indirectly depends on.

#### Declaring modules

In the project root directory, new modules can be created as follows:

- A single-file module must be defined in a file that has the same name as the module itself. For example, module `single_mod` must be defined in `src/single_mod.pnt`. The absolute [path](../syntax.md#paths) to items in `single_mod.pnt` is `::single_mod::<item>`.
- A multi-file module must be defined inside a directory that has the same name as the module itself. Moreover, the entry file of the module must also have the same name as the module. For example, multi-file module `multi_mod` must be defined in `src/multi_mod/multi_mod.pnt` and its submodule dependencies must live in `src/multi_mod/`. The absolute [path](../syntax.md#paths) to items in `multi_mod.pnt` is `::multi_mod::<item>`.

#### Declaring submodules

In any directory other than the project root directory, new submodules can be created as follows:

- A single-file submodule must be defined in a file that has the same name as the submodule itself. For example, submodule `single_submod` of module `multi_mod` must be defined in `src/multi_mod/single_submod.pnt`. The absolute [path](../syntax.md#paths) to items in `single_submod.pnt` is `::multi_mod::single_submod::<item>`.
- A multi-file submodule must be defined inside a directory that has the same name as the submodule itself. Moreover, the entry file of the submodule must also have the same name as the submodule. For example, multi-file submodule `multi_submod` must be defined in `src/multi_mod/multi_submod/multi_submod.pnt` and its own submodule dependencies must live in `src/multi_mod/multi_submod/`. The absolute [path](../syntax.md#paths) to items in `multi_submod.pnt` is `::multi_mod::multi_submod::<item>`.

Note that it is not allowed to have a file and a folder with the same name in any of the project's subdirectories. For example, a project that contains both `src/my_mod.pnt` and `src/my_mod/...` should not compile. Moreover, having a single file in a subdirectory is allowed even though subdirectories are typically used for multi-file modules. For example, having a single file in `src/my_mod/` is allowed as long as the name of that file is `my_mod.pnt`. This is equivalent to having the module live in `src/my_mod.pnt` and skipping the subdirectory `src/my_mod/` altogether.
