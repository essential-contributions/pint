//! Given a compilation [`Plan`][crate::plan::Plan], build all packages in the graph.

use crate::{
    manifest,
    plan::{Graph, NodeIx, Pinned, PinnedManifests, Plan},
};
use clap::builder::styling::Style;
use essential_types::{
    contract::Contract, predicate::Predicate as CompiledPredicate, ContentAddress,
};
use pint_abi_types::ContractABI;
use pintc::asm_gen::compile_contract;
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};
use thiserror::Error;

/// A context that allows for iteratively compiling packages within a given compilation `Plan`.
pub struct PlanBuilder<'p> {
    /// The plan that is being executed by this builder.
    pub plan: &'p Plan,
    built_pkgs: BuiltPkgs,
    order: std::slice::Iter<'p, NodeIx>,
}

/// A package that is ready to be built.
pub struct PrebuiltPkg<'p, 'b> {
    pub plan: &'p Plan,
    built_pkgs: &'b mut BuiltPkgs,
    /// The node of the package to be built.
    n: NodeIx,
}

#[derive(Default)]
pub struct BuildOptions {
    /// A 256-bit unsigned integer in hexadeciaml format that represents the contract "salt". The
    /// value is left padded with zeros if it has less than 64 hexadecimal digits.
    ///
    /// The "salt" is hashed along with the contract's bytecode in order to make the address of the
    /// contract unique.
    ///
    /// If "salt" is provided for a library package, an error is emitted.
    pub salt: [u8; 32],
    /// Print the parsed pint program.
    pub print_parsed: bool,
    /// Print the flattened pint program.
    pub print_flat: bool,
    /// Print the optimized pint program.
    pub print_optimized: bool,
    /// Print the assembly.
    pub print_asm: bool,
    /// Skip optimizing the pint program.
    pub skip_optimize: bool,
}

/// A mapping from the node index to the associated built package.
pub type BuiltPkgs = HashMap<NodeIx, BuiltPkg>;

/// A successfully built package.
#[derive(Debug)]
pub enum BuiltPkg {
    /// A built contract.
    Contract(BuiltContract),
    /// A built library.
    Library(BuiltLibrary),
}

/// A successfully built contract package.
#[derive(Debug)]
pub struct BuiltContract {
    /// All the emitted warnings.
    pub warnings: pintc::warning::Warnings,
    /// All built predicates.
    pub predicate_metadata: Vec<PredicateMetadata>,
    /// The salt of this contract.
    pub contract: Contract,
    /// The content address of the contract.
    pub ca: ContentAddress,
    /// The entry-point into the temp library submodules used to provide the CAs.
    pub lib_entry_point: PathBuf,
    /// The ABI for the contract.
    pub abi: ContractABI,
    /// The optimized contract.
    pub optimized: pintc::predicate::Contract,
}

/// An predicate built as a part of a contract.
#[derive(Debug)]
pub struct BuiltPredicate {
    /// The content address of the predicate.
    pub ca: ContentAddress,
    /// The name of the predicate from the code.
    pub name: String,
    pub predicate: CompiledPredicate,
}

/// An predicate built as a part of a contract.
#[derive(Debug)]
pub struct PredicateMetadata {
    /// The content address of the predicate.
    pub ca: ContentAddress,
    /// The name of the predicate from the code.
    pub name: String,
}

/// A successfully built library package.
#[derive(Debug)]
pub struct BuiltLibrary {
    /// All the emitted warnings.
    pub warnings: pintc::warning::Warnings,
    /// The compiled contract.
    pub contract: pintc::predicate::Contract,
}

/// An error occurred while building according to a compilation plan.
#[derive(Debug)]
pub struct BuildError {
    /// Packages that were successfully built.
    pub built_pkgs: BuiltPkgs,
    /// The package error that occurred.
    pub pkg_err: BuildPkgError,
}

/// An error was produced while building a package.
#[derive(Debug)]
pub struct BuildPkgError {
    /// The pintc error handler.
    pub handler: pintc::error::Handler,
    /// The kind of build error that occurred.
    pub kind: BuildPkgErrorKind,
}

#[derive(Debug, Error)]
pub enum BuildPkgErrorKind {
    #[error("`pintc` encountered an error: {0}")]
    Pintc(#[from] PintcError),
    #[error("failed to create lib providing contract and predicate CAs for {0:?}: {1}")]
    ContractLibrary(String, std::io::Error),
}

#[derive(Debug, Error)]
pub enum PintcError {
    #[error("parse error")]
    Parse,
    #[error("type check error")]
    TypeCheck,
    #[error("flattening error")]
    Flatten,
    #[error("abi gen")]
    ABIGen,
    #[error("asm-gen error")]
    AsmGen,
}

/// An error occurred while writing a built package's output artifacts.
#[derive(Debug, Error)]
pub enum WriteError {
    /// Failed to serialize contract or ABI.
    #[error("failed to serialize contract or ABI: {0}")]
    SerdeJson(#[from] serde_json::Error),
    /// An I/O error occurred.
    #[error("an I/O error occurred: {0}")]
    Io(#[from] std::io::Error),
}

impl<'p> PlanBuilder<'p> {
    /// Produce the next package that is to be built.
    pub fn next_pkg(&mut self) -> Option<PrebuiltPkg> {
        let &n = self.order.next()?;
        Some(PrebuiltPkg {
            plan: self.plan,
            built_pkgs: &mut self.built_pkgs,
            n,
        })
    }

    /// Access the set of packages that have been built so far.
    pub fn built_pkgs(&self) -> &BuiltPkgs {
        &self.built_pkgs
    }

    /// Build all remaining packages.
    pub fn build_all(mut self, options: &BuildOptions) -> Result<BuiltPkgs, BuildError> {
        while let Some(prebuilt) = self.next_pkg() {
            if let Err(pkg_err) = prebuilt.build(options) {
                let built_pkgs = self.built_pkgs;
                return Err(BuildError {
                    built_pkgs,
                    pkg_err,
                });
            }
        }
        Ok(self.built_pkgs)
    }
}

impl<'p, 'b> PrebuiltPkg<'p, 'b> {
    /// The index of the package within the plan's package graph.
    pub fn node_ix(&self) -> NodeIx {
        self.n
    }

    /// This package's pinned representation from within the compilation graph.
    pub fn pinned(&self) -> &'p Pinned {
        &self.plan.graph()[self.n]
    }

    /// Build this package.
    pub fn build(self, options: &BuildOptions) -> Result<&'b BuiltPkg, BuildPkgError> {
        let Self {
            plan,
            built_pkgs,
            n,
        } = self;
        let built = build_pkg(plan, built_pkgs, n, options)?;
        built_pkgs.insert(n, built);
        Ok(&built_pkgs[&n])
    }
}

impl BuildPkgError {
    /// Consume `self` and print the errors and the warnings.
    pub fn print_diagnostics(self) {
        let (errors, warnings) = self.handler.consume();
        pintc::error::print_errors(&pintc::error::Errors(errors));
        pintc::warning::print_warnings(&pintc::warning::Warnings(warnings));
    }
}

impl BuiltPkg {
    /// Write the built artifacts for this package to the given directory.
    pub fn write_to_dir(&self, name: &str, path: &Path) -> Result<(), WriteError> {
        match self {
            Self::Library(_) => (),
            Self::Contract(built) => {
                // Write the contract.
                let contract_string = serde_json::to_string_pretty(&built.contract)?;
                let contract_path = path.join(name).with_extension("json");
                std::fs::write(contract_path, contract_string)?;

                // Write the ABI.
                let abi_string = serde_json::to_string_pretty(&built.abi)?;
                let file_stem = format!("{}-abi", name);
                let abi_path = path.join(file_stem).with_extension("json");
                std::fs::write(abi_path, abi_string)?;
            }
        }
        Ok(())
    }

    /// Print all emitted warnings.
    pub fn print_warnings(&self) {
        let (Self::Contract(BuiltContract { warnings, .. })
        | Self::Library(BuiltLibrary { warnings, .. })) = self;

        pintc::warning::print_warnings(warnings);
    }
}

/// Collect the immediate dependencies of the given package.
fn dependencies<'a>(
    n: NodeIx,
    g: &Graph,
    manifests: &'a PinnedManifests,
    built_pkgs: &'a BuiltPkgs,
) -> HashMap<String, PathBuf> {
    use petgraph::{visit::EdgeRef, Direction};
    g.edges_directed(n, Direction::Outgoing)
        .map(|e| {
            let name = e.weight().name.to_string();
            let dep_n = e.target();
            let pinned = &g[dep_n];
            let manifest = &manifests[&pinned.id()];
            let entry_point = match &built_pkgs[&dep_n] {
                BuiltPkg::Library(_lib) => manifest.entry_point(),
                BuiltPkg::Contract(contract) => contract.lib_entry_point.clone(),
            };
            (name, entry_point)
        })
        .collect()
}

/// Given a built contract, generate a library with a module and constant for
/// each predicate's contract address along with a constant for the contract's
/// content address.
///
/// Returns the entry point to the library.
fn contract_dep_lib(
    ca: &ContentAddress,
    predicates: &[BuiltPredicate],
) -> std::io::Result<PathBuf> {
    // Temporary directory for the contract project.
    let temp_dir = std::env::temp_dir().join(format!("{:x}", ca));
    std::fs::create_dir_all(&temp_dir)?;

    // Write the contract's CA to the library root.
    let lib_str = format!("const ADDRESS: b256 = 0x{:x};", ca);
    let lib_path = temp_dir.join("lib.pnt");
    std::fs::write(&lib_path, lib_str.as_bytes())?;

    // Write the predicate CAs to submodules.
    for predicate in predicates {
        let submod_str = format!("const ADDRESS: b256 = 0x{:x};", predicate.ca);

        // Create the path to the submodule from the predicate name.
        let mut submod: Vec<&str> = predicate.name.split("::").collect();
        // The root predicate is nameless when output from pint, so we give it a name.
        if matches!(&submod[..], &[""]) {
            submod = vec!["root"];
        }
        let mut submod_path = temp_dir.clone();
        submod_path.extend(submod.clone());
        submod_path.set_extension("pnt");
        std::fs::create_dir_all(submod_path.parent().expect("submod has no parent dir"))?;
        std::fs::write(&submod_path, submod_str.as_bytes())?;
    }

    Ok(lib_path)
}

/// Build the package at the given index, assuming all dependencies are already built.
fn build_pkg(
    plan: &Plan,
    built_pkgs: &BuiltPkgs,
    n: NodeIx,
    options: &BuildOptions,
) -> Result<BuiltPkg, BuildPkgError> {
    let graph = plan.graph();
    let pinned = &graph[n];
    let manifest = &plan.manifests()[&pinned.id()];
    let entry_point = manifest.entry_point();
    let handler = pintc::error::Handler::default();
    let deps = dependencies(n, graph, plan.manifests(), built_pkgs);
    let source_str = match pinned.source {
        crate::source::Pinned::Member(_) => {
            format!("{}", manifest.dir().display())
        }
        _ => format!("{}", pinned.source),
    };

    // Parse the package from the entry point.
    let deps = deps
        .iter()
        .map(|(name, path)| (name.as_str(), path.as_path()))
        .collect();
    let Ok(parsed) = pintc::parser::parse_project(&handler, &deps, &entry_point) else {
        let kind = BuildPkgErrorKind::from(PintcError::Parse);
        return Err(BuildPkgError { handler, kind });
    };

    let bold = Style::new().bold();
    if options.print_parsed {
        println!(
            "   {}Printing parsed{} {} [{}] ({})",
            bold.render(),
            bold.render_reset(),
            pinned.name,
            manifest.pkg.kind,
            source_str,
        );
        println!("\n{parsed}");
    }

    // Type check the package.
    let Ok(contract) = handler.scope(|handler| parsed.type_check(handler)) else {
        let kind = BuildPkgErrorKind::from(PintcError::TypeCheck);
        return Err(BuildPkgError { handler, kind });
    };

    let built_pkg = match manifest.pkg.kind {
        manifest::PackageKind::Library => {
            // TODO: Add checks here to make sure the library is sane.. E.g., the library is
            // stateless, etc.
            let lib = BuiltLibrary {
                warnings: pintc::warning::Warnings(handler.consume().1),
                contract,
            };
            BuiltPkg::Library(lib)
        }
        manifest::PackageKind::Contract => {
            // Flatten the contract to flat pint (the IR).
            let Ok(flattened) = handler.scope(|handler| contract.flatten(handler)) else {
                let kind = BuildPkgErrorKind::from(PintcError::Flatten);
                return Err(BuildPkgError { handler, kind });
            };

            if options.print_flat {
                println!(
                    "   {}Printing flattened{} {} [{}] ({})",
                    bold.render(),
                    bold.render_reset(),
                    pinned.name,
                    manifest.pkg.kind,
                    source_str,
                );
                println!("\n{flattened}");
            }

            // Perform optimizations on the flattened contract.
            let optimized = if options.skip_optimize {
                flattened
            } else {
                flattened.optimize(&handler)
            };

            if options.print_optimized {
                println!(
                    "   {}Printing optimized{} {} [{}] ({})",
                    bold.render(),
                    bold.render_reset(),
                    pinned.name,
                    manifest.pkg.kind,
                    source_str,
                );
                println!("\n{optimized}");
            }

            // Produce the ABI for the flattened contract.
            let Ok(abi) = optimized.abi(&handler) else {
                let kind = BuildPkgErrorKind::from(PintcError::ABIGen);
                return Err(BuildPkgError { handler, kind });
            };

            // Generate the assembly and the predicates.
            let Ok(contract) = handler.scope(|h| compile_contract(h, options.salt, &optimized))
            else {
                let kind = BuildPkgErrorKind::from(PintcError::AsmGen);
                return Err(BuildPkgError { handler, kind });
            };

            if options.print_asm {
                println!(
                    "   {}Printing assembly for{} {} [{}] ({})",
                    bold.render(),
                    bold.render_reset(),
                    pinned.name,
                    manifest.pkg.kind,
                    source_str,
                );
                println!("\n{contract}");
            }

            // Collect the predicates alongside their content addresses.
            let predicates: Vec<_> = contract
                .predicates
                .into_iter()
                .zip(contract.names)
                .map(|(predicate, name)| {
                    let ca = essential_hash::content_addr(&predicate);
                    BuiltPredicate {
                        ca,
                        name,
                        predicate,
                    }
                })
                .collect();

            // The CA of the contract.
            let ca = essential_hash::contract_addr::from_predicate_addrs(
                predicates.iter().map(|predicate| predicate.ca.clone()),
                &contract.salt,
            );

            // Generate a temp lib for providing the contract and predicate CAs to dependents.
            let lib_entry_point = match contract_dep_lib(&ca, &predicates) {
                Ok(path) => path,
                Err(e) => {
                    let kind = BuildPkgErrorKind::ContractLibrary(pinned.name.clone(), e);
                    return Err(BuildPkgError { handler, kind });
                }
            };

            let (predicate_metadata, predicates) = predicates
                .into_iter()
                .map(
                    |BuiltPredicate {
                         ca,
                         name,
                         predicate,
                     }| (PredicateMetadata { ca, name }, predicate),
                )
                .unzip();

            let contract = BuiltContract {
                warnings: pintc::warning::Warnings(handler.consume().1),
                ca,
                predicate_metadata,
                contract: Contract {
                    predicates,
                    salt: contract.salt,
                },
                lib_entry_point,
                abi,
                optimized,
            };
            BuiltPkg::Contract(contract)
        }
    };

    Ok(built_pkg)
}

/// Given a compilation [`Plan`][crate::plan::Plan], return a [`PlanBuilder`]
/// that may be used to compile all packages within the graph.
pub fn build_plan(plan: &Plan) -> PlanBuilder {
    PlanBuilder {
        built_pkgs: BuiltPkgs::default(),
        plan,
        order: plan.compilation_order().iter(),
    }
}
