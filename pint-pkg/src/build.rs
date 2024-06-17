//! Given a compilation [`Plan`][crate::plan::Plan], build all packages in the graph.

use crate::{
    manifest,
    plan::{Graph, NodeIx, Pinned, PinnedManifests, Plan},
};
use essential_types::{intent::Intent, ContentAddress};
use pintc::{asm_gen::program_to_intents, intermediate::ProgramKind};
use std::{collections::HashMap, path::PathBuf};
use thiserror::Error;

/// A context that allows for iteratively compiling packages within a given compilation `Plan`.
pub struct PlanBuilder<'p> {
    /// The plan that is being executed by this builder.
    pub plan: &'p Plan,
    built_pkgs: BuiltPkgs,
    order: std::slice::Iter<'p, NodeIx>,
}

/// A package that is ready to be built, as all
pub struct PrebuiltPkg<'p, 'b> {
    pub plan: &'p Plan,
    built_pkgs: &'b mut BuiltPkgs,
    /// The node of the package to be built.
    n: NodeIx,
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
    pub kind: ProgramKind,
    /// All built intents.
    pub intents: Vec<BuiltIntent>,
    /// The content address of the contract.
    pub ca: ContentAddress,
    /// The entry-point into the temp library submodules used to provide the CAs.
    pub lib_entry_point: PathBuf,
}

/// An intent built as a part of a contract.
#[derive(Debug)]
pub struct BuiltIntent {
    /// The content address of the intent.
    pub ca: ContentAddress,
    /// The name of the intent from the code.
    pub name: String,
    pub intent: Intent,
}

/// A successfully built library package.
#[derive(Debug)]
pub struct BuiltLibrary {
    /// The compiled program.
    pub program: pintc::intermediate::Program,
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
    #[error("expected library to be stateless, but type-checking shows the library is stateful")]
    StatefulLibrary(pintc::intermediate::Program),
    #[error("failed to create lib providing contract and intent CAs for {0:?}: {1}")]
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
    #[error("intent-gen error")]
    IntentGen,
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

    /// Finish building the remaining packages.
    pub fn finish(mut self) -> Result<BuiltPkgs, BuildError> {
        while let Some(prebuilt) = self.next_pkg() {
            if let Err(pkg_err) = prebuilt.build() {
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
    pub fn build(self) -> Result<&'b BuiltPkg, BuildPkgError> {
        let Self {
            plan,
            built_pkgs,
            n,
        } = self;
        let built = build_pkg(plan, built_pkgs, n)?;
        built_pkgs.insert(n, built);
        Ok(&built_pkgs[&n])
    }
}

impl BuildPkgError {
    /// Consume `self` and print the errors.
    pub fn eprint(self) {
        let errors = self.handler.consume();
        pintc::error::print_errors(&pintc::error::Errors(errors));
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
/// each intent's contract address along with a constant for the contract's
/// content address.
///
/// Returns the entry point to the library.
fn contract_dep_lib(ca: &ContentAddress, intents: &[BuiltIntent]) -> std::io::Result<PathBuf> {
    // Temporary directory for the contract project.
    let temp_dir = std::env::temp_dir().join(format!("{:x}", ca));
    std::fs::create_dir_all(&temp_dir)?;

    // Write the contract's CA to the library root.
    // TODO: Change this to use `const` once supported.
    let lib_str = format!("var ADDRESS: b256 = 0x{:x};", ca);
    let lib_path = temp_dir.join("lib.pnt");
    std::fs::write(&lib_path, lib_str.as_bytes())?;

    // Write the intent CAs to submodules.
    for intent in intents {
        // TODO: Change this to use `const` once supported.
        let submod_str = format!("var ADDRESS: b256 = 0x{:x};", intent.ca);

        // Create the path to the submodule from the intent name.
        let mut submod: Vec<&str> = intent.name.split("::").collect();
        // The root intent is nameless when output from pint, so we give it a name.
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
fn build_pkg(plan: &Plan, built_pkgs: &BuiltPkgs, n: NodeIx) -> Result<BuiltPkg, BuildPkgError> {
    let graph = plan.graph();
    let pinned = &graph[n];
    let manifest = &plan.manifests()[&pinned.id()];
    let entry_point = manifest.entry_point();
    let handler = pintc::error::Handler::default();
    let deps = dependencies(n, graph, plan.manifests(), built_pkgs);

    // Parse the package from the entry point.
    let deps = deps
        .iter()
        .map(|(name, path)| (name.as_str(), path.as_path()))
        .collect();
    let Ok(parsed) = pintc::parser::parse_project(&handler, &deps, &entry_point) else {
        let kind = BuildPkgErrorKind::from(PintcError::Parse);
        return Err(BuildPkgError { handler, kind });
    };

    // Type check the package.
    let Ok(program) = handler.scope(|handler| parsed.type_check(handler)) else {
        let kind = BuildPkgErrorKind::from(PintcError::TypeCheck);
        return Err(BuildPkgError { handler, kind });
    };

    let built_pkg = match manifest.pkg.kind {
        manifest::PackageKind::Library => {
            // Check that the Library is not stateful.
            if let pintc::intermediate::ProgramKind::Stateful = program.kind {
                let kind = BuildPkgErrorKind::StatefulLibrary(program);
                return Err(BuildPkgError { handler, kind });
            }
            let lib = BuiltLibrary { program };
            BuiltPkg::Library(lib)
        }
        manifest::PackageKind::Contract => {
            // Flatten the program to flat pint (the IR).
            let Ok(flattened) = handler.scope(|handler| program.flatten(handler)) else {
                let kind = BuildPkgErrorKind::from(PintcError::Flatten);
                return Err(BuildPkgError { handler, kind });
            };

            // Generate the assembly and the intents.
            let Ok(contract) = handler.scope(|h| program_to_intents(h, &flattened)) else {
                let kind = BuildPkgErrorKind::from(PintcError::IntentGen);
                return Err(BuildPkgError { handler, kind });
            };

            // Collect the intents alongside their content addresses.
            let intents: Vec<_> = contract
                .intents
                .into_iter()
                .zip(contract.names)
                .map(|(intent, name)| {
                    let ca = essential_hash::content_addr(&intent);
                    BuiltIntent { ca, name, intent }
                })
                .collect();

            // The CA of the contract.
            let ca = essential_hash::intent_set_addr::from_intent_addrs(
                intents.iter().map(|intent| intent.ca.clone()),
            );

            // Generate a temp lib for providing the contract and intent CAs to dependents.
            let lib_entry_point = match contract_dep_lib(&ca, &intents) {
                Ok(path) => path,
                Err(e) => {
                    let kind = BuildPkgErrorKind::ContractLibrary(pinned.name.clone(), e);
                    return Err(BuildPkgError { handler, kind });
                }
            };

            let kind = contract.kind;
            let contract = BuiltContract {
                kind,
                ca,
                intents,
                lib_entry_point,
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
