//! Given a compilation [`Plan`][crate::plan::Plan], build all packages in the graph.

use crate::{
    manifest,
    plan::{Graph, NodeIx, PinnedManifests, Plan},
};
use essential_types::{intent::Intent, ContentAddress};
use pintc::{asm_gen::program_to_intents, intermediate::ProgramKind};
use std::{collections::HashMap, path::PathBuf};
use thiserror::Error;

/// The result of building a compilation plan.
#[derive(Debug)]
pub struct BuiltPlan {
    /// All built packages.
    pub pkgs: BuiltPkgs,
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

#[derive(Debug)]
pub struct BuildError {
    /// Packages that were successfully built.
    pub built_pkgs: BuiltPkgs,
    /// The pintc error handler.
    pub handler: pintc::error::Handler,
    /// The kind of build error that occurred.
    pub kind: BuildErrorKind,
}

#[derive(Debug, Error)]
pub enum BuildErrorKind {
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

impl BuildError {
    fn new(built_pkgs: BuiltPkgs, handler: pintc::error::Handler, kind: BuildErrorKind) -> Self {
        Self {
            built_pkgs,
            handler,
            kind,
        }
    }

    /// Consume `self` and print the errors.
    pub fn print(self) {
        let errors = self.handler.consume();
        eprintln!("{}", self.kind);
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
        .filter_map(|e| {
            let name = e.weight().name.to_string();
            let dep_n = e.target();
            let pinned = &g[dep_n];
            let manifest = &manifests[&pinned.id()];
            let entry_point = match &built_pkgs[&dep_n] {
                BuiltPkg::Library(_lib) => manifest.entry_point(),
                BuiltPkg::Contract(contract) => contract.lib_entry_point.clone(),
            };
            Some((name, entry_point))
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
    let temp_dir = std::env::temp_dir().join(&format!("{:x}", ca));
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

/// Given a compilation [`Plan`][crate::plan::Plan], build all packages in the graph.
pub fn build_plan(plan: &Plan) -> Result<BuiltPlan, BuildError> {
    let graph = plan.graph();
    let mut built_pkgs = BuiltPkgs::default();
    for &n in plan.compilation_order() {
        let pinned = &graph[n];
        let manifest = &plan.manifests()[&pinned.id()];
        let entry_point = manifest.entry_point();
        let handler = pintc::error::Handler::default();
        let deps = dependencies(n, graph, plan.manifests(), &built_pkgs);

        // Parse the package from the entry point.
        let deps = deps
            .iter()
            .map(|(name, path)| (name.as_str(), path.as_path()))
            .collect();
        let Ok(parsed) = pintc::parser::parse_project(&handler, &deps, &entry_point) else {
            let kind = BuildErrorKind::from(PintcError::Parse);
            return Err(BuildError::new(built_pkgs, handler, kind));
        };

        // Type check the package.
        let Ok(program) = handler.scope(|handler| parsed.type_check(handler)) else {
            let kind = BuildErrorKind::from(PintcError::TypeCheck);
            return Err(BuildError::new(built_pkgs, handler, kind));
        };

        let built_pkg = match manifest.pkg.kind {
            manifest::PackageKind::Library => {
                // Check that the Library is not stateful.
                if let pintc::intermediate::ProgramKind::Stateful = program.kind {
                    let kind = BuildErrorKind::StatefulLibrary(program);
                    return Err(BuildError::new(built_pkgs, handler, kind));
                }
                let lib = BuiltLibrary { program };
                BuiltPkg::Library(lib)
            }
            manifest::PackageKind::Contract => {
                // Flatten the program to flat pint (the IR).
                let Ok(flattened) = handler.scope(|handler| program.flatten(handler)) else {
                    let kind = BuildErrorKind::from(PintcError::Flatten);
                    return Err(BuildError::new(built_pkgs, handler, kind));
                };

                // Generate the assembly and the intents.
                let Ok(contract) = handler.scope(|h| program_to_intents(h, &flattened)) else {
                    let kind = BuildErrorKind::from(PintcError::IntentGen);
                    return Err(BuildError::new(built_pkgs, handler, kind));
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
                        let kind = BuildErrorKind::ContractLibrary(pinned.name.clone(), e);
                        return Err(BuildError::new(built_pkgs, handler, kind));
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
        built_pkgs.insert(n, built_pkg);
    }
    Ok(BuiltPlan { pkgs: built_pkgs })
}
