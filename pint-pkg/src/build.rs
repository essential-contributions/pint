//! Given a compilation [`Plan`][crate::plan::Plan], build all packages in the graph.

use crate::{
    manifest,
    plan::{Graph, NodeIx, PinnedManifests, Plan},
};
use pintc::asm_gen::{program_to_intents, Intents};
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
    pub intents: Intents,
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
) -> HashMap<String, (PathBuf, &'a pintc::intermediate::Program)> {
    use petgraph::{visit::EdgeRef, Direction};
    g.edges_directed(n, Direction::Outgoing)
        .filter_map(|e| {
            let name = e.weight().name.to_string();
            let dep_n = e.target();
            let pinned = &g[dep_n];
            let manifest = &manifests[&pinned.id()];
            let entry_point = manifest.entry_point();
            let program = match &built_pkgs[&dep_n] {
                BuiltPkg::Library(lib) => &lib.program,
                BuiltPkg::Contract(_contract) => {
                    // TODO:
                    // Generate and compile a single-mod pkg with the `ADDRESS` const.
                    // Probably best to generate and compile this module
                    // when the contract package itself is being compiled and store
                    // the program in `BuiltContract`?
                    eprintln!("WARNING: TODO: provide contract module with `ADDRESS`");
                    return None;
                }
            };
            Some((name, (entry_point, program)))
        })
        .collect()
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
        let dep_paths = deps
            .iter()
            .map(|(n, (path, _))| (n.as_str(), path.as_path()))
            .collect();
        let Ok(parsed) = pintc::parser::parse_project(&handler, &dep_paths, &entry_point) else {
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
                let Ok(intents) = handler.scope(|h| program_to_intents(h, &flattened)) else {
                    let kind = BuildErrorKind::from(PintcError::IntentGen);
                    return Err(BuildError::new(built_pkgs, handler, kind));
                };

                let contract = BuiltContract { intents };
                BuiltPkg::Contract(contract)
            }
        };
        built_pkgs.insert(n, built_pkg);
    }
    Ok(BuiltPlan { pkgs: built_pkgs })
}
