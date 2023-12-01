use crate::{
    error::{CompileError, Error},
    expr::Ident,
    intent::intermediate::{CallKey, IntermediateIntent},
    lexer,
    macros::{MacroCall, MacroDecl},
    span::{self, Span},
};
use std::{
    fs,
    path::{Path, PathBuf},
    rc::Rc,
};

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(#[allow(clippy::ptr_arg)] pub yurt_parser);

mod use_path;
pub(crate) use use_path::{UsePath, UseTree};

mod context;
pub(crate) use context::ParserContext;

#[cfg(test)]
mod tests;

pub fn parse_project(root_src_path: &Path) -> Result<IntermediateIntent, Vec<Error>> {
    ProjectParser::new(PathBuf::from(root_src_path))
        .parse_project()
        .expand_macros()
        .finalize()
}

#[derive(Default)]
struct ProjectParser {
    intent: IntermediateIntent,
    macros: Vec<MacroDecl>,
    macro_calls: slotmap::SecondaryMap<CallKey, MacroCall>,
    proj_root_path: PathBuf,
    root_src_path: PathBuf,
    visited_paths: Vec<PathBuf>,
    errors: Vec<Error>,
}

#[derive(Clone)]
pub(crate) struct NextModPath {
    // Determines whether the path of the current module needs to be used as a prefix for next path
    pub(crate) is_abs: bool,

    // This is the module path to consider if the suffix refers to the name of a declaration
    pub(crate) mod_path_strs: Vec<String>,

    // This is the suffix, which may refer to the name of a declaration or an enum variant
    pub(crate) suffix: String,

    // This is the module path to consider if the suffix refers to an enum variant
    pub(crate) enum_path_strs: Option<Vec<String>>,

    // This is the span of the original full path (including the suffix)
    pub(crate) span: Span,
}

impl ProjectParser {
    fn new(root_src_path: PathBuf) -> Self {
        let proj_root_path = root_src_path
            .parent()
            .map_or_else(|| PathBuf::from("/"), PathBuf::from);

        Self {
            root_src_path,
            proj_root_path,

            ..Self::default()
        }
    }

    /// Given a next module path and a path to the current module, decide on a file in the project
    /// to actually parse.
    fn find_next_path(
        &mut self,
        is_abs: bool,
        path_strs: &[String],
        mod_path: &[String],
        span: &Span,
    ) -> Option<(PathBuf, Vec<String>)> {
        // Collect the module path as a vector of `String`s. Also, collect the next file path
        // starting from the project root path.
        let mut next_path = self.proj_root_path.clone();
        let mut next_mod_path = Vec::new();
        if !is_abs {
            for m in mod_path {
                next_path.push(m);
                next_mod_path.push(m.clone());
            }
        }
        for m in path_strs {
            next_path.push(m);
            next_mod_path.push(m.clone());
        }

        // An alternative next path includes a folder with the same name as the module.
        let alternative_next_path = next_path
            .join(next_mod_path.last().unwrap_or(&String::new()))
            .with_extension("yrt");

        next_path.set_extension("yrt");

        // Determine which of the two file paths above to actually parse. If both files exist,
        // that's an error. If only 1 exists, return that one. If no paths exist, simply return
        // None.
        match (next_path.exists(), alternative_next_path.exists()) {
            (false, false) => None,
            (true, false) => Some((next_path, next_mod_path)),
            (false, true) => Some((alternative_next_path, next_mod_path)),
            (true, true) => {
                self.errors.push(Error::Compile {
                    error: CompileError::DualModulity {
                        mod_path: next_mod_path.join("::"),
                        file_path_a: next_path,
                        file_path_b: alternative_next_path,
                        span: span.clone(),
                    },
                });
                None
            }
        }
    }

    /// Parse the project starting with the `root_src_path` and return an intermediate intent. Upon
    /// failure, return a vector of all compile errors encountered.
    fn parse_project(mut self) -> Self {
        let mut pending_paths = vec![(self.root_src_path.clone(), Vec::new())];
        while let Some((src_path, mod_path)) = pending_paths.pop() {
            if self.visited_paths.contains(&src_path) {
                continue;
            }

            // Store this path as parsed to avoid re-parsing later.
            self.visited_paths.push(src_path.to_path_buf());

            // Parse this file module, returning any paths to other potential modules.
            let mut mod_prefix = mod_path
                .iter()
                .map(|el| format!("::{el}"))
                .collect::<Vec<_>>()
                .concat();
            mod_prefix.push_str("::");

            let next_paths = self.parse_module(&Rc::from(src_path), &mod_path, &mod_prefix);

            for NextModPath {
                is_abs,
                mod_path_strs,
                suffix,
                enum_path_strs,
                span,
            } in &next_paths
            {
                // The idea here is to try to handle the next path assuming the suffix refers to a
                // declaration. If that fails, then try to handle the next path assuming it's a path to
                // an enum variant. If both options fail, then we emit an error.

                if let Some(next_path) =
                    self.find_next_path(*is_abs, mod_path_strs, &mod_path, span)
                {
                    pending_paths.push(next_path);
                    continue;
                }

                if let Some(enum_path_strs) = enum_path_strs {
                    if let Some(next_path) =
                        self.find_next_path(*is_abs, enum_path_strs, &mod_path, span)
                    {
                        pending_paths.push(next_path);
                        continue;
                    }

                    let path_mod = mod_path_strs.join("::");
                    let path_enum = enum_path_strs.join("::");
                    let path_full = format!("{path_mod}::{suffix}");

                    self.errors.push(Error::Compile {
                        error: CompileError::NoFileFoundForPath {
                            path_full: path_full.clone(),
                            path_mod,
                            path_enum,
                            span: span.clone(),
                        },
                    });
                }
            }
        }

        self
    }

    fn parse_module(
        &mut self,
        src_path: &Rc<Path>,
        mod_path: &[String],
        mod_prefix: &String,
    ) -> Vec<NextModPath> {
        let src_str = fs::read_to_string(src_path).unwrap_or_else(|io_err| {
            self.errors.push(Error::Compile {
                error: CompileError::FileIO {
                    error: io_err,
                    file: src_path.to_path_buf(),
                    span: span::empty_span(),
                },
            });
            String::new()
        });

        let span_from = |start, end| Span {
            context: Rc::clone(src_path),
            range: start..end,
        };

        let mut next_paths = Vec::new();
        let mut context = ParserContext {
            mod_path,
            mod_prefix,
            ii: &mut self.intent,
            macros: &mut self.macros,
            macro_calls: &mut self.macro_calls,
            span_from: &span_from,
            use_paths: &mut Vec::new(),
            next_paths: &mut next_paths,
        };

        let _ = yurt_parser::YurtParser::new()
            .parse(
                &mut context,
                &mut self.errors,
                lexer::Lexer::new(&src_str, src_path),
            )
            .map_err(|lalrpop_err| {
                self.errors.push(Error::Parse {
                    error: (lalrpop_err, src_path).into(),
                });
            });

        next_paths
    }

    fn expand_macros(self) -> Self {
        self
    }

    fn finalize(self) -> Result<IntermediateIntent, Vec<Error>> {
        if self.errors.is_empty() {
            Ok(self.intent)
        } else {
            Err(self.errors)
        }
    }
}
