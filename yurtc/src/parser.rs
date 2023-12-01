use crate::{
    error::{CompileError, Error},
    expr::Ident,
    intent::intermediate::IntermediateIntent,
    lexer,
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
    let mut parser = ProjectParser::default();
    parser.root_src_path = PathBuf::from(root_src_path);
    parser.proj_root_path = parser
        .root_src_path
        .clone()
        .parent()
        .map_or_else(|| PathBuf::from("/"), PathBuf::from);
    parser.parse_project(&parser.root_src_path.clone(), &Vec::new());
    parser.finalize()
}

#[derive(Default)]
struct ProjectParser {
    intent: IntermediateIntent,
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
    /// Given a next module path and a path to the current module, decide on a file in the project
    /// to actually parse and parse it.
    fn parse_next_path(
        &mut self,
        is_abs: bool,
        path_strs: &[String],
        mod_path: &[String],
        span: &Span,
    ) -> bool {
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
        // that's an error. If only 1 exists, parse that one. If no paths exist, simply return
        // `false`.
        match [next_path.clone(), alternative_next_path.clone()]
            .iter()
            .filter(|path| path.exists())
            .collect::<Vec<_>>()
            .as_slice()
        {
            [] => false,
            [path] => {
                // Make sure to only parse each file in the project once
                if !self.visited_paths.contains(path) {
                    self.parse_project(path, &next_mod_path);
                }
                true
            }
            _ => {
                self.errors.push(Error::Compile {
                    error: CompileError::DualModulity {
                        mod_path: next_mod_path.join("::"),
                        file_path_a: next_path,
                        file_path_b: alternative_next_path,
                        span: span.clone(),
                    },
                });
                false
            }
        }
    }

    /// Parse `source` and return an intermediate intent. Upon failure, return a vector of all
    /// compile errors encountered.
    fn parse_project(&mut self, src_path: &Path, mod_path: &[String]) {
        // Parse this file module, returning any paths to other potential modules.
        let mut mod_prefix = mod_path
            .iter()
            .map(|el| format!("::{el}"))
            .collect::<Vec<_>>()
            .concat();
        mod_prefix.push_str("::");

        let next_paths = self.parse_module(&Rc::from(src_path), mod_path, &mod_prefix);

        // Store this path as parsed to avoid re-parsing later.
        self.visited_paths.push(src_path.to_path_buf());

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

            if self.parse_next_path(*is_abs, mod_path_strs, mod_path, span) {
                continue;
            }

            if let Some(enum_path_strs) = enum_path_strs {
                if self.parse_next_path(*is_abs, enum_path_strs, mod_path, span) {
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

        let mut macros = Vec::new();
        let mut macro_calls = slotmap::SecondaryMap::new();
        let mut use_trees = Vec::new();
        let mut next_paths = Vec::new();
        let mut context = ParserContext {
            mod_path,
            mod_prefix,
            ii: &mut self.intent,
            macros: &mut macros,
            macro_calls: &mut macro_calls,
            span_from: &span_from,
            use_paths: &mut use_trees,
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

    fn finalize(self) -> Result<IntermediateIntent, Vec<Error>> {
        if self.errors.is_empty() {
            Ok(self.intent)
        } else {
            Err(self.errors)
        }
    }
}
