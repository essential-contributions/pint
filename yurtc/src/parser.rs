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
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("/"));
    parser.parse_project(parser.root_src_path.clone(), Vec::new())?;
    parser.finalize()
}

#[derive(Default)]
struct ProjectParser {
    intent: IntermediateIntent,
    proj_root_path: PathBuf,
    root_src_path: PathBuf,
    visited_paths: Vec<PathBuf>,
}

impl ProjectParser {
    /// Parse `source` and return an intermediate intent. Upon failure, return a vector of all
    /// compile errors encountered.
    fn parse_project(
        &mut self,
        src_path: PathBuf,
        mod_path: Vec<String>,
    ) -> Result<(), Vec<Error>> {
        // Parse this file module, returning any paths to other potential modules.
        let mut mod_prefix = mod_path
            .iter()
            .map(|el| format!("::{el}"))
            .collect::<Vec<_>>()
            .concat();
        mod_prefix.push_str("::");

        let next_paths = self.parse_module(&Rc::from(src_path.clone()), &mod_path, &mod_prefix)?;

        // Store this path as parsed to avoid re-parsing later.
        self.visited_paths.push(src_path.clone());

        for (is_abs, mod_path_strs) in &next_paths {
            let mut next_path = self.proj_root_path.clone();
            let mut next_mod_path = Vec::new();
            if !is_abs {
                for m in &mod_path {
                    next_path.push(m);
                    next_mod_path.push(m.clone());
                }
            }
            for m in mod_path_strs {
                next_path.push(m);
                next_mod_path.push(m.clone());
            }
            next_path.set_extension("yrt");

            // If the next path does not exist, it may be a path to an enum type. In that case, we
            // pop the enum name from the path and try to parse the module where the enum is
            // defined. For example, if the path is `::a::b::MyEnum`, then we try to parse
            // `::a::b`. For that, we have to pop the enum type from both `next_path` and
            // `next_mod_path`. A special case is when, after popping the enum name, `next_path`
            // matches the project root path. In that case, we set `next_path` to the root source
            // path (i.e. `main.yrt` in most cases) and reset `next_mod_path`.
            if !next_path.exists() {
                next_path.pop();
                next_mod_path.pop();
                if next_path == self.proj_root_path {
                    next_path = self.root_src_path.clone();
                    next_mod_path = Vec::new();
                } else {
                    next_path.set_extension("yrt");
                }
            }

            if !self.visited_paths.contains(&next_path) {
                self.parse_project(next_path, next_mod_path)?;
            }
        }

        Ok(())
    }

    fn parse_module(
        &mut self,
        src_path: &Rc<Path>,
        mod_path: &[String],
        mod_prefix: &String,
    ) -> Result<Vec<(bool, Vec<String>)>, Vec<Error>> {
        println!("Compiling {}", src_path.display());
        let src_str = fs::read_to_string(src_path).map_err(|io_err| {
            vec![Error::Compile {
                error: CompileError::FileIO {
                    error: io_err,
                    file: Some(src_path.to_path_buf()),
                    span: span::empty_span(),
                },
            }]
        })?;
        let span_from = |start, end| Span {
            context: Rc::clone(src_path),
            range: start..end,
        };

        let mut use_trees = Vec::new();
        let mut next_paths = Vec::new();
        let mut context = ParserContext {
            mod_path,
            mod_prefix,
            ii: &mut self.intent,
            span_from: &span_from,
            use_paths: &mut use_trees,
            next_paths: &mut next_paths,
        };
        let mut errors = Vec::new();

        match yurt_parser::YurtParser::new().parse(
            &mut context,
            &mut errors,
            lexer::Lexer::new(&src_str, src_path),
        ) {
            Ok(result) => {
                if errors.is_empty() {
                    Ok(result)
                } else {
                    Err(errors)
                }
            }
            Err(lalrpop_err) => {
                errors.push(Error::Parse {
                    error: (lalrpop_err, src_path).into(),
                });
                Err(errors)
            }
        }?;

        Ok(next_paths)
    }

    fn finalize(self) -> Result<IntermediateIntent, Vec<Error>> {
        Ok(self.intent)
    }
}
