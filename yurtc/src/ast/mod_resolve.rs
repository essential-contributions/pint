use crate::{
    error::{CompileError, Error},
    parser, span,
};

use std::{fs::read_to_string, path::Path, rc::Rc};

/// Parse a path to the _root_ module into ASTs.  Each root AST is recursively
/// inspected and any referenced sub-modules are also parsed _and merged_ into the project AST, and
/// then returned.
pub(crate) fn parse_project(source_path: &std::path::Path) -> Result<super::Ast, Vec<Error>> {
    // TODO: The actual module resolution stuff is still coming.  In the meantime we'll just return
    // the main file unresolved AST.
    let source_code = read_to_string(Path::new(&source_path)).map_err(|error| {
        // Convert io::Error to Error and put it in a vector.
        vec![Error::Compile {
            error: CompileError::FileIO {
                error,
                span: span::empty_span(),
            },
        }]
    })?;

    let filepath: Rc<Path> = Rc::from(Path::new(&source_path));
    parser::parse_str_to_ast(&source_code, filepath)
}
