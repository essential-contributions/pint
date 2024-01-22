use crate::{
    error::{ErrorLabel, ReportableError},
    span::{Span, Spanned},
};
use std::path::PathBuf;
use thiserror::Error;
use yansi::Color;

#[derive(Error, Debug)]
pub enum CompileError {
    #[error("compiler internal error: {msg}")]
    Internal { msg: &'static str, span: Span },
    #[error("couldn't read {file}: {error}")]
    FileIO {
        error: std::io::Error,
        file: PathBuf,
        span: Span,
    },
    #[error("multiple source files found for module")]
    DualModulity {
        mod_path: String,
        file_path_a: PathBuf,
        file_path_b: PathBuf,
        span: Span,
    },
    #[error("no file found for path")]
    NoFileFoundForPath {
        path_full: String,
        path_mod: String,
        path_enum: String,
        span: Span,
    },
    #[error("macro {name} is declared multiple times")]
    MacroDeclClash {
        name: String,
        span: Span,
        prev_span: Span,
    },
    #[error("macro not found")]
    MacroNotFound { name: String, span: Span },
    #[error("unable to match macro call")]
    MacroCallMismatch { name: String, span: Span },
    #[error("undefined macro parameter")]
    MacroUndefinedParam { name: String, span: Span },
    #[error("macro call is recursive")]
    MacroRecursion {
        name: String,
        call_span: Span,
        decl_span: Span,
    },
    #[error("`forall` index `{name}` has already been declared")]
    DuplicateForAllIndex {
        name: String,
        span: Span,
        prev_span: Span,
    },
    #[error("invalid bound for `forall` index `{name}`")]
    InvalidForAllIndexBound { name: String, span: Span },
    #[error("cannot find value `{name}` in this scope")]
    SymbolNotFound { name: String, span: Span },
    #[error("attempt to use a non-constant value as an array length")]
    NonConstArrayLength { span: Span },
    #[error("attempt to use an invalid constant as an array length")]
    InvalidConstArrayLength { span: Span },
    #[error("attempt to use a non-constant value as an array index")]
    NonConstArrayIndex { span: Span },
    #[error("attempt to use an invalid constant as an array index")]
    InvalidConstArrayIndex { span: Span },
    #[error("cannot index into value")]
    CannotIndexIntoValue { span: Span, index_span: Span },
}

impl ReportableError for CompileError {
    fn labels(&self) -> Vec<ErrorLabel> {
        use CompileError::*;
        match self {
            DualModulity { mod_path, span, .. } => {
                vec![ErrorLabel {
                    message: format!("multiple source files found for module {mod_path}"),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            NoFileFoundForPath {
                path_full, span, ..
            } => {
                vec![ErrorLabel {
                    message: format!("failed to resolve path {path_full}"),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            MacroDeclClash {
                name,
                span,
                prev_span,
            } => {
                vec![
                    ErrorLabel {
                        message: format!("previous declaration of the macro `{name}` here"),
                        span: prev_span.clone(),
                        color: Color::Blue,
                    },
                    ErrorLabel {
                        message: format!(
                            "`{name}` redeclared here with the same number of parameters"
                        ),
                        span: span.clone(),
                        color: Color::Red,
                    },
                ]
            }

            MacroNotFound { name, span } => {
                vec![ErrorLabel {
                    message: format!("macro `{name}` not found"),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            MacroCallMismatch { name, span } => {
                vec![ErrorLabel {
                    message: format!("unable to match call to macro `{name}`"),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            MacroUndefinedParam { name, span } => {
                vec![ErrorLabel {
                    message: format!("undefined parameter `{name}` in macro body"),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            MacroRecursion {
                name,
                call_span,
                decl_span,
            } => {
                vec![
                    ErrorLabel {
                        message: format!("macro '{name}' is recursively called"),
                        span: call_span.clone(),
                        color: Color::Red,
                    },
                    ErrorLabel {
                        message: format!("macro '{name}' declared here"),
                        span: decl_span.clone(),
                        color: Color::Blue,
                    },
                ]
            }

            DuplicateForAllIndex {
                name,
                span,
                prev_span,
            } => {
                vec![
                    ErrorLabel {
                        message: format!("previous declaration of the index `{name}` here"),
                        span: prev_span.clone(),
                        color: Color::Blue,
                    },
                    ErrorLabel {
                        message: format!("`{name}` redeclared here"),
                        span: span.clone(),
                        color: Color::Red,
                    },
                ]
            }

            InvalidForAllIndexBound { name, span } => {
                vec![ErrorLabel {
                    message: format!("invalid bound for `forall` index `{name}`"),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            SymbolNotFound { span, .. } => {
                vec![ErrorLabel {
                    message: "not found in this scope".to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            NonConstArrayLength { span } | NonConstArrayIndex { span } => {
                vec![ErrorLabel {
                    message: "this must be a constant".to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            InvalidConstArrayLength { span } => {
                vec![ErrorLabel {
                    message: "this must be a strictly positive integer value".to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            InvalidConstArrayIndex { span } => {
                vec![ErrorLabel {
                    message: "this must be a non-negative integer value".to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            CannotIndexIntoValue { span, index_span } => {
                vec![
                    ErrorLabel {
                        message: "this must be an array".to_string(),
                        span: span.clone(),
                        color: Color::Blue,
                    },
                    ErrorLabel {
                        message: "invalid indexing here".to_string(),
                        span: index_span.clone(),
                        color: Color::Red,
                    },
                ]
            }

            Internal { .. } | FileIO { .. } => Vec::new(),
        }
    }

    fn note(&self) -> Option<String> {
        use CompileError::*;
        match self {
            DualModulity {
                file_path_a,
                file_path_b,
                ..
            } => Some(format!(
                "both the files `{}` and `{}` exist, where only one or the other is allowed",
                file_path_a.display(),
                file_path_b.display()
            )),

            NoFileFoundForPath {
                path_mod,
                path_enum,
                ..
            } => Some(format!(
                "one of the modules `{}` or `{}` must exist",
                path_mod, path_enum
            )),

            MacroDeclClash { name, .. } => Some(format!(
                "it is valid to have multiple macros named `{name}` \
                but they must have differing parameter lists"
            )),

            MacroCallMismatch { name, .. } => Some(format!(
                "a macro named `{name}` is defined but not with the required \
                signature to fulfill this call"
            )),

            MacroRecursion { .. } => Some(
                "a macro called recursively with the same number of arguments \
                    will cause a non-terminating loop during expansion"
                    .to_string(),
            ),

            DuplicateForAllIndex { name, .. } => Some(format!(
                "`forall` index `{name}` must be declared only once in this scope"
            )),

            InvalidForAllIndexBound { .. } => {
                Some("`forall` index bound must be an integer literal".to_string())
            }

            Internal { .. }
            | FileIO { .. }
            | MacroNotFound { .. }
            | MacroUndefinedParam { .. }
            | SymbolNotFound { .. }
            | NonConstArrayLength { .. }
            | InvalidConstArrayLength { .. }
            | NonConstArrayIndex { .. }
            | InvalidConstArrayIndex { .. }
            | CannotIndexIntoValue { .. } => None,
        }
    }

    fn code(&self) -> Option<String> {
        None
    }

    fn help(&self) -> Option<String> {
        None
    }
}

impl Spanned for CompileError {
    fn span(&self) -> &Span {
        use CompileError::*;
        match self {
            FileIO { span, .. }
            | Internal { span, .. }
            | DualModulity { span, .. }
            | NoFileFoundForPath { span, .. }
            | MacroDeclClash { span, .. }
            | MacroNotFound { span, .. }
            | MacroCallMismatch { span, .. }
            | MacroUndefinedParam { span, .. }
            | MacroRecursion {
                call_span: span, ..
            }
            | DuplicateForAllIndex { span, .. }
            | InvalidForAllIndexBound { span, .. }
            | SymbolNotFound { span, .. }
            | NonConstArrayIndex { span }
            | InvalidConstArrayLength { span }
            | NonConstArrayLength { span }
            | InvalidConstArrayIndex { span }
            | CannotIndexIntoValue { span, .. } => span,
        }
    }
}
