use crate::{
    error::{ErrorLabel, ReportableError},
    span::{Span, Spanned},
};
use std::path::PathBuf;
use thiserror::Error;
use yansi::Color;

#[derive(Error, Debug)]
pub enum CompileError {
    #[error("internal error: {msg}")]
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
}

impl ReportableError for CompileError {
    fn labels(&self) -> Vec<ErrorLabel> {
        use CompileError::*;
        match self {
            Internal { msg, span } => {
                vec![ErrorLabel {
                    message: msg.to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

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
                    message: format!("failed to resolve path {path_full} "),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }

            FileIO { .. } => Vec::new(),
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

            _ => None,
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
        match &self {
            FileIO { span, .. }
            | Internal { span, .. }
            | DualModulity { span, .. }
            | NoFileFoundForPath { span, .. } => span,
        }
    }
}
