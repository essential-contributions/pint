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
    #[error("I/O error: {error}")]
    FileIO {
        error: std::io::Error,
        file: Option<PathBuf>,
        span: Span,
    },
    #[error("multiple source files for module {path} found")]
    DualModulity {
        path: PathBuf,
        path_a: PathBuf,
        path_b: PathBuf,
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

            FileIO { .. } | DualModulity { .. } => Vec::new(),
        }
    }

    fn note(&self) -> Option<String> {
        use CompileError::*;
        match self {
            DualModulity { path_a, path_b, .. } => Some(format!(
                "both the files `{}` and `{}` exist, where only one or the other is allowed",
                path_a.display(),
                path_b.display()
            )),

            FileIO { file, .. } => file
                .clone()
                .map(|file| format!("when accessing path {}", file.display())),

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
            FileIO { span, .. } | Internal { span, .. } | DualModulity { span, .. } => span,
        }
    }
}
