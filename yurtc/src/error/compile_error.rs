use crate::{
    error::{ErrorLabel, ReportableError},
    span::{Span, Spanned},
};
use thiserror::Error;
use yansi::Color;

#[derive(Error, Debug)]
pub(crate) enum CompileError {
    #[error("internal error: {msg}")]
    Internal { msg: &'static str, span: Span },
    #[error("I/O error: {error}")]
    FileIO { error: std::io::Error, span: Span },
    #[error("symbol `{sym}` has already been declared")]
    NameClash {
        sym: String,
        span: Span,      // Actual error location
        prev_span: Span, // Span of the previous occurrence
    },
}

impl ReportableError for CompileError {
    fn labels(&self) -> Vec<ErrorLabel> {
        use CompileError::*;
        match self {
            NameClash {
                sym,
                span,
                prev_span,
            } => {
                vec![
                    ErrorLabel {
                        message: format!("previous definition of the value `{sym}` here"),
                        span: prev_span.clone(),
                        color: Color::Blue,
                    },
                    ErrorLabel {
                        message: format!("`{sym}` redefined here"),
                        span: span.clone(),
                        color: Color::Red,
                    },
                ]
            }
            FileIO { error, span } => {
                vec![ErrorLabel {
                    message: error.to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }
            Internal { msg, span } => {
                vec![ErrorLabel {
                    message: msg.to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }
        }
    }

    fn note(&self) -> Option<String> {
        use CompileError::*;
        match self {
            NameClash { sym, .. } => {
                Some(format!("`{sym}` must be defined only once in this scope"))
            }
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
            FileIO { span, .. } | Internal { span, .. } | NameClash { span, .. } => span,
        }
    }
}
