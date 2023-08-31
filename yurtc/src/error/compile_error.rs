use crate::{
    error::{ErrorLabel, ReportableError},
    span::{Span, Spanned},
};
use thiserror::Error;
use yansi::Color;

#[derive(Error, Debug, PartialEq, Clone)]
pub(crate) enum CompileError {
    #[error("internal error: {msg}")]
    Internal { span: Span, msg: &'static str },
    #[error("symbol `{sym}` has already been declared")]
    NameClash {
        sym: String,
        span: Span,      // Actual error location
        prev_span: Span, // Span of the previous occurance
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
            Internal { span, msg } => {
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
            Internal { span, .. } | NameClash { span, .. } => span,
        }
    }
}
