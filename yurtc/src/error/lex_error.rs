use crate::error::{ErrorLabel, ReportableError};
use thiserror::Error;

/// An error originating from the lexer
#[derive(Error, Debug, Clone, PartialEq, Default)]
pub(crate) enum LexError {
    #[default]
    #[error("invalid token")]
    InvalidToken,
}

impl ReportableError for LexError {
    fn labels(&self) -> Vec<ErrorLabel> {
        Vec::new()
    }

    fn note(&self) -> Option<String> {
        None
    }

    fn code(&self) -> Option<String> {
        None
    }

    fn help(&self) -> Option<String> {
        None
    }
}
