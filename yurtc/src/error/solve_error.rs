use crate::{
    error::{ErrorLabel, ReportableError},
    span::{empty_span, Span, Spanned},
};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum SolveError {
    #[error("internal error: {msg}")]
    Internal { msg: &'static str },
}

impl ReportableError for SolveError {
    fn labels(&self) -> Vec<ErrorLabel> {
        vec![]
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

impl Spanned for SolveError {
    fn span(&self) -> Span {
        empty_span()
    }
}
