use crate::{
    error::{ErrorLabel, ReportableError},
    span::{Span, Spanned},
};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum SolveError {
    #[error("solver internal error: {msg}")]
    Internal { msg: &'static str, span: Span },
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
    fn span(&self) -> &Span {
        use SolveError::*;
        match &self {
            Internal { span, .. } => span,
        }
    }
}
