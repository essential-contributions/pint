use crate::lexer::Token;
use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::prelude::*;
use thiserror::Error;

pub(super) type Span = std::ops::Range<usize>;

/// An error originating from the lexer
#[derive(Error, Debug, Clone, PartialEq, Default)]
pub(super) enum LexError {
    #[default]
    #[error("Invalid token")]
    InvalidToken,
}

/// A general compile error
#[derive(Error, Debug, Clone, PartialEq)]
pub(super) enum CompileError<'a> {
    #[error("{}", error)]
    Lex { span: Span, error: LexError },
    #[error("{}", error)]
    ParseError { error: Simple<Token<'a>> },
}

impl<'a> CompileError<'a> {
    fn span(&self) -> Span {
        use CompileError::*;
        match self {
            Lex { span, .. } => span.clone(),
            ParseError { error } => error.span(),
        }
    }
}

/// Print a list of `CompileError` using the `ariadne` library
pub(super) fn print_on_failure(filename: &str, source: &str, errs: &Vec<CompileError>) -> usize {
    for err in errs {
        let span = err.span();
        Report::build(ReportKind::Error, filename, span.start())
            .with_label(
                Label::new((filename, span.start()..span.end()))
                    .with_message(format!("{}", err.fg(Color::Red)))
                    .with_color(Color::Red),
            )
            .finish()
            .print((filename, Source::from(source)))
            .unwrap();
    }

    errs.len()
}

/// A simple warpper around `anyhow::bail!` that prints a different message based on a the number
/// of compiler errors.
macro_rules! yurtc_bail {
    ($number_of_errors: expr, $filename: expr) => {
        if $number_of_errors == 1 {
            anyhow::bail!("could not compile {} due to previous error", $filename)
        } else {
            anyhow::bail!(
                "could not compile {} due to {} previous errors",
                $filename,
                $number_of_errors
            )
        }
    };
}
