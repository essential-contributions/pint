use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::prelude::*;
use thiserror::Error;

use crate::lexer::Token;

pub(super) type Span = std::ops::Range<usize>;

/// An error originating from the lexer
#[derive(Error, Debug, Clone, PartialEq, Default)]
pub(super) enum LexError {
    #[default]
    #[error("invalid token")]
    InvalidToken,
}

/// An error originating from the parser
#[derive(Error, Debug, Clone, PartialEq)]
pub(super) enum ParseError {
    #[error("Error formatting starting at location {} and ending at location {}", span.start, span.end)]
    InvalidParse { span: Span },
}

/// A general lexing/parsing or formatting error
#[derive(Error, Debug, Clone, PartialEq)]
pub(super) enum FormatterError {
    #[error("{}", error)]
    Lex { span: Span, error: LexError },
    #[error("{}", error)]
    Parse { error: ParseError },
    #[error("Error formatting a message into a stream: {0}")]
    FormatError(#[from] std::fmt::Error),
}

/// Implement the `Error` trait from Chumsky for `ParseError`
impl<'a> chumsky::Error<Token<'a>> for ParseError {
    type Span = Span;
    type Label = ();

    fn expected_input_found<Iter: IntoIterator<Item = Option<Token<'a>>>>(
        span: Span,
        _: Iter,
        _: Option<Token<'a>>,
    ) -> Self {
        Self::InvalidParse { span }
    }

    // Not currently doing anything with the label
    fn with_label(self, _: Self::Label) -> Self {
        self
    }

    // Not currently doing anything with merging errors
    fn merge(self, _: Self) -> Self {
        self
    }
}

/// Print a list of `FormatterError`s using the `ariadne` library
pub(super) fn print_on_failure(filename: &str, source: &str, errs: &Vec<FormatterError>) -> usize {
    let pretty_print_error = |span: &Span, err: &FormatterError| {
        Report::build(ReportKind::Error, filename, span.start())
            .with_label(
                Label::new((filename, span.start()..span.end()))
                    .with_message(format!("{}", err.fg(Color::Red)))
                    .with_color(Color::Red),
            )
            .finish()
            .eprint((filename, Source::from(source)))
            .unwrap();
    };

    for err in errs {
        match err {
            FormatterError::Lex { span, .. } => pretty_print_error(span, err),
            FormatterError::Parse { error } => match error {
                ParseError::InvalidParse { span } => pretty_print_error(span, err),
            },
            FormatterError::FormatError(error) => {
                println!("{}", yansi::Color::Red.paint(format!("{error}")));
            }
        }
    }

    errs.len()
}

/// A simple warpper around `anyhow::bail!` that prints a different message based on a the number
/// of formatter errors.
macro_rules! yurtfmt_bail {
    ($number_of_errors: expr, $filename: expr) => {
        if $number_of_errors == 1 {
            anyhow::bail!("could not format \"{}\" due to previous error", $filename)
        }
        anyhow::bail!(
            "could not format \"{}\" due to {} previous errors",
            $filename,
            $number_of_errors
        )
    };
}
