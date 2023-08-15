use crate::lexer::Token;
use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::prelude::*;
use thiserror::Error;

pub(super) type Span = std::ops::Range<usize>;

/// An error originating from the lexer
#[derive(Error, Debug, Clone, PartialEq, Default)]
pub(super) enum LexError {
    #[default]
    #[error("invalid token")]
    InvalidToken,
}

/// A general lexing/parsing or formatting error
#[derive(Error, Debug, Clone, PartialEq)]
pub(super) enum FormatterError<'a> {
    #[error("{}", error)]
    Lex { span: Span, error: LexError },
    #[error("{}", error)]
    Parse { error: Box<Simple<Token<'a>>> },
    #[error("Error formatting a message into a stream: {0}")]
    FormatError(#[from] std::fmt::Error),
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
            .print((filename, Source::from(source)))
            .unwrap();
    };

    for err in errs {
        match err {
            FormatterError::Lex { span, .. } => pretty_print_error(span, err),
            FormatterError::Parse { error } => pretty_print_error(&error.span(), err),
            FormatterError::FormatError(error) => {
                println!("{}", ansi_term::Colour::Red.paint(format!("{error}")));
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
        } else {
            anyhow::bail!(
                "could not format \"{}\" due to {} previous errors",
                $filename,
                $number_of_errors
            )
        }
    };
}
