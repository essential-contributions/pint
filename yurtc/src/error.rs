mod compile_error;
mod lex_error;
mod parse_error;

use crate::span::{Span, Spanned};
use ariadne::{Label, Report, ReportKind, Source};
use chumsky::prelude::*;
pub(super) use compile_error::CompileError;
pub(super) use lex_error::LexError;
pub(super) use parse_error::ParseError;
use thiserror::Error;
use yansi::{Color, Style};

/// An error label used for pretty printing error messages to the terminal
pub(super) struct ErrorLabel {
    pub(super) message: String,
    pub(super) span: Span,
    pub(super) color: Color,
}

/// A general compile error
#[derive(Error, Debug, Clone, PartialEq)]
pub(super) enum Error<'a> {
    #[error("{}", error)]
    Lex { span: Span, error: LexError },
    #[error("{}", error)]
    Parse { error: ParseError<'a> },
    #[error("{}", error)]
    Compile { error: CompileError },
}

/// Types that implement this trait can be pretty printed to the terminal using the `ariadne` crate
/// by calling the `print()` method.
pub(super) trait ReportableError
where
    Self: std::fmt::Display + Spanned,
{
    /// A list of error labels for emitting diagnostics at multiple span locations
    fn labels(&self) -> Vec<ErrorLabel>;

    /// A helpful "note" about the error
    fn note(&self) -> Option<String>;

    /// A unique error code
    fn code(&self) -> Option<String>;

    /// Additional information to help the user address the diagnostic
    fn help(&self) -> Option<String>;

    /// Pretty print an error to the terminal
    fn print(&self, filename: &str, source: &str) {
        let mut report_builder = Report::build(ReportKind::Error, filename, self.span().start)
            .with_message(format!("{}", Style::default().bold().paint(self)))
            .with_labels({
                let mut labels = vec![];
                for label in self.labels() {
                    let style = Style::new(label.color).bold();
                    labels.push(
                        Label::new((filename, label.span.start()..label.span.end()))
                            .with_message(style.paint(label.message))
                            .with_color(label.color),
                    );
                }
                labels
            });

        if let Some(code) = self.code() {
            report_builder = report_builder.with_code(code)
        };

        if let Some(note) = self.note() {
            report_builder = report_builder.with_note(note)
        };

        if let Some(help) = self.help() {
            report_builder = report_builder.with_help(help)
        };

        report_builder
            .finish()
            .print((filename, Source::from(source)))
            .unwrap();
    }
}

impl ReportableError for Error<'_> {
    fn labels(&self) -> Vec<ErrorLabel> {
        use Error::*;
        match self {
            Lex { error, span } => match error {
                // For lex errors, insert the labels here for now because we don't have access to
                // `span` in `LexError` just yet.
                LexError::InvalidToken => vec![ErrorLabel {
                    message: "invalid token".into(),
                    span: span.clone(),
                    color: Color::Red,
                }],
            },
            Parse { error } => error.labels(),
            Compile { error } => error.labels(),
        }
    }

    fn note(&self) -> Option<String> {
        use Error::*;
        match self {
            Lex { .. } => None,
            Parse { error } => error.note(),
            Compile { error } => error.note(),
        }
    }

    fn code(&self) -> Option<String> {
        use Error::*;
        match self {
            Lex { .. } => None,
            Parse { error } => error.code().map(|code| format!("P{}", code)),
            Compile { error } => error.code().map(|code| format!("C{}", code)),
        }
    }

    fn help(&self) -> Option<String> {
        use Error::*;
        match self {
            Lex { .. } => None,
            Parse { error } => error.help(),
            Compile { error } => error.help(),
        }
    }
}

impl Spanned for Error<'_> {
    fn span(&self) -> &Span {
        use Error::*;
        match &self {
            Lex { span, .. } => span,
            Parse { error } => error.span(),
            Compile { error } => error.span(),
        }
    }
}

/// Print a list of [`Error`] using the `ariadne` crate
pub(super) fn print_errors(errs: &Vec<Error>, filename: &str, source: &str) {
    for err in errs {
        err.print(filename, source);
    }
}

/// A simple warpper around `anyhow::bail!` that prints a different message based on a the number
/// of compile errors.
macro_rules! yurtc_bail {
    ($number_of_errors: expr, $filename: expr) => {
        if $number_of_errors == 1 {
            anyhow::bail!("could not compile `{}` due to previous error", $filename)
        } else {
            anyhow::bail!(
                "could not compile `{}` due to {} previous errors",
                $filename,
                $number_of_errors
            )
        }
    };
}
