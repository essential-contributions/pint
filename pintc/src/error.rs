mod compile_error;
mod handler;
mod lex_error;
mod parse_error;

use crate::span::{empty_span, Span, Spanned};
use ariadne::{FnCache, Label, Report, ReportKind, Source};
use std::fmt::{Display, Formatter, Result, Write};
use thiserror::Error;
use yansi::{Color, Paint, Style};

pub(super) use compile_error::CompileError;
pub(super) use compile_error::LargeTypeError;
pub use handler::{ErrorEmitted, Handler};
pub(super) use lex_error::LexError;
pub(super) use parse_error::ParseError;

/// An error label used for pretty printing error messages to the terminal
pub struct ErrorLabel {
    pub(super) message: String,
    pub(super) span: Span,
    pub(super) color: Color,
}

/// A general compile error
#[derive(Error, Debug)]
pub enum Error {
    #[error("{error}")]
    Lex { span: Span, error: LexError },
    #[error("{error}")]
    Parse { error: ParseError },
    #[error("{error}")]
    Compile { error: CompileError },
    #[error("{child}")]
    MacroBodyWrapper {
        child: Box<Self>,
        macro_name: String,
        macro_span: Span,
    },
    #[error("compiler internal error: {msg}")]
    Internal { msg: String, span: Span },
}

#[derive(Debug)]
pub struct Errors(pub Vec<Error>);

impl Display for Errors {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(|err| err.display_raw())
                .collect::<String>()
                .trim_end()
        )
    }
}

/// Types that implement this trait can be pretty printed to the terminal using the `ariadne` crate
/// by calling the `print()` method.
pub trait ReportableError
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
    fn print(&self) {
        let filepaths_and_sources = self
            .labels()
            .iter()
            .map(|label| {
                let filepath = format!("{}", label.span.context().display());
                let source = std::fs::read_to_string(filepath.clone()).unwrap_or("<none>".into());
                (filepath, source)
            })
            .collect::<Vec<(String, String)>>();

        let error_file: &str = &format!("{}", self.span().context().display());
        let mut report_builder =
            Report::build(ReportKind::Error, (error_file, self.span().range.clone()))
                .with_message(format!("{}", self.bold()))
                .with_labels(
                    self.labels()
                        .iter()
                        .enumerate()
                        .map(|(index, label)| {
                            let filepath: &str = &filepaths_and_sources[index].0;
                            let mut style = Style::new().bold();
                            style.foreground = Some(label.color);
                            Label::new((filepath, label.span.start()..label.span.end()))
                                .with_message(label.message.clone().paint(style))
                                .with_color(label.color)
                        })
                        .collect::<Vec<_>>(),
                );

        if let Some(code) = self.code() {
            report_builder = report_builder.with_code(code);
        }

        if let Some(note) = self.note() {
            report_builder = report_builder.with_note(note);
        }

        if let Some(help) = self.help() {
            report_builder = report_builder.with_help(help);
        }

        report_builder
            .finish()
            .eprint(
                FnCache::new(|id: &&str| {
                    Err(Box::new(format!("Failed to fetch source '{id}'")) as _)
                })
                .with_sources(
                    filepaths_and_sources
                        .iter()
                        .map(|(id, s)| (&id[..], Source::from(s)))
                        .collect(),
                ),
            )
            .unwrap();
    }

    fn display_raw(&self) -> String {
        self.to_string()
            + "\n"
            + &self.labels().iter().fold(String::new(), |mut acc, label| {
                writeln!(
                    &mut acc,
                    "@{}..{}: {}",
                    label.span.start(),
                    label.span.end(),
                    label.message
                )
                .expect("Failed to write label to string");
                acc
            })
            + &self
                .note()
                .map_or(String::new(), |note| format!("{note}\n"))
            + &self
                .help()
                .map_or(String::new(), |help| format!("{help}\n"))
    }
}

impl ReportableError for Error {
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
            MacroBodyWrapper {
                child,
                macro_name,
                macro_span,
            } => {
                let mut labels = child.labels();
                labels.push(ErrorLabel {
                    message: format!("when making macro call to '{macro_name}'"),
                    span: macro_span.clone(),
                    color: Color::Yellow,
                });
                labels
            }
            Internal { msg, span } => {
                if span == &empty_span() {
                    Vec::new()
                } else {
                    vec![ErrorLabel {
                        message: msg.to_string(),
                        span: span.clone(),
                        color: Color::Red,
                    }]
                }
            }
        }
    }

    fn note(&self) -> Option<String> {
        use Error::*;
        match self {
            Lex { .. } => None,
            Parse { error } => error.note(),
            Compile { error } => error.note(),
            MacroBodyWrapper { child, .. } => child.note(),
            Internal { .. } => None,
        }
    }

    fn code(&self) -> Option<String> {
        use Error::*;
        match self {
            Lex { .. } => None,
            Parse { error } => error.code().map(|code| format!("P{code}")),
            Compile { error } => error.code().map(|code| format!("C{code}")),
            MacroBodyWrapper { child, .. } => child.code(),
            Internal { .. } => None,
        }
    }

    fn help(&self) -> Option<String> {
        use Error::*;
        match self {
            Lex { .. } => None,
            Parse { error } => error.help(),
            Compile { error } => error.help(),
            MacroBodyWrapper { child, .. } => child.help(),
            Internal { .. } => None,
        }
    }
}

impl Spanned for Error {
    fn span(&self) -> &Span {
        use Error::*;
        match &self {
            Lex { span, .. } => span,
            Parse { error } => error.span(),
            Compile { error } => error.span(),
            MacroBodyWrapper { child, .. } => child.span(),
            Internal { span, .. } => span,
        }
    }
}

/// Print a list of [`Error`] using the `ariadne` crate
pub fn print_errors(errs: &Errors) {
    for err in &errs.0 {
        err.print();
    }
}

/// A simple wrapper around `anyhow::bail!` that prints a different message based on a the number
/// of compile errors.
#[macro_export]
macro_rules! pintc_bail {
    ($number_of_errors: expr, $filepath: expr) => {
        if $number_of_errors == 1 {
            anyhow::bail!(
                "could not compile `{}` due to previous error",
                format!("{}", $filepath.display())
            )
        } else {
            anyhow::bail!(
                "could not compile `{}` due to {} previous errors",
                format!("{}", $filepath.display()),
                $number_of_errors
            )
        }
    };
}
