use crate::span::{Span, Spanned};
use ariadne::{FnCache, Label, Report, ReportKind, Source};
use std::fmt::{Display, Formatter, Result, Write};
use thiserror::Error;
use yansi::{Color, Paint, Style};

/// A warning label used for pretty printing warning messages to the terminal
pub struct WarningLabel {
    pub(super) message: String,
    pub(super) span: Span,
    pub(super) color: Color,
}

/// A general compile warning
#[derive(Error, Debug)]
pub enum Warning {
    #[error("unneeded else branch")]
    MatchUnneededElse { span: Span },
    #[error("constraint is always `true`")]
    TrivialConstraint { span: Span },
    #[error("constraint is always `false`")]
    AlwaysFalseConstraint { span: Span },
}

impl ReportableWarning for Warning {
    fn labels(&self) -> Vec<WarningLabel> {
        use Warning::*;
        match self {
            MatchUnneededElse { span } => vec![WarningLabel {
                message: "`else` branch in match will never be evaluated".to_string(),
                span: span.clone(),
                color: Color::Yellow,
            }],

            TrivialConstraint { span } => vec![WarningLabel {
                message: "this constraint always evaluates to `true`".to_string(),
                span: span.clone(),
                color: Color::Yellow,
            }],

            AlwaysFalseConstraint { span } => vec![WarningLabel {
                message: "this constraint always evaluates to `false` and can never be satisfied"
                    .to_string(),
                span: span.clone(),
                color: Color::Yellow,
            }],
        }
    }

    fn note(&self) -> Option<String> {
        use Warning::*;
        match self {
            MatchUnneededElse { .. } | TrivialConstraint { .. } | AlwaysFalseConstraint { .. } => {
                None
            }
        }
    }

    fn code(&self) -> Option<String> {
        None
    }

    fn help(&self) -> Option<String> {
        use Warning::*;
        match self {
            TrivialConstraint { .. } => {
                Some("if this is intentional, consider removing this constraint".to_string())
            }

            AlwaysFalseConstraint { .. } => {
                Some("if this is intentional, consider removing the containing predicate because its constraints can never be satisfied".to_string())
            }

            MatchUnneededElse { .. } => None,
        }
    }
}

impl Spanned for Warning {
    fn span(&self) -> &Span {
        use Warning::*;
        match self {
            MatchUnneededElse { span }
            | TrivialConstraint { span }
            | AlwaysFalseConstraint { span } => span,
        }
    }
}

#[derive(Debug)]
pub struct Warnings(pub Vec<Warning>);

impl Display for Warnings {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(|warning| warning.display_raw())
                .collect::<String>()
                .trim_end()
        )
    }
}

/// Types that implement this trait can be pretty printed to the terminal using the `ariadne` crate
/// by calling the `print()` method.
pub trait ReportableWarning
where
    Self: std::fmt::Display + Spanned,
{
    /// A list of warning labels for emitting diagnostics at multiple span locations
    fn labels(&self) -> Vec<WarningLabel>;

    /// A helpful "note" about the warning
    fn note(&self) -> Option<String>;

    /// A unique warning code
    fn code(&self) -> Option<String>;

    /// Additional information to help the user address the diagnostic
    fn help(&self) -> Option<String>;

    /// Pretty print an warning to the terminal
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

        let warning_file: &str = &format!("{}", self.span().context().display());
        let mut report_builder =
            Report::build(ReportKind::Warning, warning_file, self.span().start())
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

/// Print a list of ([`Warning`]) using the `ariadne` crate
pub fn print_warnings(warnings: &Warnings) {
    for warning in &warnings.0 {
        warning.print();
    }
}
