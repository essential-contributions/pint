use crate::{ast, lexer::Token};
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

/// An error originating from the parser
#[derive(Error, Debug, PartialEq, Clone)]
pub(super) enum ParseError<'a> {
    #[error("{}", format_expected_found_error(&mut expected.clone(), found))]
    ExpectedFound {
        span: Span,
        expected: Vec<Option<Token<'a>>>,
        found: Option<Token<'a>>,
    },
    #[error("expected identifier, found keyword \"{keyword}\"")]
    KeywordAsIdent { span: Span, keyword: Token<'a> },
    #[error(
        "type annotation or initializer needed for decision variable \"{}\"", name.0
    )]
    UntypedDecisionVar { span: Span, name: ast::Ident },
    #[error("invalid integer value \"{}\" for tuple index", index)]
    InvalidIntegerTupleIndex { span: Span, index: &'a str },
    #[error("invalid value \"{}\" for tuple index", index)]
    InvalidTupleIndex { span: Span, index: Token<'a> },
    #[error("empty tuple expressions are not allowed")]
    EmptyTupleExpr { span: Span },
    #[error("empty tuple types are not allowed")]
    EmptyTupleType { span: Span },
}

fn format_expected_found_error<'a>(
    expected: &mut Vec<Option<Token<'a>>>,
    found: &Option<Token<'a>>,
) -> String {
    let format_optional_token = |token: &Option<Token>| match &token {
        Some(token) => format!("\"{token}\""),
        None => "end of input".to_string(),
    };

    format!(
        "found {} but expected {}",
        format_optional_token(found),
        match &expected[..] {
            [] => "something else".to_string(),
            [expected] => format_optional_token(expected),
            _ => {
                // Make sure that the list of expected tokens is printed in a deterministic order
                expected.sort();

                let mut token_list = "".to_string();
                for expected in &expected[..expected.len() - 1] {
                    token_list = format!("{token_list}{}, ", format_optional_token(expected));
                }
                format!(
                    "{token_list} or {}",
                    format_optional_token(expected.last().unwrap())
                )
            }
        }
    )
}

/// Implement the `ParseError` trait from Chumsky for `ParseError`
impl<'a> chumsky::Error<Token<'a>> for ParseError<'a> {
    type Span = Span;
    type Label = ();

    fn expected_input_found<Iter: IntoIterator<Item = Option<Token<'a>>>>(
        span: Span,
        expected: Iter,
        found: Option<Token<'a>>,
    ) -> Self {
        Self::ExpectedFound {
            span,
            expected: expected.into_iter().collect(),
            found,
        }
    }

    // Not currently doing anything with the label
    fn with_label(self, _: Self::Label) -> Self {
        self
    }

    // Merge two errors that point to the same input together, combining their information. Only
    // relevant for the `ExpectedFound` error for now.
    fn merge(mut self, mut other: Self) -> Self {
        #[allow(clippy::single_match)]
        match (&mut self, &mut other) {
            (
                Self::ExpectedFound { expected, .. },
                Self::ExpectedFound {
                    expected: expected_other,
                    ..
                },
            ) => expected.append(expected_other),
            _ => {}
        }
        self
    }
}

/// A general compile error
#[derive(Error, Debug, Clone, PartialEq)]
pub(super) enum CompileError<'a> {
    #[error("{}", error)]
    Lex { span: Span, error: LexError },
    #[error("{}", error)]
    Parse { error: ParseError<'a> },
}

impl<'a> CompileError<'a> {
    pub(super) fn span(&self) -> Span {
        use CompileError::*;
        match self {
            Lex { span, .. } => span.clone(),
            Parse { error } => match error {
                ParseError::ExpectedFound { span, .. } => span.clone(),
                ParseError::KeywordAsIdent { span, .. } => span.clone(),
                ParseError::UntypedDecisionVar { span, .. } => span.clone(),
                ParseError::InvalidIntegerTupleIndex { span, .. } => span.clone(),
                ParseError::InvalidTupleIndex { span, .. } => span.clone(),
                ParseError::EmptyTupleExpr { span } => span.clone(),
                ParseError::EmptyTupleType { span } => span.clone(),
            },
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
            anyhow::bail!("could not compile \"{}\" due to previous error", $filename)
        } else {
            anyhow::bail!(
                "could not compile \"{}\" due to {} previous errors",
                $filename,
                $number_of_errors
            )
        }
    };
}
