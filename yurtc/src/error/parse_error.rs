use crate::{
    error::{ErrorLabel, ReportableError},
    lexer::Token,
    span::Span,
};
use thiserror::Error;
use yansi::Color;

/// An error originating from the parser
#[derive(Error, Debug, PartialEq, Clone)]
pub(crate) enum ParseError<'a> {
    #[error("{}", format_expected_found_error(&mut expected.clone(), found))]
    ExpectedFound {
        span: Span,
        expected: Vec<Option<Token<'a>>>,
        found: Option<Token<'a>>,
    },
    #[error("expected identifier, found keyword `{keyword}`")]
    KeywordAsIdent { span: Span, keyword: Token<'a> },
    #[error("type annotation or initializer needed for variable `{name}`")]
    UntypedVariable { span: Span, name: String },
    #[error("empty array expressions are not allowed")]
    EmptyArrayExpr { span: Span },
    #[error("invalid integer `{}` as tuple index", index)]
    InvalidIntegerTupleIndex { span: Span, index: &'a str },
    #[error("invalid value `{}` as tuple index", index)]
    InvalidTupleIndex { span: Span, index: Token<'a> },
    #[error("empty tuple expressions are not allowed")]
    EmptyTupleExpr { span: Span },
    #[error("empty tuple types are not allowed")]
    EmptyTupleType { span: Span },
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

impl ReportableError for ParseError<'_> {
    fn labels(&self) -> Vec<ErrorLabel> {
        use ParseError::*;
        match self {
            ExpectedFound { span, expected, .. } => {
                vec![ErrorLabel {
                    message: format_expected_tokens_message(&mut expected.clone()),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }
            KeywordAsIdent { span, .. } => {
                vec![ErrorLabel {
                    message: "expected identifier, found keyword".to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }
            UntypedVariable { span, .. } => {
                vec![ErrorLabel {
                    message: "type annotation or initializer needed".to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }
            EmptyArrayExpr { span } => {
                vec![ErrorLabel {
                    message: "empty array expression found".to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }
            InvalidIntegerTupleIndex { span, .. } => {
                vec![ErrorLabel {
                    message: "invalid integer as tuple index".to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }
            InvalidTupleIndex { span, .. } => {
                vec![ErrorLabel {
                    message: "invalid value as tuple index".to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }
            EmptyTupleExpr { span } => {
                vec![ErrorLabel {
                    message: "empty tuple expression found".to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }
            EmptyTupleType { span } => {
                vec![ErrorLabel {
                    message: "empty tuple type found".to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }
        }
    }

    fn note(&self) -> Option<String> {
        None
    }

    fn code(&self) -> Option<String> {
        None
    }

    fn help(&self) -> Option<String> {
        use ParseError::*;
        match self {
            UntypedVariable { name, .. } => Some(format!(
                "consider giving `{name}` an explicit type or an initializer"
            )),
            _ => None,
        }
    }
}

fn format_optional_token(token: &Option<Token>) -> String {
    match &token {
        Some(token) => format!("`{token}`"),
        None => "\"end of input\"".into(),
    }
}

fn format_expected_tokens_message(expected: &mut Vec<Option<Token<'_>>>) -> String {
    format!(
        "expected {}",
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
                    "{token_list}or {}",
                    format_optional_token(expected.last().unwrap())
                )
            }
        }
    )
}

fn format_expected_found_error<'a>(
    expected: &mut Vec<Option<Token<'a>>>,
    found: &Option<Token<'a>>,
) -> String {
    format!(
        "{}, found {}",
        format_expected_tokens_message(expected),
        format_optional_token(found),
    )
}
