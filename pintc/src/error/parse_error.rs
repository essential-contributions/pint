use crate::{
    error::{ErrorLabel, ReportableError},
    lexer::{self, Token},
    span::{Span, Spanned},
};
use fxhash::FxHashSet;
use std::{path::Path, sync::Arc};
use thiserror::Error;
use yansi::Color;

/// An error originating from the parser
#[derive(Error, Debug, PartialEq, Clone, Default)]
pub enum ParseError {
    // This is the default error which can be generated by the Logos lexer on a non-match.
    #[default]
    #[error("invalid token")]
    InvalidToken,
    #[error("invalid token")]
    Lex { span: Span },

    #[error("{}", format_expected_found_error(&mut expected.clone(), found))]
    ExpectedFound {
        span: Span,
        expected: Vec<Option<String>>,
        found: Option<String>,
    },
    #[error("missing array or map index")]
    EmptyIndexAccess { span: Span },
    #[error("invalid integer `{}` as tuple index", index)]
    InvalidIntegerTupleIndex { span: Span, index: String },
    #[error("invalid value `{}` as tuple index", index)]
    InvalidTupleIndex { span: Span, index: String },
    #[error("empty tuple expressions are not allowed")]
    EmptyTupleExpr { span: Span },
    #[error("empty tuple types are not allowed")]
    EmptyTupleType { span: Span },
    #[error("symbol `{sym}` has already been declared")]
    NameClash {
        sym: String,
        span: Span,      // Actual error location
        prev_span: Span, // Span of the previous occurrence
    },
    #[error("leading `+` is not supported")]
    UnsupportedLeadingPlus { span: Span },
    #[error("`self` import can only appear in an import list with a non-empty prefix")]
    SelfWithEmptyPrefix { span: Span },
    #[error("`self` is only allowed at the end of a use path")]
    SelfNotAtTheEnd { span: Span },
    #[error("unexpected binary integer literal length")]
    BinaryLiteralLength { digits: usize, span: Span },
    #[error("unexpected hexadecimal integer literal length")]
    HexLiteralLength { digits: usize, span: Span },
    #[error("integer literal is too large")]
    IntLiteralTooLarge { span: Span },
    #[error("`storage` block has already been declared")]
    TooManyStorageBlocks {
        span: Span,      // Actual error location
        prev_span: Span, // Span of the previous occurrence
    },
    #[error("a `storage` block can only appear in the top level module")]
    StorageDirectiveMustBeTopLevel { span: Span },
    #[error("`storage` access expressions can only appear in the top level module")]
    StorageAccessMustBeTopLevel { span: Span },
    #[error("bad argument splice")]
    BadSplice(Span),
    #[error("no intrinsic named `{name}` is found")]
    MissingIntrinsic { name: String, span: Span },
    #[error("Unsupported type")]
    TypeNotSupported { ty: String, span: Span },
    #[error("Unsupported literal")]
    LiteralNotSupported { kind: String, span: Span },
    #[error("`consts` must be declared outside of a `predicate`")]
    UnsupportedConstLocation { span: Span },
    #[error("assembly instruction can only have a 64-bit integer argument")]
    ExpectedIntegerLiteral { span: Span },
}

impl ReportableError for ParseError {
    fn labels(&self) -> Vec<ErrorLabel> {
        use ParseError::*;
        match self {
            InvalidToken => Vec::new(),
            Lex { span } => {
                vec![ErrorLabel {
                    message: "tokenization failure, unmatched input".to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }
            ExpectedFound { span, expected, .. } => {
                vec![ErrorLabel {
                    message: format_expected_tokens_message(&mut expected.clone()),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }
            EmptyIndexAccess { span } => {
                vec![ErrorLabel {
                    message: "missing array or map element index".to_string(),
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
            NameClash {
                sym,
                span,
                prev_span,
            } => {
                vec![
                    ErrorLabel {
                        message: format!("previous declaration of the symbol `{sym}` here"),
                        span: prev_span.clone(),
                        color: Color::Blue,
                    },
                    ErrorLabel {
                        message: format!("`{sym}` redeclared here"),
                        span: span.clone(),
                        color: Color::Red,
                    },
                ]
            }
            UnsupportedLeadingPlus { span } => {
                vec![ErrorLabel {
                    message: "unexpected `+`".to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }
            SelfWithEmptyPrefix { span } => {
                vec![ErrorLabel {
                    message: "can only appear in an import list with a non-empty prefix"
                        .to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }
            SelfNotAtTheEnd { span } => {
                vec![ErrorLabel {
                    message: "`self` can only appear at the end of a use path".to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }
            BinaryLiteralLength { digits, span } => {
                vec![ErrorLabel {
                    message: format!(
                        "{digits} is not a valid number of digits in a binary integer literal"
                    ),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }
            HexLiteralLength { digits, span } => {
                vec![ErrorLabel {
                    message: format!(
                        "{digits} is not a valid number of digits in a hexadecimal integer literal"
                    ),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }
            IntLiteralTooLarge { span } => {
                vec![ErrorLabel {
                    message: "integer literal is too large".to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }
            TooManyStorageBlocks { span, prev_span } => {
                vec![
                    ErrorLabel {
                        message: "previous declaration of a `storage` block here".to_string(),
                        span: prev_span.clone(),
                        color: Color::Blue,
                    },
                    ErrorLabel {
                        message: "another `storage` block is declared here".to_string(),
                        span: span.clone(),
                        color: Color::Red,
                    },
                ]
            }
            StorageDirectiveMustBeTopLevel { span } => {
                vec![ErrorLabel {
                    message: "a `storage` block can only appear in the top level module"
                        .to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }
            StorageAccessMustBeTopLevel { span } => {
                vec![ErrorLabel {
                    message: "`storage` access expressions can only appear in the top level module"
                        .to_string(),
                    span: span.clone(),
                    color: Color::Red,
                }]
            }
            BadSplice(span) => vec![ErrorLabel {
                message: "the macro argument splice operator `~` must be applied to an identifier"
                    .to_string(),
                span: span.clone(),
                color: Color::Red,
            }],
            MissingIntrinsic { span, .. } => vec![ErrorLabel {
                message: "intrinsic not found".to_string(),
                span: span.clone(),
                color: Color::Red,
            }],
            TypeNotSupported { ty, span } => vec![ErrorLabel {
                message: format!("type `{ty}` is not currently supported in Pint"),
                span: span.clone(),
                color: Color::Red,
            }],
            LiteralNotSupported { kind, span } => vec![ErrorLabel {
                message: format!("\"{kind}\" literals are not currently supported in Pint"),
                span: span.clone(),
                color: Color::Red,
            }],
            UnsupportedConstLocation { span } => vec![ErrorLabel {
                message: "unexpected `const`".to_string(),
                span: span.clone(),
                color: Color::Red,
            }],
            ExpectedIntegerLiteral { span } => vec![ErrorLabel {
                message: "expecting a 64-bit integer here".to_string(),
                span: span.clone(),
                color: Color::Red,
            }],
        }
    }

    fn note(&self) -> Option<String> {
        use ParseError::*;
        match self {
            NameClash { sym, .. } => Some(format!(
                "`{sym}` must be declared or imported only once in this scope"
            )),
            BinaryLiteralLength { .. } => {
                Some("number of digits must be either 256 or between 1 and 64".to_string())
            }
            HexLiteralLength { .. } => {
                Some("number of digits must be either 64 or between 1 and 16".to_string())
            }
            IntLiteralTooLarge { .. } => {
                Some("value exceeds limit of `9,223,372,036,854,775,807`".to_string())
            }
            _ => None,
        }
    }

    fn code(&self) -> Option<String> {
        None
    }

    fn help(&self) -> Option<String> {
        use ParseError::*;
        match self {
            UnsupportedLeadingPlus { .. } => Some("try removing the `+`".to_string()),
            UnsupportedConstLocation { .. } => {
                Some("try declaring the const outside the body of the `predicate`".to_string())
            }
            _ => None,
        }
    }
}

fn format_optional_token(token: &Option<String>) -> String {
    match &token {
        Some(token) => format!("`{token}`"),
        None => "\"end of input\"".into(),
    }
}

fn format_expected_tokens_message(expected: &mut [Option<String>]) -> String {
    format!(
        "expected {}",
        match expected {
            [] => "something else".to_string(),
            [expected] => format_optional_token(&lexer::get_token_error_category(expected)),
            _ => {
                let mut expected: Vec<Option<String>> = expected
                    .iter()
                    .map(lexer::get_token_error_category)
                    .collect::<FxHashSet<_>>() // Remove duplicates
                    .into_iter()
                    .collect();

                // Make sure that the list of expected tokens is printed in a deterministic order
                expected.sort();

                let mut token_list = String::new();
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

fn format_expected_found_error(expected: &mut [Option<String>], found: &Option<String>) -> String {
    format!(
        "{}, found {}",
        format_expected_tokens_message(expected),
        format_optional_token(found),
    )
}

impl Spanned for ParseError {
    fn span(&self) -> &Span {
        use ParseError::*;
        match self {
            ExpectedFound { span, .. }
            | EmptyIndexAccess { span }
            | InvalidIntegerTupleIndex { span, .. }
            | InvalidTupleIndex { span, .. }
            | EmptyTupleExpr { span, .. }
            | EmptyTupleType { span, .. }
            | NameClash { span, .. }
            | UnsupportedLeadingPlus { span, .. }
            | SelfWithEmptyPrefix { span, .. }
            | SelfNotAtTheEnd { span, .. }
            | BinaryLiteralLength { span, .. }
            | HexLiteralLength { span, .. }
            | IntLiteralTooLarge { span, .. }
            | TooManyStorageBlocks { span, .. }
            | StorageDirectiveMustBeTopLevel { span, .. }
            | StorageAccessMustBeTopLevel { span, .. }
            | BadSplice(span)
            | MissingIntrinsic { span, .. }
            | TypeNotSupported { span, .. }
            | LiteralNotSupported { span, .. }
            | UnsupportedConstLocation { span, .. }
            | ExpectedIntegerLiteral { span, .. }
            | Lex { span } => span,

            InvalidToken => unreachable!("The `InvalidToken` error is always wrapped in `Lex`."),
        }
    }
}

type LalrpopError = lalrpop_util::ParseError<usize, Token, ParseError>;

impl From<(LalrpopError, &Arc<Path>)> for ParseError {
    fn from(err_and_path: (LalrpopError, &Arc<Path>)) -> Self {
        fn span_at(src_path: &Arc<Path>, start: usize, end: usize) -> Span {
            Span {
                context: src_path.clone(),
                range: start..end,
            }
        }

        let parse_err = err_and_path.0;
        let src_path = err_and_path.1;

        match parse_err {
            lalrpop_util::ParseError::InvalidToken { location } => ParseError::Lex {
                span: span_at(src_path, location, location + 1),
            },
            lalrpop_util::ParseError::UnrecognizedEof { location, expected } => {
                ParseError::ExpectedFound {
                    span: span_at(src_path, location, location), // Not going to send span beyond EOF..?
                    expected: expected
                        .into_iter()
                        .map(|mut expected| {
                            expected.retain(|c| c != '\"');
                            Some(expected)
                        })
                        .collect(),
                    found: Some("end of file".to_owned()),
                }
            }
            lalrpop_util::ParseError::UnrecognizedToken {
                token: (start, tok, end),
                expected,
            } => ParseError::ExpectedFound {
                span: span_at(src_path, start, end),
                expected: expected
                    .into_iter()
                    .map(|mut expected| {
                        expected.retain(|c| c != '\"');
                        Some(expected)
                    })
                    .collect(),
                found: Some(tok.to_string()),
            },
            lalrpop_util::ParseError::ExtraToken {
                token: (start, tok, end),
            } => ParseError::ExpectedFound {
                span: span_at(src_path, start, end),
                expected: vec![Some("end of file".to_string())],
                found: Some(tok.to_string()),
            },
            lalrpop_util::ParseError::User { error } => error,
        }
    }
}
