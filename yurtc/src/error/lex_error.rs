use thiserror::Error;

// We would like to implement the trait `ReportableError` for `LexError` but that trait requires
// that the types that implement it are `Spanned`. Since, we don't have access to spans in
// `LexError`, and because it only has a single lex error variant for now, we don't need to
// over-engineer this. We'll leave to `impl ReportableError for Error<'_>` to directly handle lex
// error labels, error code, note, and help message.

/// An error originating from the lexer
#[derive(Error, Debug, Clone, PartialEq, Default)]
pub(crate) enum LexError {
    #[default]
    #[error("invalid token")]
    InvalidToken,
}
