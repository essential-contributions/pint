use std::{fmt, ops::Range, path::Path, sync::Arc};

#[derive(Clone, PartialEq)]
pub struct Span {
    pub(super) context: Arc<Path>,
    pub(super) range: Range<usize>,
}

type Context = Arc<Path>;
type Offset = usize;

impl Span {
    // For now, the context is just a `Path`. This may change in the future
    pub fn new(context: Context, range: Range<Offset>) -> Self {
        Self { context, range }
    }

    pub fn context(&self) -> Context {
        Arc::clone(&self.context)
    }

    pub fn start(&self) -> Offset {
        self.range.start
    }

    pub fn end(&self) -> Offset {
        self.range.end
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}:{:?}", self.context, self.range)
    }
}

pub(super) fn empty_span() -> Span {
    Span {
        range: 0..0,
        context: Arc::from(Path::new("")),
    }
}

/// Join two spans into a new span ranging from the `lhs` to `rhs`.
/// NOTE: no validation is performed--it is assumed that `lhs` is before `rhs` and that they share
/// the same context, which is copied from `lhs`.  Behaviour is undefined otherwise.
pub(super) fn join(lhs: &Span, rhs: &Span) -> Span {
    Span {
        range: lhs.range.start..rhs.range.end,
        context: Arc::clone(&lhs.context),
    }
}

pub trait Spanned {
    fn span(&self) -> &Span;
}
