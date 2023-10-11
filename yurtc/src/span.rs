use std::{fmt, ops::Range, path::Path, rc::Rc};

#[derive(Clone, PartialEq)]
pub struct Span {
    pub(super) context: Rc<Path>,
    pub(super) range: Range<usize>,
}

impl chumsky::Span for Span {
    // For now, the context is just a `Path`. This may change in the future
    type Context = Rc<Path>;

    type Offset = usize;

    fn new(context: Self::Context, range: Range<Self::Offset>) -> Self {
        Self { context, range }
    }

    fn context(&self) -> Self::Context {
        Rc::clone(&self.context)
    }

    fn start(&self) -> Self::Offset {
        self.range.start
    }
    fn end(&self) -> Self::Offset {
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
        context: Rc::from(Path::new("")),
    }
}

pub(super) trait Spanned {
    fn span(&self) -> &Span;
}
