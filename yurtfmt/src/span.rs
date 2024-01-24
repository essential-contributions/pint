use std::ops::Range;

pub type Span = Range<usize>;

pub(super) trait Spanned {
    fn span(&self) -> &Span;
}
