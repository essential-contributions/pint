pub(super) type Span = std::ops::Range<usize>;

pub(super) fn empty_span() -> Span {
    0..0
}

pub(super) trait Spanned {
    fn span(&self) -> &Span;
}
