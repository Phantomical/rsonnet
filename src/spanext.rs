use miette::SourceSpan;

pub trait SpanExt {
    fn join(self, other: Self) -> Self;
}

impl SpanExt for SourceSpan {
    fn join(self, other: Self) -> Self {
        let start = usize::min(self.offset(), other.offset());
        let end = usize::max(self.offset() + self.len(), other.offset() + other.len());

        (start..end).into()
    }
}
