use std::ops::Range;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    start: usize,
    len: usize,
}

impl Span {
    pub fn start_end(start: usize, end: usize) -> Self {
        Self {
            start,
            len: end - start,
        }
    }
}

impl From<Range<usize>> for Span {
    fn from(r: Range<usize>) -> Self {
        Self::start_end(r.start, r.end)
    }
}
