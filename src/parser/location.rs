use logos::Span;

#[derive(Default, Clone, Debug)]
pub struct Location {
    filename: String,
    lineno: usize,
    span: Span,
}

impl Location {
    pub(crate) fn set_location(&mut self, filename: impl Into<String>, lineno: usize, span: Span) {
        self.filename = filename.into();
        self.lineno = lineno;
        self.span = span;
    }

    pub(crate) fn filename(&self) -> &str {
        &self.filename
    }

    pub(crate) fn span(&self) -> &Span {
        &self.span
    }

    pub(crate) fn inc_lineno(&mut self) {
        self.lineno += 1;
    }

    pub(crate) fn lineno(&self) -> usize {
        self.lineno
    }
}
