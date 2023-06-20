use std::fmt;
use std::string::String as StdString;

use miette::SourceSpan;

#[derive(Clone)]
pub struct Ident<'p> {
    text: &'p str,
    span: SourceSpan,
}

impl<'p> Ident<'p> {
    pub fn new(text: &'p str, span: SourceSpan) -> Self {
        Self { text, span }
    }

    pub fn text(&self) -> &'p str {
        self.text
    }

    pub fn span(&self) -> SourceSpan {
        self.span
    }
}

impl fmt::Debug for Ident<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Ident({} ({}..{}))",
            self.text,
            self.span.offset(),
            self.span.offset() + self.span.len()
        )
    }
}

#[derive(Clone)]
pub struct String<'p> {
    text: &'p str,
    span: SourceSpan,
    value: StdString,
}

impl<'p> String<'p> {
    pub fn new(text: &'p str, value: StdString, span: SourceSpan) -> Self {
        Self { text, span, value }
    }

    pub fn text(&self) -> &'p str {
        self.text
    }

    pub fn value(&self) -> &str {
        &self.value
    }

    pub fn into_value(self) -> StdString {
        self.value
    }

    pub fn span(&self) -> SourceSpan {
        self.span
    }
}

impl fmt::Debug for String<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "String({:?} ({}..{}))",
            self.value,
            self.span.offset(),
            self.span.offset() + self.span.len()
        )
    }
}

#[derive(Clone)]
pub struct Number<'p> {
    text: &'p str,
    span: SourceSpan,
}

impl<'p> Number<'p> {
    pub fn new(text: &'p str, span: SourceSpan) -> Self {
        Self { text, span }
    }

    pub fn text(&self) -> &'p str {
        self.text
    }

    pub fn span(&self) -> SourceSpan {
        self.span
    }
}

impl fmt::Debug for Number<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Ident({} ({}..{}))",
            self.text,
            self.span.offset(),
            self.span.offset() + self.span.len()
        )
    }
}
