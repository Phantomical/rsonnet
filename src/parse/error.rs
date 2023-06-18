use std::fmt;

use miette::{Diagnostic, SourceSpan};
use peg::error::{ExpectedSet, ParseError as PegParseError};
use thiserror::Error;

use super::{Parser, StreamPosition};

pub type ParseResult<'p, T> = Result<T, ParseError<'p>>;

#[derive(Clone, Debug, Error, Diagnostic)]
#[error("unexpected token {found}, expected {}", expected)]
pub struct ParseError<'p> {
    #[source_code]
    src: &'p str,
    found: Found<'p>,
    #[label]
    span: SourceSpan,
    expected: ExpectedSet,
}

impl<'p> ParseError<'p> {
    pub(crate) fn new(parser: &Parser<'p>, error: PegParseError<StreamPosition>) -> Self {
        let pos = error.location;
        let found = match parser.tokens.get(pos.index) {
            Some(token) => Found::Token(token.text),
            None => Found::Eof,
        };

        Self {
            src: parser.source,
            found,
            span: pos.span,
            expected: error.expected,
        }
    }
}

#[derive(Clone, Debug)]
enum Found<'p> {
    Token(&'p str),
    Eof,
}

impl fmt::Display for Found<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Token(token) => write!(f, "`{token}`"),
            Self::Eof => write!(f, "<eof>"),
        }
    }
}
