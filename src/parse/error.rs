use std::fmt;

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use super::Parser;
use crate::lexer::{LexerError, LexerErrorCode};

pub type ParseResult<'p, T> = Result<T, ParseError<'p>>;

impl<'p> Parser<'p> {
    pub(super) fn error(&self, code: ParseErrorCode<'p>) -> ParseError<'p> {
        ParseError {
            source: self.lexer.source(),
            code,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ParseError<'p> {
    source: &'p str,
    code: ParseErrorCode<'p>,
}

impl<'p> ParseError<'p> {
    pub fn is_eof(&self) -> bool {
        matches!(self.code, ParseErrorCode::Lexer(LexerErrorCode::Eof))
    }
}

#[derive(Clone, Debug, Error, Diagnostic)]
pub(in crate::parse) enum ParseErrorCode<'p> {
    #[error(transparent)]
    Lexer(LexerErrorCode<'p>),

    #[error("unexpected token, expected {expected}")]
    UnexpectedToken {
        #[label]
        span: SourceSpan,
        expected: &'static str,
    },
}

impl fmt::Display for ParseError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.code.fmt(f)
    }
}

impl std::error::Error for ParseError<'_> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.code.source()
    }
}

// Manually implement Diagnostic as a pass-through to ParseErrorCode
impl<'p> Diagnostic for ParseError<'p> {
    fn code<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.code.code()
    }

    fn severity(&self) -> Option<miette::Severity> {
        self.code.severity()
    }

    fn help<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.code.help()
    }

    fn url<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.code.url()
    }

    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        Some(&self.source)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        self.code.labels()
    }

    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        self.code.related()
    }

    fn diagnostic_source(&self) -> Option<&dyn Diagnostic> {
        self.code.diagnostic_source()
    }
}

impl<'p> From<LexerError<'p>> for ParseError<'p> {
    fn from(e: LexerError<'p>) -> Self {
        Self {
            source: e.source,
            code: ParseErrorCode::Lexer(e.code),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum Expected {
    Custom(String),
    Tokens(TokenSet),
}

impl fmt::Display for Expected {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Custom(message) => f.write_str(message),
            Self::Tokens(set) => set.fmt(f),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct TokenSet(Vec<&'static str>);

impl TokenSet {
    pub fn new(tokens: &[&'static str]) -> Self {
        assert!(!tokens.is_empty());

        Self(tokens.to_vec())
    }
}

impl fmt::Display for TokenSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self.0 {
            [] => write!(f, "a token"),
            [token] => write!(f, "`{token}`"),
            ref tokens => {
                let (last, rest) = tokens.split_last().unwrap();
                write!(f, "one of ")?;

                for token in rest {
                    write!(f, "`{token}`, ")?;
                }

                write!(f, "`{last}`")
            }
        }
    }
}
