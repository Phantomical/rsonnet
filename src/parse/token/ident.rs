use std::fmt;

use miette::SourceSpan;

use crate::lexer::Token;
use crate::parse::{Parse, ParseErrorCode, ParseResult, Parser};

#[derive(Copy, Clone, Debug)]
pub struct Ident<'p> {
    string: &'p str,
    span: SourceSpan,
}

impl<'p> Ident<'p> {
    pub fn new(string: &'p str, span: SourceSpan) -> Self {
        Self { string, span }
    }

    pub fn span(&self) -> SourceSpan {
        self.span
    }

    pub fn set_span(&mut self, span: SourceSpan) {
        self.span = span;
    }

    pub fn value(&self) -> &'p str {
        self.string
    }

    pub fn parse_any(p: &mut Parser<'p>) -> ParseResult<'p, Self> {
        match p.parse_token()? {
            Token::Ident { span, text } => Ok(Self { string: text, span }),
            token => Err(p.error(ParseErrorCode::UnexpectedToken {
                span: token.span(),
                expected: "an ident",
            })),
        }
    }
}

impl AsRef<str> for Ident<'_> {
    fn as_ref(&self) -> &str {
        self.string
    }
}

impl<'p> Parse<'p> for Ident<'p> {
    fn parse(p: &mut Parser<'p>) -> ParseResult<'p, Self> {
        use super::keyword::KEYWORDS;

        let ident = Self::parse_any(p)?;
        if KEYWORDS.contains(&ident.value()) {
            return Err(p.error(ParseErrorCode::UnexpectedToken {
                span: ident.span(),
                expected: "an ident",
            }));
        }

        Ok(ident)
    }
}

impl fmt::Display for Ident<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.string)
    }
}
