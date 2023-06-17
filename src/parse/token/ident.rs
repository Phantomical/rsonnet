use std::fmt;

use miette::{SourceOffset, SourceSpan};

use crate::lexer::Token;
use crate::parse::{Parse, ParseErrorCode, ParseResult, Parser};

#[derive(Copy, Clone)]
pub struct Ident<'p> {
    string: &'p str,
    offset: SourceOffset,
}

impl<'p> Ident<'p> {
    pub fn new(string: &'p str, span: SourceSpan) -> Self {
        assert_eq!(span.len(), string.len());
        Self {
            string,
            offset: span.offset().into(),
        }
    }

    pub fn span(&self) -> SourceSpan {
        SourceSpan::new(self.offset, self.string.len().into())
    }

    pub fn value(&self) -> &'p str {
        self.string
    }

    pub fn parse_any(p: &mut Parser<'p>) -> ParseResult<'p, Self> {
        match p.parse_token()? {
            Token::Ident { span, text } => Ok(Self::new(text, span)),
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

impl fmt::Debug for Ident<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let span = self.span();

        f.debug_struct("Ident")
            .field("text", &self.string)
            .field(
                "span",
                &format_args!("{}..{}", span.offset(), span.offset() + span.len()),
            )
            .finish()
    }
}
