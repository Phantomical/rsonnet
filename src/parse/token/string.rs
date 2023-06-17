use miette::SourceSpan;

use crate::lexer::{StringStyle, Token};
use crate::parse::error::ParseErrorCode;
use crate::parse::{Parse, ParseResult, Parser};

#[derive(Clone, Debug)]
pub struct StringT<'p> {
    span: SourceSpan,
    text: &'p str,
    value: String,
    style: StringStyle,
}

impl<'p> StringT<'p> {
    pub fn span(&self) -> SourceSpan {
        self.span
    }

    pub fn text(&self) -> &'p str {
        self.text
    }

    pub fn value(&self) -> &str {
        &self.value
    }
}

impl<'p> Parse<'p> for StringT<'p> {
    fn parse(p: &mut Parser<'p>) -> ParseResult<'p, Self> {
        match p.parse_token()? {
            Token::String {
                span,
                text,
                value,
                style,
            } => Ok(Self {
                span,
                text,
                value,
                style,
            }),
            token => Err(p.error(ParseErrorCode::UnexpectedToken {
                span: token.span(),
                expected: "a string",
            })),
        }
    }
}
