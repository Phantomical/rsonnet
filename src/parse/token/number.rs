use std::num::ParseFloatError;

use miette::SourceSpan;

use crate::lexer::Token;
use crate::parse::error::ParseErrorCode;
use crate::parse::{Parse, ParseResult, Parser};

#[derive(Copy, Clone, Debug)]
pub struct Number<'p> {
    span: SourceSpan,
    text: &'p str,
    value: Option<f64>,
}

impl<'p> Number<'p> {
    pub fn span(&self) -> SourceSpan {
        self.span
    }

    pub fn text(&self) -> &'p str {
        self.text
    }

    pub fn value(&self) -> Result<f64, ParseFloatError> {
        self.text.parse()
    }
}

impl<'p> Parse<'p> for Number<'p> {
    fn parse(p: &mut Parser<'p>) -> ParseResult<'p, Self> {
        match p.parse_token()? {
            Token::Number { span, text } => Ok(Self {
                span,
                text,
                value: text.parse().ok(),
            }),
            token => Err(p.error(ParseErrorCode::UnexpectedToken {
                span: token.span(),
                expected: "a number",
            })),
        }
    }
}
