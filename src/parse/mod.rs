use self::error::ParseErrorCode;
use crate::lexer::{Lexer, LexerErrorCode, Token};

mod error;
pub mod token;

pub use self::error::{ParseError, ParseResult};

pub trait Parse<'p>: Sized {
    fn parse(p: &mut Parser<'p>) -> ParseResult<'p, Self>;
}

#[derive(Clone)]
pub struct Parser<'p> {
    lexer: Lexer<'p>,
}

impl<'p> Parser<'p> {
    pub fn new(source: &'p str) -> Self {
        Self {
            lexer: Lexer::new(source),
        }
    }

    pub fn parse<P: Parse<'p>>(&mut self) -> ParseResult<'p, P> {
        let mut copy = self.clone();
        let value = P::parse(&mut copy)?;
        *self = copy;
        Ok(value)
    }

    pub fn try_parse<F, R>(&mut self, func: F) -> ParseResult<'p, R>
    where
        F: FnOnce(&mut Self) -> ParseResult<'p, R>,
    {
        let mut copy = self.clone();
        let value = func(&mut copy)?;
        *self = copy;
        Ok(value)
    }

    pub fn parse_or_eof<P: Parse<'p>>(&mut self) -> ParseResult<'p, Option<P>> {
        match self.parse() {
            Ok(value) => Ok(Some(value)),
            Err(e) if e.is_eof() => Ok(None),
            Err(e) => Err(e),
        }
    }

    pub fn try_parse_or_eof<F, R>(&mut self, func: F) -> ParseResult<'p, Option<R>>
    where
        F: FnOnce(&mut Self) -> ParseResult<'p, R>,
    {
        match self.try_parse(func) {
            Ok(value) => Ok(Some(value)),
            Err(e) if e.is_eof() => Ok(None),
            Err(e) => Err(e),
        }
    }

    pub fn peek_parse<P: Parse<'p>>(&mut self) -> Option<P> {
        self.parse().ok()
    }

    pub fn parse_token(&mut self) -> ParseResult<'p, Token<'p>> {
        self.try_parse(|p| {
            let token: Token = p.parse()?;

            if token.is_closing() {
                return Err(p.error(ParseErrorCode::Lexer(LexerErrorCode::Eof)));
            }

            Ok(token)
        })
    }

    pub fn peek<P: Parse<'p>>(&self) -> ParseResult<'p, Option<P>> {
        match self.fork().parse() {
            Ok(token) => Ok(Some(token)),
            Err(e) if e.is_eof() => Ok(None),
            Err(e) => Err(e),
        }
    }

    pub fn fork(&self) -> Self {
        self.clone()
    }
}

impl<'p> Parse<'p> for crate::lexer::Token<'p> {
    fn parse(p: &mut Parser<'p>) -> ParseResult<'p, Self> {
        Ok(p.lexer.parse_token()?)
    }
}
