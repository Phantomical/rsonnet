use std::fmt;

use miette::SourceSpan;

use crate::lexer::Token;
use crate::parse::error::ParseErrorCode;
use crate::parse::{Parse, ParseResult, Parser};

macro_rules! symbols {
    {$(
        $( #[$attr:meta] )*
        $name:ident => $symbol:literal;
    )*} => {
        $(
            $( #[$attr] )*
            #[derive(Copy, Clone, Debug)]
            pub struct $name(SourceSpan);

            impl $name {
                pub fn span(&self) -> SourceSpan {
                    self.0
                }

                pub fn set_span(&mut self, span: SourceSpan) {
                    self.0 = span;
                }
            }

            impl<'p> Parse<'p> for $name {
                fn parse(p: &mut Parser<'p>) -> ParseResult<'p, Self> {
                    match p.parse_token()? {
                        Token::Symbol { text: $symbol, span, .. } => Ok(Self(span)),
                        token => Err(p.error(ParseErrorCode::UnexpectedToken {
                            span: token.span(),
                            expected: concat!("`", $symbol, "`")
                        }))
                    }
                }
            }

            impl fmt::Display for $name {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.write_str($symbol)
                }
            }
        )*
    };
}

macro_rules! symbols_raw {
    {$(
        $( #[$attr:meta] )*
        $name:ident => $symbol:literal;
    )*} => {
        $(
            $( #[$attr] )*
            #[derive(Copy, Clone, Debug)]
            pub struct $name(SourceSpan);

            impl $name {
                pub fn span(&self) -> SourceSpan {
                    self.0
                }

                pub fn set_span(&mut self, span: SourceSpan) {
                    self.0 = span;
                }
            }

            impl<'p> Parse<'p> for $name {
                fn parse(p: &mut Parser<'p>) -> ParseResult<'p, Self> {
                    match p.parse::<Token<'p>>()? {
                        Token::Symbol { text: $symbol, span, .. } => Ok(Self(span)),
                        token => Err(p.error(ParseErrorCode::UnexpectedToken {
                            span: token.span(),
                            expected: concat!("`", $symbol, "`")
                        }))
                    }
                }
            }

            impl fmt::Display for $name {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.write_str($symbol)
                }
            }
        )*
    };
}

symbols! {
    Comma => ",";
    Dot => ".";
    SemiColon => ";";
}

symbols_raw! {
    LParen => "(";
    RParen => ")";
    LBrace => "{";
    RBrace => "}";
    LBracket => "[";
    RBracket => "]";
}
