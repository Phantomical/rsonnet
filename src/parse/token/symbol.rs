use std::fmt;

use miette::{SourceOffset, SourceSpan};

use crate::lexer::Token;
use crate::parse::error::ParseErrorCode;
use crate::parse::{Parse, ParseResult, Parser};

macro_rules! symbol_base {
    {
        $( #[$attr:meta] )*
        $name:ident => $symbol:literal;
    } => {
        $( #[$attr] )*
        #[derive(Copy, Clone)]
        pub struct $name(SourceOffset);

        impl $name {
            pub fn new(span: SourceSpan) -> Self {
                assert_eq!(span.len(), $symbol.len());
                Self(span.offset().into())
            }

            pub fn span(&self) -> SourceSpan {
                SourceSpan::new(self.0, $symbol.len().into())
            }
        }

        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let span = self.span();
                
                write!(
                    f,
                    "{}({}..{})",
                    stringify!($name),
                    span.offset(),
                    span.offset() + span.len(),
                )
            }
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str($symbol)
            }
        }
    }
}

macro_rules! symbols {
    {$(
        $( #[$attr:meta] )*
        $name:ident => $symbol:literal;
    )*} => {
        $(
            symbol_base! {
                $( #[$attr] )*
                $name => $symbol;
            }

            impl<'p> Parse<'p> for $name {
                fn parse(p: &mut Parser<'p>) -> ParseResult<'p, Self> {
                    match p.parse_token()? {
                        Token::Symbol { text: $symbol, span, .. } => Ok(Self::new(span)),
                        token => Err(p.error(ParseErrorCode::UnexpectedToken {
                            span: token.span(),
                            expected: concat!("`", $symbol, "`")
                        }))
                    }
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
            symbol_base! {
                $( #[$attr] )*
                $name => $symbol;
            }

            impl<'p> Parse<'p> for $name {
                fn parse(p: &mut Parser<'p>) -> ParseResult<'p, Self> {
                    match p.parse::<Token<'p>>()? {
                        Token::Symbol { text: $symbol, span, .. } => Ok(Self::new(span)),
                        token => Err(p.error(ParseErrorCode::UnexpectedToken {
                            span: token.span(),
                            expected: concat!("`", $symbol, "`")
                        }))
                    }
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
