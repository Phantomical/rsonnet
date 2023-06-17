use miette::SourceSpan;

use super::*;
use crate::parse::{ParseResult, Parser};
use crate::spanext::SpanExt;

macro_rules! paren {
    ($name:ident => [$left:ident, $right:ident]) => {
        #[derive(Copy, Clone, Debug)]
        pub struct $name {
            open: $left,
            close: $right,
        }

        impl $name {
            pub fn span(&self) -> SourceSpan {
                self.open.span().join(self.close.span())
            }

            pub fn parse<'p, F, R>(p: &mut Parser<'p>, func: F) -> ParseResult<'p, (Self, R)>
            where
                F: FnOnce(&mut Parser<'p>) -> ParseResult<'p, R>,
                R: 'p,
            {
                let open: $left = p.parse()?;
                let inner = func(p)?;
                let close: $right = p.parse()?;

                Ok((Self { open, close }, inner))
            }
        }
    };
}

paren!(Paren => [LParen, RParen]);
paren!(Bracket => [LBracket, RBracket]);
paren!(Brace => [LBrace, RBrace]);
