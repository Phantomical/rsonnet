use miette::SourceSpan;

use super::*;
use crate::parse::{ParseResult, Parser};

macro_rules! paren {
    ($name:ident => [$left:ident, $right:ident]) => {
        #[derive(Copy, Clone, Debug)]
        pub struct $name {
            open: SourceSpan,
            close: SourceSpan,
        }

        impl $name {
            pub fn span(&self) -> SourceSpan {
                let start = usize::min(self.open.offset(), self.close.offset());
                let end = usize::max(
                    self.open.offset() + self.open.len(),
                    self.close.offset() + self.close.len(),
                );

                (start..end).into()
            }

            pub fn parse<'p, F, R>(p: &mut Parser<'p>, func: F) -> ParseResult<'p, (Self, R)>
            where
                F: FnOnce(&mut Parser<'p>) -> ParseResult<'p, R>,
                R: 'p,
            {
                let open: $left = p.parse()?;
                let inner = func(p)?;
                let close: $right = p.parse()?;

                Ok((
                    Self {
                        open: open.span(),
                        close: close.span(),
                    },
                    inner,
                ))
            }
        }
    };
}

paren!(Paren => [LParen, RParen]);
paren!(Bracket => [LBracket, RBracket]);
paren!(Brace => [LBrace, RBrace]);
