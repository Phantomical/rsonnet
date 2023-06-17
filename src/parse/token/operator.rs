use std::fmt;

use miette::{SourceOffset, SourceSpan};

use crate::lexer::Token;
use crate::parse::error::ParseErrorCode;
use crate::parse::{Parse, ParseResult, Parser};

macro_rules! operators {
    {$(
        $( #[$attr:meta] )*
        $name:ident => $op:literal;
    )*} => {
        $(
            $( #[$attr] )*
            #[derive(Copy, Clone)]
            pub struct $name(SourceOffset);

            impl $name {
                pub fn new(span: SourceSpan) -> Self {
                    assert_eq!(span.len(), $op.len());
                    Self(span.offset().into())
                }

                pub fn span(&self) -> SourceSpan {
                    SourceSpan::new(self.0, $op.len().into())
                }
            }

            impl<'p> Parse<'p> for $name {
                fn parse(p: &mut Parser<'p>) -> ParseResult<'p, Self> {
                    match p.parse_token()? {
                        Token::Operator { span, operator: $op } => Ok(Self::new(span)),
                        token => Err(p.error(ParseErrorCode::UnexpectedToken {
                            span: token.span(),
                            expected: concat!("`", $op, "`")
                        }))
                    }
                }
            }

            impl fmt::Display for $name {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.write_str($op)
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
        )*
    }
}

operators! {
    Mul => "*";
    Div => "/";
    Mod => "%";
    Plus => "+";
    Minus => "-";
    Shl => "<<";
    Shr => ">>";
    Less => "<";
    LessEqual => "<=";
    Greater => ">";
    GreaterEqual => ">=";
    Equal => "==";
    NotEqual => "!=";
    And => "&";
    Or => "|";
    Xor => "^";
    LogicalAnd => "&&";
    LogicalOr => "||";
    Not => "!";
    Tilde => "~";
    Visible => ":";
    Hidden => "::";
    ForceVisible => ":::";
    Dollar => "$";
    Assign => "=";
}
