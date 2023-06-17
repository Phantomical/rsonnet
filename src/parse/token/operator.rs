use std::fmt;

use miette::SourceSpan;

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
                        Token::Operator { span, operator: $op } => Ok(Self(span)),
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
