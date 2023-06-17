use std::fmt;

use miette::SourceSpan;

use crate::parse::token::Ident;
use crate::parse::{Parse, ParseErrorCode, ParseResult, Parser};

macro_rules! keywords {
    {$(
        $( #[$attr:meta] )*
        $name:ident => $kw:literal;
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
                    let ident = Ident::parse_any(p)?;

                    match ident.value() {
                        $kw => Ok(Self(ident.span())),
                        _ => Err(p.error(ParseErrorCode::UnexpectedToken {
                            span: ident.span(),
                            expected: concat!("`", $kw, "`"),
                        }))
                    }
                }
            }

            impl fmt::Display for $name {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.write_str($kw)
                }
            }
        )*

        pub(crate) const KEYWORDS: &[&str] = &[
            $( $kw, )*
        ];
    };
}

keywords! {
    Assert => "assert";
    Else => "else";
    Error => "error";
    False => "false";
    For => "for";
    Function => "function";
    If => "if";
    Import => "import";
    ImportStr => "importstr";
    ImportBin => "importbin";
    In => "in";
    Local => "local";
    Null => "null";
    TailStrict => "tailstrict";
    Then => "then";
    SelfT => "self";
    Super => "super";
    True => "true";
}
