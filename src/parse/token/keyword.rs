use std::fmt;

use miette::{SourceOffset, SourceSpan};

use crate::parse::token::Ident;
use crate::parse::{Parse, ParseErrorCode, ParseResult, Parser};

macro_rules! keywords {
    {$(
        $( #[$attr:meta] )*
        $name:ident => $kw:literal;
    )*} => {
        $(
            $( #[$attr] )*
            #[derive(Copy, Clone)]
            pub struct $name(SourceOffset);

            impl $name {
                pub fn new(span: SourceSpan) -> Self {
                    assert_eq!(span.len(), $kw.len());
                    Self(span.offset().into())
                }

                pub fn span(&self) -> SourceSpan {
                    SourceSpan::new(self.0, $kw.len().into())
                }
            }

            impl<'p> Parse<'p> for $name {
                fn parse(p: &mut Parser<'p>) -> ParseResult<'p, Self> {
                    let ident = Ident::parse_any(p)?;

                    match ident.value() {
                        $kw => Ok(Self::new(ident.span())),
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
