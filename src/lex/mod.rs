use std::fmt;

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

mod lexer;
#[cfg(test)]
mod test;

pub type LexerResult<'p, T> = Result<T, LexerError<'p>>;

pub struct Lexer<'p> {
    source: &'p str,
    current: &'p str,
}

impl<'p> Lexer<'p> {
    pub fn new(source: &'p str) -> Self {
        Self {
            source,
            current: source,
        }
    }

    fn span_of(&self, text: &'p str) -> SourceSpan {
        let offset = text.as_ptr() as usize - self.source.as_ptr() as usize;
        SourceSpan::new(offset.into(), text.len().into())
    }

    pub fn parse_token(&mut self) -> Result<Token<'p>, LexerError<'p>> {
        let (next, span, raw) = self::lexer::lexer(self.current);
        self.current = next;

        match raw {
            RawToken::Ident(text) => Ok(Token::Ident { span, text }),
            RawToken::Number(text) => Ok(Token::Number {
                span,
                text: self.parse_number(text)?,
            }),
            RawToken::Symbol(text) => {
                assert!(text.len() == 1);
                Ok(Token::Symbol {
                    span,
                    symbol: text.chars().next().unwrap() as char,
                })
            }
            RawToken::Operator(text) => Ok(Token::Operator {
                span,
                operator: text,
            }),
            RawToken::String(text) => Ok(Token::String {
                span,
                text,
                style: StringStyle::Quoted,
                value: self
                    .parse_string_quoted(text)
                    .map_err(|code| LexerError::new(self.source, code))?,
            }),
            RawToken::StringVerbatim(text) => Ok(Token::String {
                span,
                text,
                style: StringStyle::Verbatim,
                value: self
                    .parse_string_verbatim(text)
                    .map_err(|code| LexerError::new(self.source, code))?,
            }),
            RawToken::StringBlock(text) => Ok(Token::String {
                span,
                text,
                style: StringStyle::Block,
                value: self
                    .parse_string_block(text)
                    .map_err(|code| LexerError::new(self.source, code))?,
            }),
            RawToken::Eof => Err(LexerError {
                source: self.source,
                code: LexerErrorCode::Eof,
            }),
            RawToken::Unknown(text) => Err(LexerError {
                source: self.source,
                code: LexerErrorCode::UnexpectedToken {
                    token: text,
                    span: self.span_of(text),
                },
            }),
        }
    }

    fn parse_number(&self, text: &'p str) -> Result<&'p str, LexerError<'p>> {
        let mut chars = text.chars();

        match (chars.next(), chars.next()) {
            (Some('0'), Some('0'..='9')) => (),
            _ => return Ok(text),
        }

        let mut lag = text.chars();
        let mut lead = text.chars().peekable();

        loop {
            match lead.next() {
                Some('0') if lead.peek() == Some(&'.') => break,
                Some('0') => (),
                _ => break,
            }

            lag.next();
        }

        return Err(LexerError {
            source: self.source,
            code: LexerErrorCode::LeadingZerosOnNumber {
                span: self.span_of(text),
                suggestion: lag.as_str(),
            },
        });
    }

    fn parse_string_quoted(&self, mut text: &'p str) -> Result<String, LexerErrorCode<'p>> {
        let mut value = String::new();

        let quote = match text.chars().next() {
            Some(c @ ('"' | '\'')) => {
                text = text.split_at(1).1;
                c
            }
            Some(c) => {
                return Err(LexerErrorCode::UnexpectedChar {
                    c,
                    span: self.span_of(&text[..c.len_utf8()]),
                })
            }
            None => panic!("re2c lexer returned an empty string"),
        };

        loop {
            let index = match text.find(['\\', quote]) {
                Some(index) => index,
                None => panic!("re2c lexer returned an invalid string"),
            };

            let (head, rest) = text.split_at(index);
            value.push_str(head);
            text = rest;

            match text.as_bytes() {
                [] => panic!("lexer returned string with no closing quote"),
                [c] if *c == quote as _ => break,
                [c, ..] if *c == quote as _ => panic!("unescaped quote in the middle of a string"),
                [b'\\', c @ (b'"' | b'\'' | b'\\' | b'/'), ..] => value.push(*c as char),
                [b'\\', b'b', ..] => value.push(0x08 as char),
                [b'\\', b'f', ..] => value.push(0x0C as char),
                [b'\\', b'n', ..] => value.push('\n'),
                [b'\\', b'r', ..] => value.push('\r'),
                [b'\\', b't', ..] => value.push('\t'),
                [b'\\', b'u', ..] => {
                    if text.len() < 6 {
                        return Err(LexerErrorCode::InvalidStringEscape {
                            escape: text.into(),
                            span: self.span_of(text),
                        });
                    }

                    let (escape, rest) = text.split_at(6);
                    text = rest;
                    let escape = escape;

                    let code: u32 = u32::from_str_radix(&escape[2..6], 16).map_err(|_| {
                        LexerErrorCode::InvalidUnicodeEscape {
                            escape: escape.into(),
                            span: self.span_of(escape),
                        }
                    })?;
                    let code = char::from_u32(code).ok_or_else(|| {
                        LexerErrorCode::InvalidUnicodeEscape {
                            escape: escape.into(),
                            span: self.span_of(escape),
                        }
                    })?;

                    value.push(code);
                    continue;
                }
                [b'\\', ..] => {
                    let index = text.char_indices().nth(2).unwrap_or((1, '\0')).0;
                    let escape = &text[..index];

                    return Err(LexerErrorCode::InvalidStringEscape {
                        escape: escape.into(),
                        span: self.span_of(escape),
                    });
                }
                [c, ..] if *c >= 0x80 => unreachable!(),
                [c, ..] => {
                    value.push(*c as char);
                    text = text.split_at(1).1;
                    continue;
                }
            }

            text = text.split_at(2).1;
        }

        Ok(value)
    }

    fn parse_string_verbatim(&self, mut text: &'p str) -> Result<String, LexerErrorCode<'p>> {
        let mut value = String::new();

        assert!(
            text.starts_with("@"),
            "re2c lexer returned an invalid string"
        );
        text = text.split_at(1).1;

        let quote = match text.chars().next() {
            Some(c @ ('"' | '\'')) => {
                text = text.split_at(1).1;
                c
            }
            Some(c) => {
                return Err(LexerErrorCode::UnexpectedChar {
                    c,
                    span: self.span_of(&text[..c.len_utf8()]),
                })
            }
            None => panic!("re2c lexer returned an empty string"),
        };

        loop {
            let index = match text.find(quote) {
                Some(index) => index,
                None => panic!("re2c lexer returned an invalid string"),
            };

            let (head, rest) = text.split_at(index);
            text = rest;
            value.push_str(head);

            match text.as_bytes() {
                [] => unreachable!(),
                [_] => break,
                [_, b, ..] if *b == quote as _ => {
                    text = text.split_at(2).1;
                    value.push(*b as char);
                }
                _ => unreachable!(),
            }
        }

        Ok(value)
    }

    fn parse_string_block(&self, mut text: &'p str) -> Result<String, LexerErrorCode<'p>> {
        let mut value = String::new();

        text = text
            .strip_prefix("|||")
            .expect("re2c lexer returned invalid block string");

        text = text.trim_start_matches([' ', '\t', '\r']);
        text = match text.strip_prefix("\n") {
            Some(text) => text,
            None => {
                let content = match text.find('\n') {
                    Some(index) => &text[..index],
                    None => text,
                };

                return Err(LexerErrorCode::TextAfterBlockStringStart {
                    span: self.span_of(content),
                });
            }
        };

        let next = text.trim_start_matches([' ', '\t']);
        let offset = next.as_ptr() as usize - text.as_ptr() as usize;
        let (prefix, rest) = text.split_at(offset);

        match prefix {
            "" if rest == "|||" => return Ok(value),
            "" => {
                return Err(LexerErrorCode::MissingLeadingWhitespace {
                    span: self.span_of(&text[..0]),
                })
            }
            _ => (),
        }

        if rest == "|||" {
            return Ok(value);
        }

        loop {
            text = match text.strip_prefix(prefix) {
                Some(text) => text,
                None => {
                    let next = text.trim_start_matches([' ', '\t']);
                    let offset = next.as_ptr() as usize - text.as_ptr() as usize;
                    let prefix = &text[..offset];

                    if next == "|||" {
                        break;
                    }

                    return Err(LexerErrorCode::InvalidLeadingWhitespace {
                        span: self.span_of(prefix),
                    });
                }
            };

            let offset = text
                .find('\n')
                .expect("re2c lexer returned invalid block string");

            let (head, rest) = text.split_at(offset + 1);
            text = rest;
            value.push_str(head);
        }

        Ok(value)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum StringStyle {
    Quoted,
    Verbatim,
    Block,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Token<'p> {
    Ident {
        span: SourceSpan,
        text: &'p str,
    },
    Number {
        span: SourceSpan,
        text: &'p str,
    },
    Symbol {
        span: SourceSpan,
        symbol: char,
    },
    Operator {
        span: SourceSpan,
        operator: &'p str,
    },
    String {
        span: SourceSpan,
        text: &'p str,
        value: String,
        style: StringStyle,
    },
}

enum RawToken<'p> {
    Eof,
    Ident(&'p str),
    Number(&'p str),
    String(&'p str),
    StringVerbatim(&'p str),
    StringBlock(&'p str),
    Symbol(&'p str),
    Operator(&'p str),
    Unknown(&'p str),
}

#[derive(Clone, Debug, Error, Diagnostic)]
enum LexerErrorCode<'p> {
    #[error("unexpected end of file")]
    Eof,

    #[error("unexpected character `{c}`")]
    UnexpectedChar {
        c: char,
        #[label]
        span: SourceSpan,
    },

    #[error("unexpected token `{token}`")]
    UnexpectedToken {
        token: &'p str,
        #[label]
        span: SourceSpan,
    },

    #[error("invalid string escape `{escape}`")]
    InvalidStringEscape {
        escape: &'p str,
        #[label]
        span: SourceSpan,
    },

    #[error("invalid unicode escape `{escape}`")]
    #[diagnostic(help("the maximum valid unicode value is \\u10FFFF"))]
    InvalidUnicodeEscape {
        escape: &'p str,
        #[label]
        span: SourceSpan,
    },

    #[error("block strings may not have text after `|||` on the same line")]
    TextAfterBlockStringStart {
        #[label]
        span: SourceSpan,
    },

    #[error("block string contents must be indented by some amount of whitespace")]
    MissingLeadingWhitespace {
        #[label]
        span: SourceSpan,
    },

    #[error("leading whitespace did not match that of the first line")]
    #[diagnostic(help("note that the leading whitespace must match exactly"))]
    InvalidLeadingWhitespace {
        #[label]
        span: SourceSpan,
    },

    #[error("jsonnet literals starting with `0` may not have digits following it")]
    #[diagnostic(help("remove the leading zeros: `{suggestion}`"))]
    LeadingZerosOnNumber {
        #[label]
        span: SourceSpan,

        suggestion: &'p str,
    },
}

#[derive(Clone, Debug)]
pub struct LexerError<'p> {
    source: &'p str,
    code: LexerErrorCode<'p>,
}

impl<'p> LexerError<'p> {
    fn new(source: &'p str, code: LexerErrorCode<'p>) -> Self {
        Self { source, code }
    }
}

impl fmt::Display for LexerError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.code.fmt(f)
    }
}

impl std::error::Error for LexerError<'_> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.code.source()
    }
}

impl<'p> Diagnostic for LexerError<'p> {
    fn code<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.code.code()
    }

    fn severity(&self) -> Option<miette::Severity> {
        self.code.severity()
    }

    fn help<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.code.help()
    }

    fn url<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.code.url()
    }

    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        Some(&self.source)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        self.code.labels()
    }

    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        self.code.related()
    }

    fn diagnostic_source(&self) -> Option<&dyn Diagnostic> {
        self.code.diagnostic_source()
    }
}
