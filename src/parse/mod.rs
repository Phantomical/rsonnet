use std::fmt;
use std::ops::Deref;
use std::string::String as StdString;

use miette::SourceSpan;
use peg::{Parse, ParseElem, ParseLiteral, ParseSlice};

use crate::lexer::{LexerResult, Token as LexerToken};

mod ast;
mod error;
mod grammar;
#[cfg(test)]
mod test;
mod token;

pub use self::ast::*;
pub use self::error::{ParseError, ParseResult};
pub use self::token::*;

const KEYWORDS: &[&str] = &[
    "assert",
    "else",
    "error",
    "false",
    "for",
    "function",
    "if",
    "import",
    "importstr",
    "importbin",
    "in",
    "local",
    "null",
    "tailstrict",
    "then",
    "self",
    "super",
    "true",
];

pub struct Parser<'p> {
    tokens: TokenStream<'p>,
    source: &'p str,
}

impl<'p> Parser<'p> {
    pub fn new(source: &'p str) -> LexerResult<'p, Self> {
        let tokens = TokenStream::parse(source)?;
        Ok(Self { source, tokens })
    }

    pub fn parse_expr(&self) -> ParseResult<'p, Expr<'p>> {
        self::grammar::jsonnet::expr(&self.tokens).map_err(|e| ParseError::new(self, e))
    }
}

#[derive(Clone, Debug)]
pub struct Token<'p> {
    pub span: SourceSpan,
    pub text: &'p str,
    pub kind: TokenKind,
}

#[derive(Clone, Debug)]
pub enum TokenKind {
    Ident,
    Number,
    String(StdString),
    Literal,
}

impl<'p> From<LexerToken<'p>> for Token<'p> {
    fn from(value: LexerToken<'p>) -> Self {
        match value {
            LexerToken::Ident { span, text } if KEYWORDS.contains(&text) => Self {
                span,
                text,
                kind: TokenKind::Literal,
            },
            LexerToken::Ident { span, text } => Self {
                span,
                text,
                kind: TokenKind::Ident,
            },
            LexerToken::Number { span, text } => Self {
                span,
                text,
                kind: TokenKind::Number,
            },
            LexerToken::String {
                span, text, value, ..
            } => Self {
                span,
                text,
                kind: TokenKind::String(value),
            },
            LexerToken::Symbol { span, text, .. } => Self {
                span,
                text,
                kind: TokenKind::Literal,
            },
            LexerToken::Operator { span, operator } => Self {
                span,
                text: operator,
                kind: TokenKind::Literal,
            },
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct TokenSpan {
    offset: usize,
    len: usize,
}

impl TokenSpan {
    pub fn new(offset: usize, len: usize) -> Self {
        Self { offset, len }
    }

    pub fn range(start: usize, end: usize) -> Self {
        assert!(start <= end);

        Self::new(start, end - start)
    }

    pub fn resolve(&self, p: &TokenStream) -> SourceSpan {
        p.span_of(self.offset, self.offset + self.len)
    }
}

pub struct TokenStream<'p> {
    tokens: Vec<Token<'p>>,
}

impl<'p> TokenStream<'p> {
    pub fn parse(input: &'p str) -> LexerResult<'p, Self> {
        let mut tokens = Vec::new();
        let mut lexer = crate::lexer::Lexer::new(input);

        loop {
            match lexer.parse_token() {
                Ok(token) => tokens.push(token.into()),
                Err(e) if e.is_eof() => break,
                Err(e) => return Err(e),
            }
        }

        Ok(Self { tokens })
    }

    pub fn as_slice<'i>(&'i self) -> &'i TokenSlice<'p> {
        TokenSlice::new(self.tokens.as_slice())
    }
}

impl<'p> Deref for TokenStream<'p> {
    type Target = TokenSlice<'p>;

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

#[repr(transparent)]
pub struct TokenSlice<'p> {
    tokens: [Token<'p>],
}

impl<'p> TokenSlice<'p> {
    fn new<'i>(tokens: &'i [Token<'p>]) -> &'i Self {
        unsafe { std::mem::transmute(tokens) }
    }

    pub fn get(&self, index: usize) -> Option<&Token<'p>> {
        self.tokens.get(index)
    }

    pub fn span_of(&self, start: usize, end: usize) -> SourceSpan {
        use crate::spanext::SpanExt;

        assert!(start <= end);

        let st = &self.tokens[start];

        match self.tokens.get(end) {
            Some(et) => st.span.join(et.span),
            None => SourceSpan::new(
                st.span.offset().into(),
                (usize::MAX - st.span.offset()).into(),
            ),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct StreamPosition<'p> {
    pub span: SourceSpan,
    pub index: usize,
    pub text: &'p str,
}

impl fmt::Display for StreamPosition<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?} {}..{}",
            self.text,
            self.span.offset(),
            self.span.offset() + self.span.len()
        )
    }
}

impl<'p> Parse for TokenStream<'p> {
    type PositionRepr = StreamPosition<'p>;

    fn start(&self) -> usize {
        self.as_slice().start()
    }

    fn is_eof(&self, p: usize) -> bool {
        self.as_slice().is_eof(p)
    }

    fn position_repr(&self, p: usize) -> Self::PositionRepr {
        self.as_slice().position_repr(p)
    }
}

impl<'i, 'p: 'i> ParseElem<'i> for TokenStream<'p> {
    type Element = &'i Token<'p>;

    fn parse_elem(&'i self, pos: usize) -> peg::RuleResult<Self::Element> {
        self.as_slice().parse_elem(pos)
    }
}

impl<'p> ParseLiteral for TokenStream<'p> {
    fn parse_string_literal(&self, pos: usize, literal: &str) -> peg::RuleResult<()> {
        self.as_slice().parse_string_literal(pos, literal)
    }
}

impl<'i, 'p: 'i> ParseSlice<'i> for TokenStream<'p> {
    type Slice = &'i TokenSlice<'p>;

    fn parse_slice(&'i self, p1: usize, p2: usize) -> Self::Slice {
        self.as_slice().parse_slice(p1, p2)
    }
}

impl<'i, 'p> Parse for TokenSlice<'p> {
    type PositionRepr = StreamPosition<'p>;

    fn start(&self) -> usize {
        0
    }

    fn is_eof<'input>(&'input self, p: usize) -> bool {
        p >= self.tokens.len()
    }

    fn position_repr<'input>(&'input self, p: usize) -> Self::PositionRepr {
        match self.tokens.get(p) {
            Some(token) => StreamPosition {
                span: token.span,
                index: p,
                text: token.text,
            },
            None => StreamPosition {
                span: SourceSpan::new(0.into(), 0.into()),
                index: p,
                text: "<eof>",
            },
        }
    }
}

impl<'i, 'p: 'i> ParseElem<'i> for TokenSlice<'p> {
    type Element = &'i Token<'p>;

    fn parse_elem(&'i self, pos: usize) -> peg::RuleResult<Self::Element> {
        match self.tokens.get(pos) {
            Some(token) => peg::RuleResult::Matched(pos + 1, token),
            None => peg::RuleResult::Failed,
        }
    }
}

impl<'i, 'p> ParseLiteral for TokenSlice<'p> {
    fn parse_string_literal(&self, pos: usize, literal: &str) -> peg::RuleResult<()> {
        let Some(token) = self.tokens.get(pos) else {
            return peg::RuleResult::Failed;
        };

        if !matches!(token.kind, TokenKind::Literal) {
            return peg::RuleResult::Failed;
        }

        if token.text == literal {
            peg::RuleResult::Matched(pos + 1, ())
        } else {
            peg::RuleResult::Failed
        }
    }
}

impl<'i, 'p: 'i> ParseSlice<'i> for TokenSlice<'p> {
    type Slice = &'i TokenSlice<'p>;

    fn parse_slice(&'i self, p1: usize, p2: usize) -> Self::Slice {
        TokenSlice::new(&self.tokens[p1..p2])
    }
}
