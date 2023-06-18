use std::fmt;

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Clone, Debug)]
pub struct LexerError<'p> {
    pub(crate) source: &'p str,
    pub(crate) code: LexerErrorCode<'p>,
}
#[derive(Clone, Debug, Error, Diagnostic)]
pub(crate) enum LexerErrorCode<'p> {
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

impl<'p> LexerError<'p> {
    pub(super) fn new(source: &'p str, code: LexerErrorCode<'p>) -> Self {
        Self { source, code }
    }

    pub fn is_eof(&self) -> bool {
        matches!(self.code, LexerErrorCode::Eof)
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
