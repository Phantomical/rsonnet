use assert_matches::assert_matches;

use super::LexerErrorCode::*;
use super::*;

macro_rules! parse_test {
    {
        $(
            $name:ident => match $text:literal {
                $( $pattern:pat $( if $cond:expr )? $( => $arm:expr )?),* $(,)?
            }
        )*
    } => {$(
        #[test]
        fn $name() {
            #[allow(unused_variables)]
            let mut lexer = Lexer::new($text);

            $( assert_matches!(lexer.parse_token(), $pattern $( if $cond )? $( => $arm )? ); )*
        }
    )*}
}

parse_test! {
    ident => match "ident" {
        Ok(Token::Ident { text: "ident", .. })
    }

    number_0 => match "0" {
        Ok(Token::Number { text: "0", .. })
    }

    number_fraction => match "0.11111" {
        Ok(Token::Number { text: "0.11111", .. })
    }

    number_exponent => match "1e+888" {
        Ok(Token::Number { text: "1e+888", .. })
    }

    number_fraction_exponent => match "77.777e10" {
        Ok(Token::Number { text: "77.777e10", .. })
    }

    number_all_digits => match "1234567890" {
        Ok(Token::Number { text: "1234567890", .. })
    }

    number_multiple_leading_zero => match "001" {
        Err(LexerError {
            code: LeadingZerosOnNumber {
                suggestion: "1", ..
            },
            ..
        })
    }

    number_multiple_zero => match "00000.121" {
        Err(LexerError {
            code: LeadingZerosOnNumber { suggestion: "0.121", .. },
            ..
        })
    }

    operator_terminated => match "<<!>>" {
        Ok(Token::Operator { operator: "<<!>>", .. })
    }

    operator_split => match "+-~!" {
        Ok(Token::Operator { operator: "+", .. }),
        Ok(Token::Operator { operator: "-", .. }),
        Ok(Token::Operator { operator: "~", .. }),
        Ok(Token::Operator { operator: "!", .. }),
        Err(LexerError { code: Eof, .. }),
    }

    operator_with_line_comment => match "^//!@#&!(/* asasas" {
        Ok(Token::Operator { operator: "^", .. }),
        Err(LexerError { code: Eof, .. }),
    }

    operator_with_line_comment_2 => match "||// test" {
        Ok(Token::Operator { operator: "||", .. }),
        Err(LexerError { code: Eof, .. }),
    }

    operator_with_block_comment => match "^/*%&!@*/" {
        Ok(Token::Operator { operator: "^", .. }),
        Err(LexerError { code: Eof, .. }),
    }

    operator_with_block_comment_2 => match "|//* test */" {
        Ok(Token::Operator { operator: "|", .. }),
        Err(LexerError { code: Eof, .. }),
    }

    operator_with_block_string => match "/|||\n|||" {
        Ok(Token::Operator { operator: "/", .. }),
        Ok(Token::String { text: "|||\n|||", .. }),
        Err(LexerError { code: Eof, .. }),
    }

    symbols => match "{.}" {
        Ok(Token::Symbol { symbol: '{', .. }),
        Ok(Token::Symbol { symbol: '.', .. }),
        Ok(Token::Symbol { symbol: '}', .. }),
        Err(LexerError { code: Eof, .. }),
    }

    string_escapes => match r#" "\t\r\n\"\'" "# {
        Ok(Token::String { text: r#""\t\r\n\"\'""#, value, .. }) if value == "\t\r\n\"\'",
        Err(LexerError { code: Eof, .. }),
    }

    string_escape_unicode => match r#""\u12AB""# {
        Ok(Token::String { text: r#""\u12AB""#, value, .. }) if value == "\u{12AB}",
    }

    string_verbatim => match r#"@"\t\t\u12835555""# {
        Ok(Token::String { text: r#"@"\t\t\u12835555""#, value, .. }) if value == r#"\t\t\u12835555"#
    }

    string_verbatim_escaped => match r#"@"aa""aaa""# {
        Ok(Token::String { text: r#"@"aa""aaa""#, value, .. }) if value == "aa\"aaa"
    }

    string_block => match "|||  \n  test 123\r\n    test 234\n|||      " {
        Ok(Token::String{ text: "|||  \n  test 123\r\n    test 234\n|||", value, .. }) => {
            assert_eq!(value, "test 123\r\n  test 234\n");
        }
    }

    string_block_dedent => match "|||\n    aaa\n  b\n|||" {
        Err(LexerError { code: InvalidLeadingWhitespace { span }, .. }) => {
            assert_eq!(span.offset(), "|||\n    aaa\n".len());
            assert_eq!(span.len(), "  ".len());
        }
    }

    string_content_after_block_start => match "||| bl ah\n   ahhhh\n|||" {
        Err(LexerError { code: TextAfterBlockStringStart { span }, .. }) => {
            assert_eq!(span.offset(), "||| ".len());
            assert_eq!(span.len(), "bl ah".len());
        }
    }

    plus_vis => match "+:::" {
        Ok(Token::Operator { operator: "+", .. }),
        Ok(Token::Operator { operator: ":::", .. })
    }
}

// #[test]
#[allow(dead_code)]
fn display_sequence() {
    let mut lexer = Lexer::new("||| bl ah\n   ahhhh\n|||");

    loop {
        let token = lexer.parse_token();
        println!("{:?}", token);

        if token.is_err() {
            break;
        }
    }

    panic!();
}
