#![allow(unused_braces, unused_mut, unused_variables, dead_code)]

use miette::SourceSpan;

use super::RawToken;

const COMMENT_START: &str = "/*";

#[rustfmt::skip]
pub(super) fn lexer(input: &str) -> (&str, SourceSpan, RawToken) {
    use RawToken::*;

    const NONE: usize = usize::MAX;

    let mut cursor: usize = 0;
    let mut marker: usize = 0;
    let mut ctxmarker: usize = 0;
    let mut yyt1 = 0;
    let mut yyt2 = 0;

    let mut s;

    let span = |s: usize, cursor: usize| SourceSpan::new(s.into(), (cursor - s).into());
    let debug = |state: usize, symbol: u8| {
        println!("lexer: state = {state}, symbol = '{}'", (symbol as char).escape_default());
    };

    let s = 'outer: loop {
        /*!re2c
            re2c:define:YYCTYPE      = u8;
            re2c:define:YYPEEK       = "input.as_bytes().get(cursor).copied().unwrap_or(0)";
            re2c:define:YYSKIP       = "cursor += 1;";
            re2c:define:YYBACKUP     = "marker = cursor;";
            re2c:define:YYRESTORE    = "cursor = marker;";
            re2c:define:YYBACKUPCTX  = "ctxmarker = cursor;";
            re2c:define:YYRESTORECTX = "cursor = ctxmarker;";
            re2c:define:YYRESTORETAG = "cursor = @@{tag};";
            re2c:define:YYLESSTHAN   = "cursor >= input.len()";
            re2c:define:YYSTAGP      = "@@{tag} = cursor;";
            re2c:define:YYSTAGN      = "@@{tag} = NONE;";
            re2c:define:YYSHIFT      = "cursor = (cursor as isize + @@{shift}) as usize;";
            re2c:define:YYSHIFTSTAG  = "@@{tag} = (@@{tag} as isize + @@{shift}) as usize;";
            re2c:define:YYDEBUG      = "debug";

            re2c:yyfill:enable = 0;
            re2c:indent:top    = 2;
            re2c:indent:string = "    ";
            //    re2c:encoding:utf8 = 1;
            re2c:eof = 0;
            re2c:tags = 1;
            re2c:unsafe = 0;

            inline_c     = ("#"|"//") .*;
            block_c      = "/*" (.|"\n")* "*/";
            comment      = inline_c | block_c;
            whitespace   = comment | [ \t\r\n];

            op_p     = [!$:~+&=<>%] | "^" | "-"; // op chars without /, |, or *
            op_c     = op_p | [/|*];             // all operator chars
            op_s     = op_p | "*";               // op chars without / or |
            // an operator is any sequence of op_c that does not contain any of
            // |||, //, or / then *.
            operator = op_s+
                     | (op_s* "/" op_p)+
                     | (op_s* "|"{1,2} op_s)+
                     | (op_s* ("/" "|"{1,2})+ op_s)+
                     | (op_s* ("|"{1,2} "/")+ op_p)+
                     ;

            ident    = [_a-zA-Z][_a-zA-Z0-9]*;
            number   = [0-9]+("."[0-9]+)?([eE][+-]?[0-9]+)?;
            symbol   = [\{\}\[\],.()\;];

            string_n = "\"" ("\\".|[^\"])* "\"" | "'" ("\\".|[^'])* "'";
            string_v = "@\"" ([^"]|"\"\"")* "\"" | "@'" ([^']|"''") "'";
            string_b = "|||" (.* "\n")* .* "|||";
            string   = string_n | string_v | string_b;

            whitespace { continue 'outer }

            @s string_n { return (&input[cursor..], span(s, cursor), String(&input[s..cursor])) }
            @s string_v { return (&input[cursor..], span(s, cursor), StringVerbatim(&input[s..cursor])) }
            @s string_b { return (&input[cursor..], span(s, cursor), StringBlock(&input[s..cursor])) }

            @s ident  { return (&input[cursor..], span(s, cursor), Ident (&input[s..cursor])) }
            @s number { return (&input[cursor..], span(s, cursor), Number(&input[s..cursor])) }
            @s symbol { return (&input[cursor..], span(s, cursor), Symbol(&input[s..cursor])) }

            // operator variants
            @s operator              { break 'outer s }
            @s op_s+ / "//"          { break 'outer s }
            @s op_s+ / "/*"          { break 'outer s }
            @s op_s+ / "|||"         { break 'outer s }
            @s op_s* "/" / [^*/]     { break 'outer s }
            @s op_s* "|"{1,2} / [^|] { break 'outer s }

            $ { return (&input[cursor..], span(cursor, cursor), Eof) }
            * { return (&input[cursor..], span(cursor, input.len()), Unknown(&input[cursor..])) }
        */
    };

    let mut op = &input[s..cursor];
    op = match op.trim_end_matches(['+', '-', '~', '!', '$']) {
        "" => &op[..1],
        trimmed => trimmed,
    };

    return (&input[s + op.len()..], span(s, s + op.len()), Operator(op))
}
