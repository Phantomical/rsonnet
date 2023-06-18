use assert_matches::assert_matches;

use super::ast::*;
use super::*;

macro_rules! parse_test {
    {
        $(
            $name:ident => match $text:literal {
                $pattern:pat $( if $cond:expr )? $( => $arm:expr )?
            }
        )*
    } => {$(
        #[test]
        fn $name() -> miette::Result<()> {
            let parser = Parser::new($text)?;
            let expr = parser.parse_expr()?;

            assert_matches!(
                expr,
                $pattern $( if $cond )? $( => $arm )?
            );

            Ok(())
        }
    )*}
}

// #[test]
#[allow(dead_code)]
fn debug_parse() {
    let text = r#"
    [
        x
        for x in y
        if x == 2
    ]
    "#;

    let parser = match Parser::new(text) {
        Ok(parser) => parser,
        Err(e) => panic!("lexer: {}", e),
    };

    let expr = match parser.parse_expr() {
        Ok(expr) => expr,
        Err(e) => panic!("parser: {}", e),
    };

    eprintln!("{expr:#?}");
    panic!()
}

parse_test! {
    empty_object => match "{}" {
        Expr::Object(Object::Plain(ObjectPlain { members, .. })) if members.is_empty()
    }

    empty_array => match "[]" {
        Expr::Array(Array::Plain(ArrayPlain { values, .. })) if values.is_empty()
    }

    blank_slice => match "x[::]" {
        Expr::Slice {
            expr,
            params: Spanned {
                item: params,
                ..
            }
        } => {
            assert_matches!(*expr, Expr::Variable(var) if var.text() == "x");
            assert_matches!(*params, [None, None, None]);
        }
    }

    array_comp => match r#"[x for x in y if x == 2]"# {
        Expr::Array(Array::Comp(ArrayComp { expr, .. })) => {
            assert_matches!(*expr, Expr::Variable(var) if var.text() == "x");
        }
    }
}
