use miette::SourceSpan;

use super::ast::*;
use super::token::*;
use super::{Token, TokenKind, TokenStream};
use crate::spanext::SpanExt;

fn ts(start: SourceSpan, end: SourceSpan) -> SourceSpan {
    start.join(end)
}

peg::parser! {
    pub(crate) grammar jsonnet<'p>() for TokenStream<'p> {
        rule sp<T>(item: rule<T>) -> Spanned<T>
            = s:p() item:item() e:p()
            { Spanned { item, span: ts(s, e) } }

        rule p() -> SourceSpan
            = span:##pos_span()
            { span }

        /// Sequence with optional trailing punctuation
        rule punctuated<T, P>(item: rule<T>, punct: rule<P>) -> Vec<T>
            = item:item() ++ punct() p2:punct()? { item }
            /                                    { Vec::new() }

        // Primitive token rules. These cover all tokens that cannot be matched
        // with a string literal.

        rule id() -> Ident<'p>
            = quiet! {
                [Token { text, span, kind: TokenKind::Ident }]
                { Ident::new(text, *span) }
            }
            / expected!("id")

        rule string() -> String<'p>
            = quiet! {
                [Token { text, span, kind: TokenKind::String(value) }]
                { String::new(text, value.clone(), *span) }
            }
            / expected!("string")

        rule number() -> Number<'p>
            = quiet! {
                [Token { text, span, kind: TokenKind::Number }]
                { Number::new(text, *span) }
            }
            / expected!("number");

        // Composite rules
        pub rule expr() -> Expr<'p> = precedence! {
            assert:assert()         ";" next:bexpr()
                { Expr::Assert { assert, next } }
            "local" b:sp(<bind()>) ++ "," ";" next:bexpr()
                { Expr::Local { locals: b, next } }
            --
            s:sp(<"import" p:string() {p}>)     { Expr::Import { path: s.item, span: s.span } }
            s:sp(<"importstr" p:string() {p}>)  { Expr::ImportStr { path: s.item, span: s.span } }
            s:sp(<"importbin" p:string() {p}>)  { Expr::ImportBin { path: s.item, span: s.span } }
            s:sp(<"error" m:bexpr() {m} >)      { Expr::Error { message: s.item, span: s.span } }
            a:p() "if" cond:bexpr() "then" then:bexpr() else_:("else" e:bexpr() {e})? b:p()
                { Expr::If { cond, then, else_, span: ts(a, b) } }
            s:p() "function" "(" params:params() ")" body:bexpr() e:p()
                { Expr::FnDef { params, body, span: ts(s, e) } }
            --
            lhs:(@) "||" rhs:@  { Expr::binop(lhs, rhs, BinOp::LOr) }
            --
            lhs:(@) "&&" rhs:@  { Expr::binop(lhs, rhs, BinOp::LAnd) }
            --
            lhs:(@) "|" rhs:@   { Expr::binop(lhs, rhs, BinOp::Or) }
            --
            lhs:(@) "^" rhs:@   { Expr::binop(lhs, rhs, BinOp::Xor) }
            --
            lhs:(@) "&" rhs:@   { Expr::binop(lhs, rhs, BinOp::And) }
            --
            lhs:(@) "==" rhs:@  { Expr::binop(lhs, rhs, BinOp::Eq) }
            lhs:(@) "!=" rhs:@  { Expr::binop(lhs, rhs, BinOp::Ne) }
            --
            lhs:(@) "<" rhs:@   { Expr::binop(lhs, rhs, BinOp::Lt) }
            lhs:(@) ">" rhs:@   { Expr::binop(lhs, rhs, BinOp::Gt) }
            lhs:(@) "<=" rhs:@  { Expr::binop(lhs, rhs, BinOp::Le) }
            lhs:(@) ">=" rhs:@  { Expr::binop(lhs, rhs, BinOp::Ge) }
            lhs:(@) "in" rhs:@  { Expr::binop(lhs, rhs, BinOp::In) }
            e:(@) s:sp(<"in" "super" >)
                { Expr::InSuper { expr: Box::new(e), span: s.span } }
            --
            lhs:(@) "<<" rhs:@  { Expr::binop(lhs, rhs, BinOp::Shl) }
            lhs:(@) ">>" rhs:@  { Expr::binop(lhs, rhs, BinOp::Shr) }
            --
            lhs:(@) "+" rhs:@   { Expr::binop(lhs, rhs, BinOp::Add) }
            lhs:(@) "-" rhs:@   { Expr::binop(lhs, rhs, BinOp::Sub) }
            --
            lhs:(@) "*" rhs:@   { Expr::binop(lhs, rhs, BinOp::Mul) }
            lhs:(@) "/" rhs:@   { Expr::binop(lhs, rhs, BinOp::Div) }
            lhs:(@) "%" rhs:@   { Expr::binop(lhs, rhs, BinOp::Mod) }
            --
            s:p() "+" expr:@    { Expr::unop(expr, s, UnaryOp::Pos) }
            s:p() "-" expr:@    { Expr::unop(expr, s, UnaryOp::Neg) }
            s:p() "!" expr:@    { Expr::unop(expr, s, UnaryOp::Not) }
            s:p() "~" expr:@    { Expr::unop(expr, s, UnaryOp::Tilde) }
            --
            e:(@) "." f:id()    { Expr::Access { source: Box::new(e), field: f } }
            e:(@) params:slice_params()
                { Expr::Slice { expr: Box::new(e), params } }
            e:(@) a:p() "(" args:args() ")"
                { Expr::FnCall { func: Box::new(e), args } }
            e:(@) o:object()    { Expr::Concat { expr: Box::new(e), object: Box::new(Expr::Object(o)) } }
            --
            s:sp(<"null">)      { Expr::Null(s.span) }
            s:sp(<"true">)      { Expr::True(s.span) }
            s:sp(<"false">)     { Expr::False(s.span) }
            s:sp(<"self">)      { Expr::SelfT(s.span) }
            s:sp(<"$">)         { Expr::SelfT(s.span) }
            s:string()          { Expr::String(s) }
            n:number()          { Expr::Number(n) }
            i:id()              { Expr::Variable(i) }
            a:array()           { Expr::Array(a) }
            o:object()          { Expr::Object(o) }

            a:p() "super" "." f:id() b:p()
                { Expr::SuperField { field: f, span: ts(a, b)} }
            a:p() "super" "[" i:bexpr() "]" b:p()
                { Expr::SuperIndex { index: i, span: ts(a, b) } }
        }

        rule bexpr() -> Box<Expr<'p>> = e:expr() { Box::new(e) }

        rule slice_params() -> Spanned<Vec<Option<Expr<'p>>>> = sp(<
              "[" "::" "]"                          { vec![None, None, None]}
            / "[" params:(expr()?) **<0,3> ":" "]"  { params }
        >)

        rule object() -> Object<'p>
            = p:obj_plain() { Object::Plain(p) }
            / c:obj_comp()  { Object::Comp(c) }

        rule obj_plain() -> ObjectPlain<'p>
            = s:p() "{" members:punctuated(<member()>, <",">) "}" e:p()
            { ObjectPlain { members, span: ts(s, e) } }

        rule obj_comp() -> ObjectComp<'p>
            = s:p()
              l1:objlocal()*
              "[" field:bexpr() "]" ":" value:bexpr()
              l2:("," l:objlocal() {l})* ","?
              forspec:forspec()
              compspec:compspec()*
              e:p()
            {
                let mut locals = l1;
                locals.extend(l2.into_iter());

                let mut spec = compspec;
                spec.insert(0, CompSpec::For(forspec));

                ObjectComp {
                    locals,
                    field,
                    value,
                    compspec: spec,
                    span: ts(s, e)
                }
            }

        rule array() -> Array<'p>
            = p:array_plain()   { Array::Plain(p) }
            / p:array_comp()    { Array::Comp(p) }

        rule array_plain() -> ArrayPlain<'p>
            = s:p() "[" values:punctuated(<expr()>, <",">) "]" e:p()
            { ArrayPlain { values, span: ts(s, e) } }

        rule array_comp() -> ArrayComp<'p>
            = s:p() "[" expr:bexpr() ","? forspec:forspec() compspec:compspec()* "]" e:p()
            {
                let mut spec = compspec;
                spec.insert(0, CompSpec::For(forspec));

                ArrayComp { expr, compspec: spec, span: ts(s, e) }
            }

        rule member() -> Member<'p>
            = b:objlocal()  { Member::Local(b)  }
            / a:assert()    { Member::Assert(a) }
            / f:field()     { Member::Field(f)  }

        rule field() -> Field<'p>
            = s:p() name:fieldname() inherit:"+"? vis:h() value:bexpr() e:p()
            { Field::Named { name, inherit: inherit.is_some(), vis, value, span: ts(s, e) } }
            / s:p() name:fieldname() "(" params:params() ")" vis:h() body:bexpr() e:p()
            { Field::Function { name, params, vis, body, span: ts(s, e) } }

        rule h() -> Visibility
            = ":"   { Visibility::Visible }
            / "::"  { Visibility::Hidden }
            / ":::" { Visibility::ForceVisible }

        rule objlocal() -> ObjectLocal<'p>
            = sp(<"local" bind:bind() { bind }>)

        rule compspec() -> CompSpec<'p>
            = forspec:forspec() { CompSpec::For(forspec) }
            / ifspec:ifspec()   { CompSpec::If(ifspec) }

        rule forspec() -> ForSpec<'p>
            = s:p() "for" var:id() "in" expr:bexpr() e:p()
            { ForSpec { var, expr, span: ts(s, e) } }

        rule ifspec() -> IfSpec<'p>
            = s:p() "if" cond:bexpr() e:p()
            { IfSpec { cond, span: ts(s, e) } }

        rule fieldname() -> FieldName<'p>
            = n:id()            { FieldName::Name(n)   }
            / s:string()        { FieldName::Expr(Box::new(Expr::String(s))) }
            / "[" e:bexpr() "]" { FieldName::Expr(e)   }

        rule assert() -> Assert<'p>
            = s:p() "assert" cond:bexpr() message:(":" m:bexpr() {m})? e:p()
            { Assert { cond, message, span: ts(s, e) }}

        rule bind() -> Bind<'p>
            = name:id() "=" expr:expr() { Bind::Var(BindVar { name, expr })}
            / name:id() "(" params:params() ")" "=" expr:expr()
            { Bind::Fn(BindFn { name, params, expr }) }

        rule arg_named() -> NamedArg<'p>
            = name:id() "=" value:expr() { NamedArg { name, value } }

        rule args() -> Args<'p>
            = s:p() named:arg_named() ** "," ","? e:p()
            { Args { positional: Vec::new(), named, span: ts(s, e) }}
            / s:p() positional:expr() ++ "," named:("," arg:arg_named() {arg})* ","? e:p()
              { Args { positional, named, span: ts(s, e) } }

        rule params() -> Vec<Param<'p>>
            = param:(param:param() ++ "," ","? { param })?
            { param.unwrap_or_default() }

        rule param() -> Param<'p>
            = name:id() default:("=" e:expr() {e})?
            { Param { name, default } }
    }
}
