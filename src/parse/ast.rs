use std::borrow::Cow;

use miette::SourceSpan;

use super::token::*;
use crate::spanext::SpanExt;

#[derive(Clone, Debug)]
pub struct Spanned<T> {
    pub item: T,
    pub span: SourceSpan,
}

impl<T> Spanned<T> {
    pub fn new(item: T, span: SourceSpan) -> Self {
        Self { item, span }
    }
}

#[derive(Clone, Debug)]
pub struct Param<'p> {
    pub name: Ident<'p>,
    pub default: Option<Expr<'p>>,
}

#[derive(Clone, Debug)]
pub struct NamedArg<'p> {
    pub name: Ident<'p>,
    pub value: Expr<'p>,
}

#[derive(Clone, Debug)]
pub struct Args<'p> {
    pub positional: Vec<Expr<'p>>,
    pub named: Vec<NamedArg<'p>>,
    pub span: SourceSpan,
}

#[derive(Clone, Debug)]
pub struct BindVar<'p> {
    pub name: Ident<'p>,
    pub expr: Expr<'p>,
}

#[derive(Clone, Debug)]
pub struct BindFn<'p> {
    pub name: Ident<'p>,
    pub params: Vec<Param<'p>>,
    pub expr: Expr<'p>,
}

#[derive(Clone, Debug)]
pub enum Bind<'p> {
    Var(BindVar<'p>),
    Fn(BindFn<'p>),
}

pub type ObjectLocal<'p> = Spanned<Bind<'p>>;

#[derive(Clone, Debug)]
pub struct Assert<'p> {
    pub span: SourceSpan,
    pub cond: Box<Expr<'p>>,
    pub message: Option<Box<Expr<'p>>>,
}

#[derive(Clone, Debug)]
pub enum FieldName<'p> {
    Name(Ident<'p>),
    Expr(Box<Expr<'p>>),
}

#[derive(Copy, Clone, Debug)]
pub enum Visibility {
    Visible,
    Hidden,
    ForceVisible,
}

#[derive(Clone, Debug)]
pub enum Field<'p> {
    Named {
        name: FieldName<'p>,
        inherit: bool,
        vis: Visibility,
        value: Box<Expr<'p>>,
        span: SourceSpan,
    },
    Function {
        name: FieldName<'p>,
        params: Vec<Param<'p>>,
        vis: Visibility,
        body: Box<Expr<'p>>,
        span: SourceSpan,
    },
}

#[derive(Clone, Debug)]
pub enum Member<'p> {
    Local(ObjectLocal<'p>),
    Assert(Assert<'p>),
    Field(Field<'p>),
}

#[derive(Clone, Debug)]
pub struct IfSpec<'p> {
    pub cond: Box<Expr<'p>>,
    pub span: SourceSpan,
}

#[derive(Clone, Debug)]
pub struct ForSpec<'p> {
    pub var: Ident<'p>,
    pub expr: Box<Expr<'p>>,
    pub span: SourceSpan,
}

#[derive(Clone, Debug)]
pub enum CompSpec<'p> {
    For(ForSpec<'p>),
    If(IfSpec<'p>),
}

#[derive(Clone, Debug)]
pub struct ObjectPlain<'p> {
    pub members: Vec<Member<'p>>,
    pub span: SourceSpan,
}

#[derive(Clone, Debug)]
pub struct ObjectComp<'p> {
    pub locals: Vec<ObjectLocal<'p>>,
    pub field: Box<Expr<'p>>,
    pub value: Box<Expr<'p>>,
    pub compspec: Vec<CompSpec<'p>>,
    pub span: SourceSpan,
}

#[derive(Clone, Debug)]
pub enum Object<'p> {
    Plain(ObjectPlain<'p>),
    Comp(ObjectComp<'p>),
}

#[derive(Clone, Debug)]
pub struct ArrayPlain<'p> {
    pub values: Vec<Expr<'p>>,
    pub span: SourceSpan,
}

#[derive(Clone, Debug)]
pub struct ArrayComp<'p> {
    pub expr: Box<Expr<'p>>,
    pub compspec: Vec<CompSpec<'p>>,
    pub span: SourceSpan,
}

#[derive(Clone, Debug)]
pub enum Array<'p> {
    Plain(ArrayPlain<'p>),
    Comp(ArrayComp<'p>),
}

#[derive(Clone, Debug)]
pub struct Slice<'p> {
    pub expr: Box<Expr<'p>>,
    pub params: Vec<Option<Expr<'p>>>,
    /// Note: only covers the [<e> : <e> : <e>] segment
    pub span: SourceSpan,
}

#[derive(Copy, Clone, Debug)]
pub enum BinOp {
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    Shl,
    Shr,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    In,
    And,
    Xor,
    Or,
    LAnd,
    LOr,
}

#[derive(Copy, Clone, Debug)]
pub enum UnaryOp {
    Neg,
    Pos,
    Not,
    Tilde,
}

#[derive(Clone, Debug)]
pub enum Expr<'p> {
    /// Used as part of the desugaring process to embed a reference to an
    /// expression that has already been desugared.
    Embed {
        index: crate::mir::ExprRef,
        span: SourceSpan,
    },

    Null(SourceSpan),
    True(SourceSpan),
    False(SourceSpan),
    SelfT(SourceSpan),
    String(String<'p>),
    Number(Number<'p>),
    Variable(Ident<'p>),
    Object(Object<'p>),
    Array(Array<'p>),
    Slice {
        expr: Box<Expr<'p>>,
        params: Spanned<Vec<Option<Expr<'p>>>>,
    },
    Access {
        source: Box<Expr<'p>>,
        field: Ident<'p>,
    },
    SuperField {
        field: Ident<'p>,
        span: SourceSpan,
    },
    SuperIndex {
        index: Box<Expr<'p>>,
        span: SourceSpan,
    },
    InSuper {
        expr: Box<Expr<'p>>,
        /// Note: only covers the `in super` tokens
        span: SourceSpan,
    },
    FnCall {
        func: Box<Expr<'p>>,
        args: Args<'p>,
    },
    FnDef {
        params: Vec<Param<'p>>,
        body: Box<Expr<'p>>,
        span: SourceSpan,
    },
    Import {
        path: String<'p>,
        span: SourceSpan,
    },
    ImportStr {
        path: String<'p>,
        span: SourceSpan,
    },
    ImportBin {
        path: String<'p>,
        span: SourceSpan,
    },
    Error {
        message: Box<Expr<'p>>,
        span: SourceSpan,
    },
    Assert {
        assert: Assert<'p>,
        next: Box<Expr<'p>>,
    },
    Local {
        locals: Vec<ObjectLocal<'p>>,
        next: Box<Expr<'p>>,
    },
    If {
        cond: Box<Expr<'p>>,
        then: Box<Expr<'p>>,
        else_: Option<Box<Expr<'p>>>,
        span: SourceSpan,
    },
    Concat {
        expr: Box<Expr<'p>>,
        object: Box<Expr<'p>>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expr<'p>>,
        span: SourceSpan,
    },
    BinaryOp {
        op: BinOp,
        lhs: Box<Expr<'p>>,
        rhs: Box<Expr<'p>>,
        span: SourceSpan,
    },
}

impl<'p> Expr<'p> {
    pub(crate) fn binop(lhs: Expr<'p>, rhs: Expr<'p>, op: BinOp) -> Self {
        Self::BinaryOp {
            op,
            span: lhs.span().join(rhs.span()),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub(crate) fn unop(expr: Expr<'p>, start: SourceSpan, op: UnaryOp) -> Self {
        Self::UnaryOp {
            op,
            span: start.join(expr.span()),
            expr: Box::new(expr),
        }
    }

    pub(crate) fn accesses(mut path: &[&'p str], span: SourceSpan) -> Self {
        assert!(!path.is_empty());

        let mut expr = None;
        while let Some((last, rest)) = path.split_last() {
            path = rest;
            expr = Some(match expr.take() {
                Some(inner) => Self::Access {
                    source: Box::new(inner),
                    field: Ident::new(last, span),
                },
                None => Self::Variable(Ident::new(last, span)),
            });
        }

        expr.unwrap()
    }

    pub(crate) fn array(elems: Vec<Expr<'p>>, span: SourceSpan) -> Self {
        Self::Array(Array::Plain(ArrayPlain {
            values: elems,
            span,
        }))
    }

    pub(crate) fn index(source: Expr<'p>, index: Expr<'p>) -> Self {
        let index_span = index.span();

        Self::Slice {
            expr: Box::new(source),
            params: Spanned::new(vec![Some(index)], index_span),
        }
    }

    pub(crate) fn id(text: &'p str, span: SourceSpan) -> Self {
        Self::Variable(Ident::new(text, span))
    }

    pub(crate) fn call_path(path: &[&'p str], args: Vec<Expr<'p>>, span: SourceSpan) -> Self {
        Self::FnCall {
            func: Box::new(Self::accesses(path, span)),
            args: Args {
                positional: args,
                named: Vec::new(),
                span,
            },
        }
    }

    pub(crate) fn error(message: impl Into<Cow<'p, str>>, span: SourceSpan) -> Self {
        let message = match message.into() {
            Cow::Owned(text) => String::new("", text, span),
            Cow::Borrowed(text) => String::new(text, text.to_owned(), span),
        };

        Self::Error {
            message: Box::new(Self::String(message)),
            span,
        }
    }

    pub(crate) fn span(&self) -> SourceSpan {
        match self {
            Self::Embed { span, .. } => *span,
            Self::Null(span) => *span,
            Self::True(span) => *span,
            Self::False(span) => *span,
            Self::SelfT(span) => *span,
            Self::String(string) => string.span(),
            Self::Number(number) => number.span(),
            Self::Variable(ident) => ident.span(),
            Self::Object(object) => object.span(),
            Self::Array(array) => array.span(),
            Self::Slice { expr, params } => expr.span().join(params.span),
            Self::Access { source, field } => source.span().join(field.span()),
            Self::Concat { expr, object } => expr.span().join(object.span()),
            Self::SuperField { span, .. } => *span,
            Self::SuperIndex { span, .. } => *span,
            Self::InSuper { expr, span } => expr.span().join(*span),
            Self::FnCall { func, args } => func.span().join(args.span),
            Self::FnDef { span, .. } => *span,
            Self::Import { span, .. } => *span,
            Self::ImportStr { span, .. } => *span,
            Self::ImportBin { span, .. } => *span,
            Self::Error { span, .. } => *span,
            Self::Assert { assert, .. } => assert.span,
            Self::Local { locals, .. } => match locals.last() {
                Some(last) => locals[0].span.join(last.span),
                None => locals[0].span,
            },
            Self::If { span, .. } => *span,
            Self::UnaryOp { span, .. } => *span,
            Self::BinaryOp { span, .. } => *span,
        }
    }
}

impl<'p> Object<'p> {
    pub(crate) fn span(&self) -> SourceSpan {
        match self {
            Self::Plain(obj) => obj.span,
            Self::Comp(obj) => obj.span,
        }
    }
}

impl<'p> Array<'p> {
    pub(crate) fn span(&self) -> SourceSpan {
        match self {
            Self::Plain(array) => array.span,
            Self::Comp(array) => array.span,
        }
    }
}

impl<'p> Param<'p> {
    pub(crate) fn span(&self) -> SourceSpan {
        match &self.default {
            Some(default) => self.name.span().join(default.span()),
            None => self.name.span(),
        }
    }
}

impl<'p> Bind<'p> {
    pub(crate) fn span(&self) -> SourceSpan {
        match self {
            Self::Var(var) => var.name.span().join(var.expr.span()),
            Self::Fn(func) => func.name.span().join(func.expr.span()),
        }
    }
}

impl<'p> NamedArg<'p> {
    pub(crate) fn span(&self) -> SourceSpan {
        self.name.span().join(self.value.span())
    }
}
