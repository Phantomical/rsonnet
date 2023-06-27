use std::borrow::Cow;

use gc_arena_derive::Collect;
use miette::SourceSpan;

use crate::parse::Visibility;

#[derive(Copy, Clone, Debug, Collect)]
#[collect(require_static)]
pub struct Span {
    file: usize,
    span: SourceSpan,
}

impl Span {
    pub fn new(file: usize, span: SourceSpan) -> Self {
        Self { file, span }
    }

    pub fn file(&self) -> usize {
        self.file
    }

    pub fn span(&self) -> SourceSpan {
        self.span
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Spanned<T> {
    pub item: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(item: T, span: Span) -> Self {
        Self { item, span }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Collect)]
#[collect(require_static)]
pub struct ExprRef(pub usize);

#[derive(Clone, Debug)]
pub struct Param {
    pub name: Spanned<String>,
    pub value: ExprRef,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct NamedField {
    pub field: ExprRef,
    pub vis: Visibility,
    pub value: ExprRef,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub span: Span,
    pub data: ExprData,
}

#[derive(Clone, Debug)]
pub enum ExprData {
    /// Special variable referring to the standard library.
    Std,

    Null,
    True,
    False,
    This,
    Super,
    Ident(Cow<'static, str>),
    String(Cow<'static, str>),
    Number(f64),
    Error(ExprRef),

    Import(String),
    ImportStr(String),
    ImportBin(String),

    /// Indicates that self and super should actually refer to
    /// self and super in the parent scope.
    UpScope(ExprRef),

    Index {
        expr: ExprRef,
        index: ExprRef,
    },

    Locals {
        locals: Vec<Param>,
        expr: ExprRef,
    },
    Object {
        effects: Vec<ExprRef>,
        fields: Vec<NamedField>,
    },

    /// Object comprehension.
    ///
    /// ```text
    /// {
    ///     [<field>]: <value>
    ///     for <var> in <seq>
    /// }
    /// ```
    ObjectComp {
        field: ExprRef,
        value: ExprRef,
        var: Cow<'static, str>,
        seq: ExprRef,
    },

    Array {
        elems: Vec<ExprRef>,
    },

    Function {
        params: Vec<Param>,
        body: ExprRef,
    },

    If {
        cond: ExprRef,
        then: ExprRef,
        else_: ExprRef,
    },

    Call {
        func: ExprRef,
        positional: Vec<ExprRef>,
        named: Vec<Param>,
    },

    Not(ExprRef),
    Neg(ExprRef),
    Pos(ExprRef),
    BitNot(ExprRef),

    Mul(ExprRef, ExprRef),
    Div(ExprRef, ExprRef),
    Add(ExprRef, ExprRef),
    Sub(ExprRef, ExprRef),
    Shl(ExprRef, ExprRef),
    Shr(ExprRef, ExprRef),
    Lt(ExprRef, ExprRef),
    Le(ExprRef, ExprRef),
    Gt(ExprRef, ExprRef),
    Ge(ExprRef, ExprRef),
    And(ExprRef, ExprRef),
    Xor(ExprRef, ExprRef),
    Or(ExprRef, ExprRef),
    LAnd(ExprRef, ExprRef),
    LOr(ExprRef, ExprRef),
}
