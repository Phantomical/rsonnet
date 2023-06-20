use std::borrow::Cow;

use miette::SourceSpan;

pub use crate::parse::Spanned;
use crate::parse::Visibility;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct ExprRef(pub usize);

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct ScopeRef(pub usize);

#[derive(Clone, Debug)]
pub struct Param<'p> {
    pub name: Spanned<&'p str>,
    pub value: ExprRef,
    pub span: SourceSpan,
}

#[derive(Clone, Debug)]
pub struct NamedField {
    pub field: ExprRef,
    pub vis: Visibility,
    pub value: ExprRef,
    pub span: SourceSpan,
}

#[derive(Clone, Debug)]
pub enum Field<'p> {
    /// A field within the object
    Named(NamedField),
    /// A local variable within the object
    Local(Param<'p>),
    /// An expression that must be evaluated when expanding the object. It has
    /// no side effects but if it evaluates to an error then so does the entire
    /// object.
    Effect(ExprRef),
}

#[derive(Clone, Debug)]
pub struct Expr<'p> {
    pub span: SourceSpan,
    pub data: ExprData<'p>,
}

#[derive(Clone, Debug)]
pub enum ExprData<'p> {
    /// Special variable referring to the standard library.
    Std,

    Null,
    True,
    False,
    ObjSelf,
    Super,
    Ident(Cow<'p, str>),
    String(Cow<'p, str>),
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
        locals: Vec<Param<'p>>,
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
        var: &'p str,
        seq: ExprRef,
    },

    Array {
        elems: Vec<ExprRef>,
    },

    Function {
        params: Vec<Param<'p>>,
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
        named: Vec<Param<'p>>,
    },

    Not(ExprRef),
    Neg(ExprRef),
    Pos(ExprRef),
    Tilde(ExprRef),

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
