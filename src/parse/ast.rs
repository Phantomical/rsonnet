use super::token::*;
use super::TokenSpan;

#[derive(Clone, Debug)]
pub struct Spanned<T> {
    pub item: T,
    pub span: TokenSpan,
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
}

#[derive(Clone, Debug)]
pub struct BindVar<'p> {
    pub name: Ident<'p>,
    pub expr: Box<Expr<'p>>,
}

#[derive(Clone, Debug)]
pub struct BindFn<'p> {
    pub name: Ident<'p>,
    pub params: Vec<Param<'p>>,
    pub expr: Box<Expr<'p>>,
}

#[derive(Clone, Debug)]
pub enum Bind<'p> {
    Var(BindVar<'p>),
    Fn(BindFn<'p>),
}

pub type ObjectLocal<'p> = Spanned<Bind<'p>>;

#[derive(Clone, Debug)]
pub struct Assert<'p> {
    pub span: TokenSpan,
    pub cond: Box<Expr<'p>>,
    pub message: Option<Box<Expr<'p>>>,
}

#[derive(Clone, Debug)]
pub enum FieldName<'p> {
    Name(Ident<'p>),
    String(String<'p>),
    Expr(Box<Expr<'p>>),
}

#[derive(Clone, Debug)]
pub struct IfSpec<'p> {
    pub cond: Box<Expr<'p>>,
    pub span: TokenSpan,
}

#[derive(Clone, Debug)]
pub struct ForSpec<'p> {
    pub var: Ident<'p>,
    pub expr: Box<Expr<'p>>,
    pub span: TokenSpan,
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
    },
    Function {
        name: FieldName<'p>,
        params: Vec<Param<'p>>,
        vis: Visibility,
        body: Box<Expr<'p>>,
    },
}

#[derive(Clone, Debug)]
pub enum Member<'p> {
    Local(ObjectLocal<'p>),
    Assert(Assert<'p>),
    Field(Field<'p>),
}

#[derive(Clone, Debug)]
pub struct CompSpec<'p> {
    pub forspec: ForSpec<'p>,
    pub filters: Vec<IfSpec<'p>>,
}

#[derive(Clone, Debug)]
pub struct ObjectPlain<'p> {
    pub members: Vec<Member<'p>>,
    pub span: TokenSpan,
}

#[derive(Clone, Debug)]
pub struct ObjectComp<'p> {
    pub locals: Vec<ObjectLocal<'p>>,
    pub field: Box<Expr<'p>>,
    pub value: Box<Expr<'p>>,
    pub spec: Vec<CompSpec<'p>>,
    pub span: TokenSpan,
}

#[derive(Clone, Debug)]
pub enum Object<'p> {
    Plain(ObjectPlain<'p>),
    Comp(ObjectComp<'p>),
}

#[derive(Clone, Debug)]
pub struct ArrayPlain<'p> {
    pub values: Vec<Expr<'p>>,
    pub span: TokenSpan,
}

#[derive(Clone, Debug)]
pub struct ArrayComp<'p> {
    pub expr: Box<Expr<'p>>,
    pub spec: Vec<CompSpec<'p>>,
    pub span: TokenSpan,
}

#[derive(Clone, Debug)]
pub enum Array<'p> {
    Plain(ArrayPlain<'p>),
    Comp(ArrayComp<'p>)
}

#[derive(Clone, Debug)]
pub struct Slice<'p> {
    pub expr: Box<Expr<'p>>,
    pub params: Vec<Option<Expr<'p>>>,
    /// Note: only covers the [<e> : <e> : <e>] segment
    pub span: TokenSpan,
}

#[derive(Copy, Clone, Debug)]
pub enum BinOp {
    Mul,
    Div,
    Mod,
    Plus,
    Minus,
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
    Null(TokenSpan),
    True(TokenSpan),
    False(TokenSpan),
    SelfT(TokenSpan),
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
        span: TokenSpan,
    },
    SuperIndex {
        index: Box<Expr<'p>>,
        span: TokenSpan,
    },
    InSuper {
        expr: Box<Expr<'p>>,
        /// Note: only covers the `in super` tokens
        span: TokenSpan,
    },
    FnCall {
        func: Box<Expr<'p>>,
        args: Args<'p>,
    },
    FnDef {
        params: Vec<Param<'p>>,
        body: Box<Expr<'p>>,
        span: TokenSpan,
    },
    Import {
        path: String<'p>,
        span: TokenSpan,
    },
    ImportStr {
        path: String<'p>,
        span: TokenSpan,
    },
    ImportBin {
        path: String<'p>,
        span: TokenSpan,
    },
    Error {
        message: Box<Expr<'p>>,
        span: TokenSpan,
    },
    Assert {
        assert: Assert<'p>,
        next: Box<Expr<'p>>,
    },
    Local {
        locals: Vec<Bind<'p>>,
        next: Box<Expr<'p>>,
    },
    If {
        cond: Box<Expr<'p>>,
        then: Box<Expr<'p>>,
        else_: Option<Box<Expr<'p>>>,
        span: TokenSpan,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expr<'p>>,
        start: usize,
    },
    BinaryOp {
        op: BinOp,
        lhs: Box<Expr<'p>>,
        rhs: Box<Expr<'p>>,
    },
}

impl<'p> Expr<'p> {
    pub(crate) fn binop(lhs: Expr<'p>, rhs: Expr<'p>, op: BinOp) -> Self {
        Self::BinaryOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub(crate) fn unop(expr: Expr<'p>, start: usize, op: UnaryOp) -> Self {
        Self::UnaryOp {
            op,
            start,
            expr: Box::new(expr),
        }
    }
}
