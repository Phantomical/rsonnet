use std::borrow::Cow;

use miette::SourceSpan;
use slab::Slab;

use crate::parse as ast;

mod irt;

pub use self::irt::*;

#[derive(Default)]
pub struct Context {
    exprs: Slab<Expr>,
}

impl Context {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn desugar(&mut self, expr: ast::Expr) -> ExprRef {
        let empty = SourceSpan::new(0.into(), 0.into());
        let std = self.mkexpr(empty, ExprData::Std);

        self.desugar_expr(
            ast::Expr::Local {
                locals: vec![Spanned::new(
                    ast::Bind::Var(ast::BindVar {
                        name: ast::Ident::new("std", empty),
                        expr: ast::Expr::Embed {
                            index: std,
                            span: empty,
                        },
                    }),
                    empty,
                )],
                next: Box::new(expr),
            },
            false,
        )
    }

    pub fn expr(&self, index: ExprRef) -> &Expr {
        &self.exprs[index.0]
    }
}

impl Context {
    fn mkexpr(&mut self, span: SourceSpan, data: ExprData) -> ExprRef {
        ExprRef(self.exprs.insert(Expr { span, data }))
    }

    fn desugar_expr(&mut self, expr: ast::Expr, in_obj: bool) -> ExprRef {
        let expr_span = expr.span();

        match expr {
            ast::Expr::Embed { index, .. } => index,
            ast::Expr::Null(span) => self.mkexpr(span, ExprData::Null),
            ast::Expr::True(span) => self.mkexpr(span, ExprData::True),
            ast::Expr::False(span) => self.mkexpr(span, ExprData::False),
            ast::Expr::SelfT(span) => self.mkexpr(span, ExprData::ObjSelf),
            ast::Expr::String(string) => {
                self.mkexpr(string.span(), ExprData::String(string.into_value().into()))
            }
            ast::Expr::Number(number) => match number.text().parse() {
                Ok(value) => self.mkexpr(number.span(), ExprData::Number(value)),
                Err(e) => self.desugar_expr(
                    ast::Expr::error(format!("invalid string literal: {e}"), number.span()),
                    in_obj,
                ),
            },
            ast::Expr::Variable(ident) => self.mkexpr(
                ident.span(),
                ExprData::Ident(ident.text().to_owned().into()),
            ),
            ast::Expr::Object(ast::Object::Plain(object)) => {
                let mut locals = Vec::new();
                let mut effects = Vec::new();
                let mut fields = Vec::new();

                for member in object.members {
                    match member {
                        ast::Member::Assert(assert) => effects.push(self.desugar_assert(assert)),
                        ast::Member::Local(local) => {
                            locals.push(self.desugar_bind(local.item, true))
                        }
                        ast::Member::Field(field) => fields.push(self.desugar_field(field, true)),
                    }
                }

                if !in_obj {
                    let span = object.span;

                    locals.push(Param {
                        name: Spanned::new("$".to_owned(), span),
                        value: self.mkexpr(span, ExprData::ObjSelf),
                        span,
                    });
                }

                let data = ExprData::Locals {
                    locals,
                    expr: self.mkexpr(object.span, ExprData::Object { effects, fields }),
                };
                self.mkexpr(object.span, data)
            }
            ast::Expr::Object(ast::Object::Comp(object)) => {
                let mut index = 0u32;
                let mut locals = Vec::new();
                let mut vars = Vec::new();
                let arr = self.mkexpr(object.span, ExprData::Ident("$arr".into()));

                for spec in &object.compspec {
                    if let ast::CompSpec::For(spec) = spec {
                        let idx = self.mkexpr(spec.var.span(), ExprData::Number(index as _));
                        let elem = self.mkexpr(
                            spec.var.span(),
                            ExprData::Index {
                                expr: arr,
                                index: idx,
                            },
                        );

                        vars.push(spec.var.clone());
                        locals.push(Param {
                            name: Spanned::new(spec.var.text().to_owned(), spec.var.span()),
                            value: elem,
                            span: spec.var.span(),
                        });

                        index += 1;
                    }
                }

                let field_span = object.field.span();
                let field = self.desugar_expr(*object.field, in_obj);
                let field = self.mkexpr(
                    field_span,
                    ExprData::Locals {
                        locals: locals.clone(),
                        expr: field,
                    },
                );

                let value_span = object.value.span();
                let value = self.desugar_expr(
                    ast::Expr::Local {
                        locals: object.locals,
                        next: object.value,
                    },
                    true,
                );
                let value = self.mkexpr(
                    value_span,
                    ExprData::Locals {
                        locals,
                        expr: value,
                    },
                );

                let seq = self.desugar_arrcomp(
                    RawArrayComp {
                        expr: Box::new(ast::Expr::array(
                            vars.into_iter().map(ast::Expr::Variable).collect(),
                            object.span,
                        )),
                        spec: object.compspec,
                        span: object.span,
                    },
                    in_obj,
                );

                self.mkexpr(
                    object.span,
                    ExprData::ObjectComp {
                        field,
                        value,
                        var: "$arr".into(),
                        seq,
                    },
                )
            }
            ast::Expr::Array(ast::Array::Plain(array)) => {
                let elems = array
                    .values
                    .into_iter()
                    .map(|value| self.desugar_expr(value, in_obj))
                    .collect();

                self.mkexpr(array.span, ExprData::Array { elems })
            }
            ast::Expr::Array(ast::Array::Comp(array)) => self.desugar_arrcomp(
                RawArrayComp {
                    expr: array.expr,
                    spec: array.compspec,
                    span: array.span,
                },
                in_obj,
            ),
            ast::Expr::Local { locals, next } => {
                let next = self.desugar_expr(*next, in_obj);
                let locals = locals
                    .into_iter()
                    .map(|local| self.desugar_bind(local.item, in_obj))
                    .collect();

                self.mkexpr(expr_span, ExprData::Locals { locals, expr: next })
            }
            ast::Expr::Concat { expr, object } => self.desugar_expr(
                ast::Expr::BinaryOp {
                    lhs: expr,
                    rhs: object,
                    op: ast::BinOp::Add,
                    span: expr_span,
                },
                in_obj,
            ),
            ast::Expr::FnDef { params, body, span } => {
                let params = params
                    .into_iter()
                    .map(|param| self.desugar_param(param, in_obj))
                    .collect();
                let body = self.desugar_expr(*body, in_obj);

                self.mkexpr(span, ExprData::Function { params, body })
            }
            ast::Expr::Assert { assert, next } => self.desugar_expr(
                ast::Expr::If {
                    cond: assert.cond,
                    then: next,
                    else_: Some(Box::new(ast::Expr::Error {
                        message: match assert.message {
                            Some(message) => message,
                            None => Box::new(ast::Expr::String(ast::String::new(
                                "",
                                "Assertion failed".to_owned(),
                                assert.span,
                            ))),
                        },
                        span: assert.span,
                    })),
                    span: assert.span,
                },
                in_obj,
            ),
            ast::Expr::Slice { expr, mut params } => {
                if params.item.len() == 1 {
                    let expr = self.desugar_expr(*expr, in_obj);
                    let index = self.desugar_expr(
                        params
                            .item
                            .pop()
                            .unwrap()
                            .map(|param| param)
                            .unwrap_or(ast::Expr::Null(params.span)),
                        in_obj,
                    );

                    self.mkexpr(expr_span, ExprData::Index { expr, index })
                } else {
                    let params_span = params.span;
                    let mut args = vec![*expr];
                    args.extend(params.item.into_iter().take(3).map(|param| match param {
                        Some(param) => param,
                        None => ast::Expr::Null(params_span),
                    }));

                    while args.len() < 4 {
                        args.push(ast::Expr::Null(params_span));
                    }

                    self.desugar_expr(
                        ast::Expr::call_path(&["std", "slice"], args, expr_span),
                        in_obj,
                    )
                }
            }
            ast::Expr::If {
                cond,
                then,
                else_,
                span,
            } => {
                let cond = self.desugar_expr(*cond, in_obj);
                let then = self.desugar_expr(*then, in_obj);
                let else_ = self.desugar_expr(
                    else_ //
                        .map(|e| *e)
                        .unwrap_or(ast::Expr::Null(span)),
                    in_obj,
                );

                self.mkexpr(span, ExprData::If { cond, then, else_ })
            }
            ast::Expr::Access { source, field } => {
                let expr = self.desugar_expr(*source, in_obj);
                let field = self.mkexpr(
                    field.span(),
                    ExprData::String(field.text().to_owned().into()),
                );

                self.mkexpr(expr_span, ExprData::Index { expr, index: field })
            }
            ast::Expr::SuperField { field, span } => self.desugar_expr(
                ast::Expr::SuperIndex {
                    index: Box::new(ast::Expr::String(ast::String::new(
                        field.text(),
                        field.text().to_owned(),
                        field.span(),
                    ))),
                    span,
                },
                in_obj,
            ),
            ast::Expr::SuperIndex { index, span } => {
                let super_ = self.mkexpr(span, ExprData::Super);
                let index = self.desugar_expr(*index, in_obj);

                self.mkexpr(
                    span,
                    ExprData::Index {
                        expr: super_,
                        index,
                    },
                )
            }
            ast::Expr::InSuper { expr, span } => {
                let super_ = self.mkexpr(span, ExprData::Super);

                self.desugar_expr(
                    ast::Expr::binop(
                        *expr,
                        ast::Expr::Embed {
                            index: super_,
                            span,
                        },
                        ast::BinOp::In,
                    ),
                    in_obj,
                )
            }
            ast::Expr::BinaryOp { op, lhs, rhs, span } => {
                let lhs_span = lhs.span();
                let rhs_span = rhs.span();

                let lhs = self.desugar_expr(*lhs, in_obj);
                let rhs = self.desugar_expr(*rhs, in_obj);

                match op {
                    ast::BinOp::Mul => self.mkexpr(span, ExprData::Mul(lhs, rhs)),
                    ast::BinOp::Div => self.mkexpr(span, ExprData::Div(lhs, rhs)),
                    ast::BinOp::Add => self.mkexpr(span, ExprData::Add(lhs, rhs)),
                    ast::BinOp::Sub => self.mkexpr(span, ExprData::Sub(lhs, rhs)),
                    ast::BinOp::Shl => self.mkexpr(span, ExprData::Shl(lhs, rhs)),
                    ast::BinOp::Shr => self.mkexpr(span, ExprData::Shr(lhs, rhs)),
                    ast::BinOp::Lt => self.mkexpr(span, ExprData::Lt(lhs, rhs)),
                    ast::BinOp::Le => self.mkexpr(span, ExprData::Le(lhs, rhs)),
                    ast::BinOp::Gt => self.mkexpr(span, ExprData::Gt(lhs, rhs)),
                    ast::BinOp::Ge => self.mkexpr(span, ExprData::Ge(lhs, rhs)),
                    ast::BinOp::And => self.mkexpr(span, ExprData::And(lhs, rhs)),
                    ast::BinOp::Xor => self.mkexpr(span, ExprData::Xor(lhs, rhs)),
                    ast::BinOp::Or => self.mkexpr(span, ExprData::Or(lhs, rhs)),
                    ast::BinOp::LAnd => self.mkexpr(span, ExprData::LAnd(lhs, rhs)),
                    ast::BinOp::LOr => self.mkexpr(span, ExprData::LOr(lhs, rhs)),

                    ast::BinOp::Mod => self.desugar_expr(
                        ast::Expr::call_path(
                            &["std", "mod"],
                            vec![
                                ast::Expr::Embed {
                                    index: lhs,
                                    span: lhs_span,
                                },
                                ast::Expr::Embed {
                                    index: rhs,
                                    span: rhs_span,
                                },
                            ],
                            span,
                        ),
                        in_obj,
                    ),

                    ast::BinOp::Ne => self.desugar_expr(
                        ast::Expr::unop(
                            ast::Expr::binop(
                                ast::Expr::Embed {
                                    index: lhs,
                                    span: lhs_span,
                                },
                                ast::Expr::Embed {
                                    index: rhs,
                                    span: rhs_span,
                                },
                                ast::BinOp::Eq,
                            ),
                            span,
                            ast::UnaryOp::Not,
                        ),
                        in_obj,
                    ),
                    ast::BinOp::Eq => self.desugar_expr(
                        ast::Expr::call_path(
                            &["std", "equals"],
                            vec![
                                ast::Expr::Embed {
                                    index: lhs,
                                    span: lhs_span,
                                },
                                ast::Expr::Embed {
                                    index: rhs,
                                    span: rhs_span,
                                },
                            ],
                            span,
                        ),
                        in_obj,
                    ),
                    ast::BinOp::In => self.desugar_expr(
                        ast::Expr::call_path(
                            &["std", "objectHasEx"],
                            vec![
                                ast::Expr::Embed {
                                    index: lhs,
                                    span: lhs_span,
                                },
                                ast::Expr::Embed {
                                    index: rhs,
                                    span: rhs_span,
                                },
                                ast::Expr::True(span),
                            ],
                            span,
                        ),
                        in_obj,
                    ),
                }
            }
            ast::Expr::UnaryOp { op, expr, span } => {
                let expr = self.desugar_expr(*expr, in_obj);

                match op {
                    ast::UnaryOp::Not => self.mkexpr(span, ExprData::Not(expr)),
                    ast::UnaryOp::Neg => self.mkexpr(span, ExprData::Neg(expr)),
                    ast::UnaryOp::Pos => self.mkexpr(span, ExprData::Pos(expr)),
                    ast::UnaryOp::Tilde => self.mkexpr(span, ExprData::Tilde(expr)),
                }
            }
            ast::Expr::FnCall { func, args } => {
                let func = self.desugar_expr(*func, in_obj);
                let positional = args
                    .positional
                    .into_iter()
                    .map(|arg| self.desugar_expr(arg, in_obj))
                    .collect::<Vec<_>>();
                let named = args
                    .named
                    .into_iter()
                    .map(|arg| Param {
                        span: arg.span(),
                        name: Spanned::new(arg.name.text().to_owned(), arg.name.span()),
                        value: self.desugar_expr(arg.value, in_obj),
                    })
                    .collect();

                self.mkexpr(
                    expr_span,
                    ExprData::Call {
                        func,
                        positional,
                        named,
                    },
                )
            }
            ast::Expr::Import { path, span } => {
                self.mkexpr(span, ExprData::Import(path.into_value()))
            }
            ast::Expr::ImportStr { path, span } => {
                self.mkexpr(span, ExprData::ImportStr(path.into_value()))
            }
            ast::Expr::ImportBin { path, span } => {
                self.mkexpr(span, ExprData::ImportBin(path.into_value()))
            }
            ast::Expr::Error { message, span } => {
                let message = self.desugar_expr(*message, in_obj);
                self.mkexpr(span, ExprData::Error(message))
            }
        }
    }

    fn desugar_assert(&mut self, assert: ast::Assert) -> ExprRef {
        let message = match assert.message {
            Some(message) => message,
            None => Box::new(ast::Expr::String(ast::String::new(
                "Assertion failed",
                "Assertion failed".to_owned(),
                assert.span,
            ))),
        };

        self.desugar_expr(
            ast::Expr::If {
                cond: assert.cond,
                then: Box::new(ast::Expr::Null(assert.span)),
                else_: Some(Box::new(ast::Expr::Error {
                    message,
                    span: assert.span,
                })),
                span: assert.span,
            },
            true,
        )
    }

    fn desugar_field(&mut self, field: ast::Field, in_obj: bool) -> NamedField {
        match field {
            ast::Field::Function {
                name,
                params,
                vis,
                body,
                span,
            } => self.desugar_field(
                ast::Field::Named {
                    name,
                    inherit: false,
                    vis,
                    value: Box::new(ast::Expr::FnDef { params, body, span }),
                    span,
                },
                in_obj,
            ),
            ast::Field::Named {
                name,
                inherit,
                vis,
                value,
                span,
            } => {
                let name = match name {
                    ast::FieldName::Expr(expr) => *expr,
                    ast::FieldName::Name(name) => {
                        ast::Expr::String(ast::String::new("", name.text().to_owned(), name.span()))
                    }
                };

                let name_span = name.span();
                let value_span = value.span();

                let mut field = self.desugar_expr(name, in_obj);
                let mut value = self.desugar_expr(*value, in_obj);

                if inherit {
                    field = self.mkexpr(name_span, ExprData::UpScope(field));
                    value = self.desugar_expr(
                        ast::Expr::If {
                            cond: Box::new(ast::Expr::InSuper {
                                expr: Box::new(ast::Expr::Embed {
                                    index: field,
                                    span: name_span,
                                }),
                                span: value_span,
                            }),
                            then: Box::new(ast::Expr::binop(
                                ast::Expr::SuperIndex {
                                    index: Box::new(ast::Expr::Embed {
                                        index: field,
                                        span: name_span,
                                    }),
                                    span: value_span,
                                },
                                ast::Expr::Embed {
                                    index: value,
                                    span: value_span,
                                },
                                ast::BinOp::Add,
                            )),
                            else_: Some(Box::new(ast::Expr::Embed {
                                index: value,
                                span: value_span,
                            })),
                            span,
                        },
                        in_obj,
                    );
                }

                NamedField {
                    field,
                    vis,
                    value,
                    span,
                }
            }
        }
    }

    fn desugar_bind(&mut self, bind: ast::Bind, in_obj: bool) -> Param {
        let span = bind.span();

        match bind {
            ast::Bind::Var(ast::BindVar { name, expr }) => Param {
                name: Spanned::new(name.text().to_owned(), name.span()),
                value: self.desugar_expr(expr, in_obj),
                span,
            },
            ast::Bind::Fn(ast::BindFn { name, params, expr }) => Param {
                name: Spanned::new(name.text().to_owned(), name.span()),
                value: self.desugar_expr(
                    ast::Expr::FnDef {
                        params,
                        body: Box::new(expr),
                        span,
                    },
                    in_obj,
                ),
                span,
            },
        }
    }

    fn desugar_param(&mut self, param: ast::Param, in_obj: bool) -> Param {
        let span = param.span();
        let name = param.name.text().to_owned();

        match param.default {
            Some(default) => Param {
                span,
                name: Spanned::new(name, param.name.span()),
                value: self.desugar_expr(default, in_obj),
            },
            None => {
                let message = self.mkexpr(
                    span,
                    ExprData::String(Cow::Owned(format!("Parameter `{name}` not bound"))),
                );
                let error = self.mkexpr(span, ExprData::Error(message));

                Param {
                    name: Spanned::new(name, span),
                    value: error,
                    span,
                }
            }
        }
    }

    fn desugar_arrcomp(&mut self, mut comp: RawArrayComp, in_obj: bool) -> ExprRef {
        let Some(spec) = comp.spec.pop() else {
            return self.desugar_expr(
                ast::Expr::Array(ast::Array::Plain(ast::ArrayPlain {
                    span: comp.expr.span(),
                    values: vec![*comp.expr],
                })),
                in_obj
            );
        };

        match spec {
            ast::CompSpec::For(spec) => {
                let src_span = spec.expr.span();
                let var_span = spec.var.span();
                let span = spec.span;

                // function($i)
                //     local <var> = $arr[$i];
                //     <desugar(comp)>
                let inner = ast::Expr::FnDef {
                    params: vec![ast::Param {
                        name: ast::Ident::new("$i", var_span),
                        default: None,
                    }],
                    body: Box::new(ast::Expr::Local {
                        locals: vec![Spanned::new(
                            ast::Bind::Var(ast::BindVar {
                                name: spec.var,
                                expr: ast::Expr::index(
                                    ast::Expr::id("$arr", src_span),
                                    ast::Expr::id("$i", var_span),
                                ),
                            }),
                            var_span,
                        )],
                        next: Box::new(ast::Expr::Embed {
                            index: self.desugar_arrcomp(comp, in_obj),
                            span,
                        }),
                    }),
                    span,
                };
                // std.join([], std.makeArray(std.length($arr), <inner>))
                let join = ast::Expr::call_path(
                    &["std", "join"],
                    vec![
                        ast::Expr::array(vec![], span),
                        ast::Expr::call_path(
                            &["std", "makeArray"],
                            vec![
                                ast::Expr::call_path(
                                    &["std", "length"],
                                    vec![ast::Expr::id("$arr", src_span)],
                                    span,
                                ),
                                inner,
                            ],
                            span,
                        ),
                    ],
                    span,
                );

                return self.desugar_expr(
                    ast::Expr::Local {
                        locals: vec![Spanned::new(
                            ast::Bind::Var(ast::BindVar {
                                name: ast::Ident::new("$arr", src_span),
                                expr: *spec.expr,
                            }),
                            src_span,
                        )],
                        next: Box::new(join),
                    },
                    in_obj,
                );
            }
            ast::CompSpec::If(spec) => {
                let expr = ast::Expr::If {
                    span: comp.span,
                    cond: spec.cond,
                    else_: Some(Box::new(ast::Expr::Array(ast::Array::Plain(
                        ast::ArrayPlain {
                            span: comp.span,
                            values: vec![],
                        },
                    )))),
                    then: Box::new(ast::Expr::Embed {
                        span: comp.span,
                        index: self.desugar_arrcomp(comp, in_obj),
                    }),
                };

                return self.desugar_expr(expr, in_obj);
            }
        }
    }
}

struct RawArrayComp<'p> {
    pub expr: Box<ast::Expr<'p>>,
    pub spec: Vec<ast::CompSpec<'p>>,
    pub span: SourceSpan,
}

impl<'p> From<ast::ArrayComp<'p>> for RawArrayComp<'p> {
    fn from(value: ast::ArrayComp<'p>) -> Self {
        let mut spec = value.compspec;
        spec.reverse();

        RawArrayComp {
            expr: value.expr,
            spec,
            span: value.span,
        }
    }
}
