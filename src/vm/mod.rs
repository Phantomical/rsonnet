#![allow(dead_code, unused_imports)]

use std::collections::HashMap;

use gc_arena::{Gc, Mutation};
use miette::SourceSpan;
use serde_json as json;

use self::error::*;
use self::frame::*;
use self::value::*;
use crate::cell::{GcCell, MultiBorrow, Token, TokenCell};
use crate::mir::{Context, ExprData, ExprRef, Span};
use crate::parse::Visibility;

mod error;
mod frame;
mod value;

type GcValue<'gc> = GcCell<'gc, Value<'gc>>;
type GcScope<'gc> = Gc<'gc, Scope<'gc>>;

#[derive(Collect)]
#[collect(no_drop)]
pub struct Vm<'gc> {
    root: GcScope<'gc>,
    std: GcValue<'gc>,

    stack: Vec<GcValue<'gc>>,
    #[collect(require_static)]
    frames: Vec<Frame>,
}

pub(crate) struct VmCtx<'a, 'gc> {
    ctx: &'a Context,
    vm: &'a mut Vm<'gc>,
    mutation: &'a Mutation<'gc>,
}

impl<'a, 'gc> VmCtx<'a, 'gc> {
    fn base(&self) -> usize {
        self.vm.frames.last().map(|frame| frame.base).unwrap_or(0)
    }

    fn invalid_span(&self) -> Span {
        Span::new(usize::MAX, SourceSpan::new(usize::MAX.into(), 0.into()))
    }

    fn push(&mut self, value: GcValue<'gc>) {
        self.vm.stack.push(value);
    }
    fn pop(&mut self) -> GcValue<'gc> {
        self.vm.stack.pop().expect("stack was empty")
    }
    fn at(&self, offset: isize) -> GcValue<'gc> {
        let base = self.base();
        let index = match offset {
            offset if offset < 0 => {
                let index = self
                    .vm
                    .stack
                    .len()
                    .checked_add_signed(offset)
                    .unwrap_or_else(|| panic!("no stack entry at offset {}", offset));

                assert!(
                    index >= base,
                    "stack entry at offset {offset} is outside of the current frame"
                );
                index
            }
            offset => base + offset as usize,
        };

        self.vm
            .stack
            .get(index)
            .copied()
            .unwrap_or_else(|| panic!("no stack entry at offset {}", offset))
    }

    fn push_frame(&mut self, ops: &'static [Op]) {
        self.vm.frames.push(Frame {
            ops,
            ip: 0,
            base: self.vm.stack.len(),
        });
    }

    fn push_string(&mut self, span: Span, string: String) {
        self.push(self.value(span, self.vm.root, ValueData::String(string)));
    }
    fn push_number(&mut self, span: Span, number: f64) {
        self.push(self.value(span, self.vm.root, ValueData::Number(number)))
    }
    fn push_thunk(&mut self, span: Span, scope: GcScope<'gc>, thunk: ExprRef) {
        self.push(self.value(span, scope, ValueData::Thunk(thunk)))
    }

    fn value(&self, span: Span, scope: GcScope<'gc>, data: ValueData<'gc>) -> GcValue<'gc> {
        GcValue::new(self.mutation, TokenCell::new(Value { span, scope, data }))
    }
    fn scope(&self, scope: Scope<'gc>) -> GcScope<'gc> {
        GcScope::new(self.mutation, scope)
    }

    fn op_copy(&mut self, offset: isize) -> VmResult<'gc> {
        self.push(self.at(offset));
        Ok(())
    }
    fn op_swap(&mut self, a: isize, b: isize) -> VmResult<'gc> {
        let base = self.base();
        let a_idx = base
            .checked_add_signed(a)
            .unwrap_or_else(|| panic!("no stack entry at offset {}", a));
        let b_idx = base
            .checked_add_signed(b)
            .unwrap_or_else(|| panic!("no stack entry at offset {}", b));

        self.vm.stack.swap(a_idx, b_idx);

        Ok(())
    }
    fn op_drop(&mut self) -> VmResult<'gc> {
        let _ = self.pop();
        Ok(())
    }
    fn op_assign(&mut self, token: &mut Token, offset: isize) -> VmResult<'gc> {
        let value = self.pop();
        let target = self.at(offset);

        if Gc::ptr_eq(value, target) {
            return Ok(());
        }

        let mut multi = MultiBorrow::new(token);
        let target = multi.borrow_mut(&target);
        let value = multi.borrow(&value);

        target.data = value.data.clone();
        Ok(())
    }

    fn op_typeof(&mut self, token: &mut Token) -> VmResult<'gc> {
        let value = self.pop();
        let value = value.borrow(token);

        self.push(self.value(
            value.span,
            value.scope,
            ValueData::String(value.ty().to_string()),
        ));

        Ok(())
    }
    fn op_frame_size_eq(&mut self, size: usize) -> VmResult<'gc> {
        let base = self.base();
        let iseq = size == (self.vm.stack.len() - base);

        let data = match iseq {
            true => ValueData::True,
            false => ValueData::False,
        };

        self.push(self.value(self.invalid_span(), self.vm.root, data));
        Ok(())
    }

    fn op_array_length(&mut self, token: &mut Token) -> VmResult<'gc> {
        let value = self.pop();
        let value = value.borrow(token);

        match &value.data {
            ValueData::Array(elems) => {
                self.push(self.value(
                    value.span,
                    value.scope,
                    ValueData::Number(elems.len() as f64),
                ));

                Ok(())
            }
            _ => Err(VmError::from_value(
                value,
                format_args!("cannot get length of an object of type {}", value.ty()),
            )),
        }
    }
    fn op_array_append(&mut self, token: &mut Token, offset: isize) -> VmResult<'gc> {
        let value = self.pop();
        let array = self.at(offset);
        let array = array.borrow_mut(token);

        match &mut array.data {
            ValueData::Array(elems) => {
                elems.push(value);
                Ok(())
            }
            _ => Err(VmError::from_value(
                array,
                format_args!("cannot append to an object of type {}", array.ty()),
            )),
        }
    }

    fn op_object_insert(&mut self, token: &mut Token, offset: isize) -> VmResult<'gc> {
        let object = self.at(offset);
        let value = self.pop();
        let vis = self.pop();
        let name = self.pop();

        let mut multi = MultiBorrow::new(token);
        let name = multi.borrow(&name);
        let vis = multi.borrow(&vis);

        let name = match &name.data {
            ValueData::String(name) => name.clone(),
            _ => {
                return Err(VmError::from_value(
                    &name,
                    format_args!(
                        "invalid type for an object field, expected string got {}",
                        name.ty()
                    ),
                ))
            }
        };

        let vis = match &vis.data {
            ValueData::String(vis) if vis == ":" => Visibility::Visible,
            ValueData::String(vis) if vis == "::" => Visibility::Hidden,
            ValueData::String(vis) if vis == ":::" => Visibility::ForceVisible,
            ValueData::String(val) => {
                return Err(VmError::from_value(
                    vis,
                    format_args!("unknown visibility `{val}`, expected one of `:`, `::`, or `:::`"),
                ))
            }
            _ => {
                return Err(VmError::from_value(
                    vis,
                    format_args!(
                        "visibility was of wrong type, expected string but got {}",
                        vis.ty()
                    ),
                ))
            }
        };

        let object = multi.borrow_mut(&object);
        let object = match &mut object.data {
            ValueData::Object(object) => object,
            _ => {
                return Err(VmError::from_value(
                    object,
                    format_args!("attempted to insert into a value of type {}", object.ty()),
                ))
            }
        };

        object.insert(name, Field { vis, value });

        Ok(())
    }

    fn op_eval(&mut self, token: &mut Token) -> VmResult<'gc> {
        let gc_value = self.pop();

        loop {
            let value = gc_value.borrow_mut(token);

            let (scope, expr) = match &value.data {
                ValueData::Thunk(expr) => (value.scope, *expr),
                _ => {
                    self.push(gc_value);
                    return Ok(());
                }
            };

            let expr = self.ctx.expr(expr);
            value.span = expr.span;

            match &expr.data {
                ExprData::Std => {
                    self.push(self.vm.std);
                    return Ok(());
                }
                ExprData::Null => value.data = ValueData::Null,
                ExprData::True => value.data = ValueData::True,
                ExprData::False => value.data = ValueData::False,
                ExprData::This => match scope.self_ {
                    Some(this) => {
                        self.push(this);
                        return Ok(());
                    }
                    None => {
                        return Err(VmError::new(
                            expr.span,
                            scope,
                            "attempted to use self outside of an object",
                        ))
                    }
                },
                ExprData::Super => match scope.super_ {
                    Some(s) => {
                        self.push(s);
                        return Ok(());
                    }
                    None => {
                        return Err(VmError::new(
                            expr.span,
                            scope,
                            "attempted to use super when there is no super object",
                        ))
                    }
                },
                ExprData::Ident(name) => match scope.value_of(name) {
                    Some(value) => {
                        self.push(value);
                        return Ok(());
                    }
                    None => {
                        return Err(VmError::new(
                            expr.span,
                            scope,
                            format_args!("no local named `{name}` in the current scope"),
                        ))
                    }
                },
                ExprData::String(text) => value.data = ValueData::String(text.to_string()),
                ExprData::Number(number) => value.data = ValueData::Number(*number),
                ExprData::Error(message) => {
                    self.push_frame(program! {
                        Manifest;
                        ToString;
                        Error;
                    });
                    self.push(self.value(expr.span, scope, ValueData::Thunk(*message)));

                    return Ok(());
                }
                ExprData::UpScope(inner) => {
                    let (self_, super_) = scope
                        .parent
                        .map(|scope| (scope.self_, scope.super_))
                        .unzip();

                    value.scope = self.scope(Scope {
                        parent: Some(scope),
                        span: expr.span,
                        vars: HashMap::new(),
                        self_: self_.flatten(),
                        super_: super_.flatten(),
                    });
                    value.data = ValueData::Thunk(*inner);
                }
                ExprData::Index {
                    expr: object,
                    index,
                } => {
                    self.push_frame(program! {
                        // stack: <value> <index> <object>
                        Eval;       // eval(<object>)
                        Swap 1, 2;  // stack: <value> <object> <index>
                        Eval;       // eval(<index>)
                        Index 1;    // push(<object>[<index>])
                        Assign 0;   // *value = pop()
                        Return;
                    });
                    self.push(gc_value);
                    self.push_thunk(expr.span, scope, *object);
                    self.push_thunk(expr.span, scope, *index);

                    return Ok(());
                }
                ExprData::Locals { locals, expr: next } => {
                    let next = *next;
                    let mut newscope = Scope {
                        parent: Some(scope),
                        span: expr.span,
                        vars: HashMap::new(),
                        self_: scope.self_,
                        super_: scope.super_,
                    };

                    for local in locals {
                        newscope.vars.insert(
                            local.name.item.clone(),
                            self.value(local.span, scope, ValueData::Thunk(local.value)),
                        );
                    }

                    let newscope = self.scope(newscope);
                    value.scope = newscope;
                    value.data = ValueData::Thunk(next);

                    for (_, var) in &newscope.vars {
                        var.borrow_mut(token).scope = newscope;
                    }
                }
                ExprData::Object { effects, fields } => {
                    let object = gc_value;
                    let scope = self.scope(Scope {
                        span: expr.span,
                        parent: Some(scope),
                        vars: HashMap::new(),
                        self_: Some(object),
                        super_: scope.self_,
                    });

                    value.scope = scope;

                    self.push_frame(program! {
                        // stack: (<effect>)* <object>
                        Jump cond;

                    start:
                        Swap -1, -2;    // stack: (<effect>)* <object> <effect>
                        Eval;           // eval(<effect>) (for side effects)
                        Drop;

                    cond:
                        FrameSizeEq 1;  // if stack == [<object>]
                        BranchF start;  // else goto start
                        Return;
                    });
                    for effect in effects {
                        self.push_thunk(expr.span, scope, *effect);
                    }

                    self.push_frame(program! {
                        // stack: <object> (<field> <vis> <name>)*
                        Jump cond;

                    start:
                        Eval;           // eval(<name>)
                        Swap -1, -3;    // stack: ... <name> <vis> <field>
                        ObjectInsert 0; // insert(<object>, <name>, <vis>, <field>)

                    cond:
                        FrameSizeEq 1;  // if stack == [<object>]
                        BranchF start;  // then goto start
                        Return;
                    });
                    self.push(object);
                    for field in fields {
                        let vis = match field.vis {
                            Visibility::Visible => ":",
                            Visibility::Hidden => "::",
                            Visibility::ForceVisible => ":::",
                        };

                        self.push_thunk(field.span, scope, field.value);
                        self.push_string(field.span, vis.to_string());
                        self.push_thunk(field.span, scope, field.field);
                    }

                    return Ok(());
                }
                ExprData::Array { elems } => {
                    let scope = self.scope(Scope {
                        span: expr.span,
                        parent: Some(scope),
                        vars: HashMap::default(),
                        self_: None,
                        super_: None,
                    });

                    value.scope = scope;
                    value.data = ValueData::Array(
                        elems
                            .iter()
                            .map(|elem| self.value(expr.span, scope, ValueData::Thunk(*elem)))
                            .collect(),
                    );
                }
                ExprData::Function { params, body } => {
                    let params = params
                        .iter()
                        .map(|param| Param {
                            name: param.name.item.clone(),
                            default: self.value(param.span, scope, ValueData::Thunk(param.value)),
                        })
                        .collect();

                    value.data = ValueData::Function {
                        params,
                        body: *body,
                        scope,
                    };
                }
                ExprData::If { cond, then, else_ } => {
                    self.push_frame(program! {
                        // stack: <then> <else> <cond>

                        Eval;           // eval(<cond>)
                        BranchF case;   // if !<cond> goto case
                        Swap 0, 1;      // stack: <else> <then>

                    case:
                        Drop;
                        Eval;
                    });

                    self.push_thunk(expr.span, scope, *then);
                    self.push_thunk(expr.span, scope, *else_);
                    self.push_thunk(expr.span, scope, *cond);

                    return Ok(());
                }
                ExprData::Call {
                    func,
                    positional,
                    named,
                } => {
                    self.push_frame(program! {
                        // stack: <named> <pos> <func>
                        Eval;       // eval(<func>)
                        Swap 0, 2;  // stack: <func> <pos> <named>
                        Call;       // <func>(<pos> <named>)
                    });

                    let named = named
                        .iter()
                        .map(|param| {
                            (
                                param.name.item.clone(),
                                Field {
                                    vis: Visibility::Visible,
                                    value: self.value(
                                        param.span,
                                        scope,
                                        ValueData::Thunk(param.value),
                                    ),
                                },
                            )
                        })
                        .collect();
                    let named = self.value(expr.span, scope, ValueData::Object(named));

                    let positional = positional
                        .iter()
                        .map(|&param| self.value(expr.span, scope, ValueData::Thunk(param)))
                        .collect();
                    let positional = self.value(expr.span, scope, ValueData::Array(positional));

                    self.push(named);
                    self.push(positional);
                    self.push_thunk(expr.span, scope, *func);

                    return Ok(());
                }

                &ExprData::Not(op) => {
                    self.push_frame(program! {
                        // stack: <thunk> <op>
                        Eval;
                        Builtin Builtin(|ctx, token| ctx.op_not(token));
                        Assign 0;
                    });

                    self.push(gc_value);
                    self.push_thunk(expr.span, scope, op);
                    return Ok(());
                }
                &ExprData::Neg(op) => {
                    self.push_frame(program! {
                        // stack: <thunk> <op>
                        Eval;
                        Builtin Builtin(|ctx, token| ctx.op_neg(token));
                        Assign 0;
                    });

                    self.push(gc_value);
                    self.push_thunk(expr.span, scope, op);
                    return Ok(());
                }
                &ExprData::Pos(op) => {
                    self.push_frame(program! {
                        // stack: <thunk> <op>
                        Eval;
                        Builtin Builtin(|ctx, token| ctx.op_pos(token));
                        Assign 0;
                    });

                    self.push(gc_value);
                    self.push_thunk(expr.span, scope, op);
                    return Ok(());
                }
                &ExprData::BitNot(op) => {
                    self.push_frame(program! {
                        // stack: <thunk> <op>
                        Eval;
                        Builtin Builtin(|ctx, token| ctx.op_bitnot(token));
                        Assign 0;
                    });

                    self.push(gc_value);
                    self.push_thunk(expr.span, scope, op);
                    return Ok(());
                }

                _ => todo!(),
            };
        }
    }

    fn op_manifest(&mut self, token: &mut Token) -> VmResult<'gc> {
        let gcvalue = self.pop();
        let value = gcvalue.borrow(token);

        match &value.data {
            ValueData::Null
            | ValueData::True
            | ValueData::False
            | ValueData::String(_)
            | ValueData::Number(_) => self.push(gcvalue),
            ValueData::Object(fields) => {
                todo!()
            }
            ValueData::Array(array) => todo!(),
            ValueData::Function { .. } => {
                return Err(VmError::from_value(
                    value,
                    "attempted to manifest a function",
                ))
            }
            ValueData::Thunk(_) => {
                return Err(VmError::from_value(value, "attempted to manifest a thunk"))
            }
        }

        Ok(())
    }

    fn op_not(&mut self, token: &mut Token) -> VmResult<'gc> {
        let op = self.pop();
        let op = op.borrow(token);
        let data = match &op.data {
            ValueData::True => ValueData::False,
            ValueData::False => ValueData::True,
            _ => {
                return Err(VmError::from_value(
                    op,
                    format_args!("unary operator ! does not operate on type {}", op.ty()),
                ))
            }
        };

        self.push(self.value(op.span, op.scope, data));
        Ok(())
    }
    fn op_neg(&mut self, token: &mut Token) -> VmResult<'gc> {
        let op = self.pop();
        let op = op.borrow(token);
        let data = match &op.data {
            &ValueData::Number(number) => ValueData::Number(-number),
            _ => {
                return Err(VmError::from_value(
                    op,
                    format_args!("unary operator - does not operate on type {}", op.ty()),
                ))
            }
        };

        self.push(self.value(op.span, op.scope, data));
        Ok(())
    }
    fn op_pos(&mut self, token: &mut Token) -> VmResult<'gc> {
        let op = self.pop();
        let op = op.borrow(token);
        let data = match &op.data {
            &ValueData::Number(number) => ValueData::Number(number),
            _ => {
                return Err(VmError::from_value(
                    op,
                    format_args!("unary operator + does not operate on type {}", op.ty()),
                ))
            }
        };

        self.push(self.value(op.span, op.scope, data));
        Ok(())
    }
    fn op_bitnot(&mut self, token: &mut Token) -> VmResult<'gc> {
        let op = self.pop();
        let op = op.borrow(token);
        let data = match &op.data {
            &ValueData::Number(number) => ValueData::Number(!(number as i64) as f64),
            _ => {
                return Err(VmError::from_value(
                    op,
                    format_args!("unary operator ~ does not operate on type {}", op.ty()),
                ))
            }
        };

        self.push(self.value(op.span, op.scope, data));
        Ok(())
    }

    fn op_binop_f64<F>(&mut self, token: &mut Token, op: &'static str, func: F) -> VmResult<'gc>
    where
        F: FnOnce(f64, f64) -> Result<f64, &'static str>,
    {
        let lhs = self.pop();
        let rhs = self.pop();

        let lhs_val = lhs.borrow(token);
        let rhs_val = rhs.borrow(token);

        let lhs = match &lhs_val.data {
            &ValueData::Number(lhs) => lhs,
            _ => {
                return Err(VmError::from_value(
                    lhs_val,
                    format_args!("operator {op} is not defined for type {}", lhs.ty()),
                ))
            }
        };

        let rhs = match &rhs_val.data {
            &ValueData::Number(rhs) => rhs,
            _ => {
                return Err(VmError::from_value(
                    rhs_val,
                    format_args!("operator {op} is not defined for type {}", rhs.ty()),
                ))
            }
        };

        let value = match func(lhs, rhs) {
            Ok(value) => value,
            Err(e) => return Err(VmError::new(lhs_val.span, lhs_val.scope, e)),
        };

        // self.push(self.value(lhs_val.span, lhs_val.))
        todo!()
    }
}
