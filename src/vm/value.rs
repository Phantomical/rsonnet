use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt;

use super::{GcScope, GcValue};
use crate::cell::{GcCell, Token};
use crate::mir::{ExprRef, Span};
use crate::parse::Visibility;

#[derive(Collect, Clone, Debug)]
#[collect(no_drop)]
pub(crate) struct Scope<'gc> {
    pub span: Span,
    pub parent: Option<GcScope<'gc>>,
    pub vars: HashMap<String, GcValue<'gc>>,
    pub self_: Option<GcValue<'gc>>,
    pub super_: Option<GcValue<'gc>>,
}

impl<'gc> Scope<'gc> {
    pub fn value_of(&self, var: &str) -> Option<GcValue<'gc>> {
        let mut scope = self;

        loop {
            if let Some(val) = self.vars.get(var) {
                return Some(*val);
            }

            scope = match &scope.parent {
                Some(scope) => &*scope,
                None => break,
            }
        }

        None
    }
}

#[derive(Collect, Clone, Debug)]
#[collect(no_drop)]
pub(crate) struct Value<'gc> {
    pub span: Span,
    pub scope: GcScope<'gc>,
    pub data: ValueData<'gc>,
}

impl<'gc> Value<'gc> {
    pub fn ty(&self) -> ValueType {
        match &self.data {
            ValueData::Null => ValueType::Null,
            ValueData::True | ValueData::False => ValueType::Boolean,
            ValueData::String(_) => ValueType::String,
            ValueData::Number(_) => ValueType::Number,
            ValueData::Object(_) => ValueType::Object,
            ValueData::Function { .. } => ValueType::Function,
            ValueData::Array(_) => ValueType::Array,
            ValueData::Thunk { .. } => ValueType::Thunk,
        }
    }
}

#[derive(Collect, Clone, Debug)]
#[collect(no_drop)]
pub(crate) enum ValueData<'gc> {
    Null,
    True,
    False,
    String(String),
    Number(f64),
    Object(HashMap<String, Field<'gc>>),
    Function {
        params: Vec<Param<'gc>>,
        body: ExprRef,
        scope: GcScope<'gc>,
    },
    Array(Vec<GcValue<'gc>>),
    Thunk(ExprRef),
}

#[derive(Collect, Clone, Debug)]
#[collect(no_drop)]
pub(crate) struct Field<'gc> {
    #[collect(require_static)]
    pub vis: Visibility,
    pub value: GcValue<'gc>,
}

#[derive(Collect, Clone, Debug)]
#[collect(no_drop)]
pub(crate) struct Param<'gc> {
    pub name: String,
    pub default: GcValue<'gc>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Collect)]
#[collect(require_static)]
pub enum ValueType {
    Null,
    Boolean,
    String,
    Number,
    Object,
    Array,
    Function,
    Thunk,
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let text = match self {
            Self::Null => "null",
            Self::String => "string",
            Self::Number => "number",
            Self::Object => "object",
            Self::Array => "array",
            Self::Thunk => "thunk",
            Self::Boolean => "boolean",
            Self::Function => "function",
        };

        f.write_str(text)
    }
}
