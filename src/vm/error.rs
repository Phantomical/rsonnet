use std::fmt::Display;

use super::value::Value;
use super::GcScope;
use crate::mir::Span;

pub(crate) type VmResult<'gc, T = ()> = Result<T, VmError<'gc>>;

pub(crate) struct VmError<'gc> {
    span: Span,
    scope: GcScope<'gc>,
    message: String,
}

impl<'gc> VmError<'gc> {
    pub fn new<D: Display>(span: Span, scope: GcScope<'gc>, message: D) -> Self {
        Self {
            span,
            scope,
            message: message.to_string(),
        }
    }

    pub fn from_value<D: Display>(value: &Value<'gc>, message: D) -> Self {
        Self::new(value.span, value.scope, message)
    }
}
