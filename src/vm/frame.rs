use super::error::VmResult;
use super::{GcScope, GcValue, VmCtx};
use crate::cell::Token;
use crate::mir::{ExprRef, Span};

use std::fmt;

#[derive(Clone, Copy, Debug)]
pub(crate) struct Frame {
    pub ops: &'static [Op],
    pub ip: usize,
    pub base: usize,
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum Op {
    Builtin(Builtin),

    /// Exit the current frame and return in the parent one.
    Return,

    /// Copy the stack entry at the provided index.
    Copy(isize),

    /// Swap the stack entries at the provided indices.
    Swap(isize, isize),

    /// Drop the value on the top of the stack.
    Drop,
    Assign(isize),

    PushNumber(f64),
    PushString(&'static str),

    Jump(usize),
    /// Jump if the value at the top of the stack is `true`
    BranchT(usize),
    /// Jump if the value at the top of the stack is `false`.
    BranchF(usize),
    FrameSizeEq(usize),

    TypeOf,
    ToString,
    Error,

    /// Opcode equivalent of std.primitiveEquals
    PrimitiveEq,
    /// Opcode equivalent of std.cmp
    Cmp,

    /// `call(func:fn, pos:array, named:object)`
    Call,

    Index(isize),

    ArrayLength,
    ArrayAppend(isize),

    ObjectInsert(isize),

    Eval,
    Manifest,
}

type BuiltinFn = for<'gc> fn (&mut VmCtx<'_, 'gc>, &mut Token) -> VmResult<'gc>;

#[derive(Copy, Clone)]
pub(crate) struct Builtin(pub BuiltinFn);

impl fmt::Debug for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Builtin(..)")
    }
}

macro_rules! program_impl {
    {
        impl(
            pos = $pos:expr,
            labels = [$( $label:ident = $offset:expr ),* $(,)?],
            instrs = [$( $instr:expr ),*]
        )
    } => {{
        $(
            #[allow(non_upper_case_globals)]
            const $label: usize = $offset;
        )*

        const __OP_PROGRAM: &'static [$crate::vm::frame::Op] = &[$($instr),*];
        __OP_PROGRAM
    }};
    {
        impl(
            pos = $pos:expr,
            labels = [$( $label:ident = $offset:expr ),* $(,)?],
            instrs = [$( $instr:expr ),*]
        )

        $nlabel:ident :
        $( $rest:tt )*
    } => {
        $crate::vm::frame::program_impl!{
            impl(
                pos = $pos,
                labels = [$($label = $offset,)* $nlabel = $pos],
                instrs = [$($instr),*]
            )

            $( $rest )*
        }
    };
    (
        impl(
            pos = $pos:expr,
            labels = [$( $label:ident = $offset:expr ),* $(,)?],
            instrs = [$( $instr:expr ),*]
        )

        $opcode:ident $( $( $args:expr ),+ )?;
        $( $rest:tt )*
    ) => {
        $crate::vm::frame::program_impl!{
            impl(
                pos = $pos + 1,
                labels = [$($label = $offset,)*],
                instrs = [
                    $($instr,)*
                    $crate::vm::frame::Op::$opcode $( ( $( $args ),+ ) )?
                ]
            )

            $( $rest )*
        }
    }
}

macro_rules! program {
    {
        $($tt:tt)*
    } => {
        $crate::vm::frame::program_impl!{
            impl(
                pos = 0,
                labels = [],
                instrs = []
            )

            $($tt)*
        }
    };
}

pub(crate) use {program, program_impl};
