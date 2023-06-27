#[macro_use]
extern crate gc_arena_derive;

pub mod lexer;
pub mod mir;
pub mod parse;
mod spanext;
pub mod vm;
pub mod cell;
