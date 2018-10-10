#![feature(core_intrinsics, dbg_macro, drain_filter, exclusive_range_pattern)]

pub mod gen_x86;
pub mod gen_ir;
pub mod irdump;
pub mod parse;
pub mod regalloc;
pub mod token;
pub mod sema;
mod util;

#[macro_use]
extern crate lazy_static;

use std::sync::Mutex;

const REGS_N: usize = 7;

lazy_static! {
    pub static ref FILE_NAME: Mutex<String> = Mutex::new(String::new());
}

#[derive(Debug, Clone)]
pub enum Ctype {
    Int,
    Char,
    Void,
    Ptr(Box<Type>), // ptr of
    Ary(Box<Type>, usize), // ary of, len
    Struct(Vec<parse::Node>), // members
}

impl Default for Ctype {
    fn default() -> Ctype {
        Ctype::Int
    }
}

#[derive(Debug, Clone)]
pub struct Type {
    pub ty: Ctype,
    pub size: usize,
    pub align: usize,
}

impl Default for Type {
    fn default() -> Type {
        Type {
            ty: Ctype::default(),
            size: 4,
            align: 4,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Scope {
    Local(usize), // offset
    Global(String, usize, bool), // data, len, is_extern
}

#[derive(Debug, Clone)]
pub struct Var {
    ty: Box<Type>,
    pub name: String,
    pub scope: Scope,
}

impl Var {
    fn new(ty: Box<Type>, name: String, scope: Scope) -> Self {
        Var { ty, name, scope }
    }

    fn new_global(ty: Box<Type>, name: String, data: String, len: usize, is_extern: bool) -> Self {
        let var = Var::new(ty, name.clone(), Scope::Global(data, len, is_extern));
        return var;
    }
}
