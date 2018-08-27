pub mod codegen;
pub mod ir;
pub mod parse;
pub mod regalloc;
pub mod token;

#[macro_use]
extern crate lazy_static;

const REGS_N: usize = 7;
const REGS: [&str; REGS_N] = ["r10", "r11", "rbx", "r12", "r13", "r14", "r15"];
