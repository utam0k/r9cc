pub mod gen_x86;
pub mod gen_ir;
pub mod parse;
pub mod regalloc;
pub mod token;

#[macro_use]
extern crate lazy_static;

const REGS_N: usize = 8;
const REGS: [&str; REGS_N] = ["rbp", "r10", "r11", "rbx", "r12", "r13", "r14", "r15"];
