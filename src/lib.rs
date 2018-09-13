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

const REGS_N: usize = 7;
const REGS: [&str; REGS_N] = ["r10", "r11", "rbx", "r12", "r13", "r14", "r15"];
const REGS8: [&str; REGS_N] = ["r10b", "r11b", "bl", "r12b", "r13b", "r14b", "r15b"];
const REGS32: [&str; REGS_N] = ["r10d", "r11d", "ebx", "r12d", "r13d", "r14d", "r15d"];
