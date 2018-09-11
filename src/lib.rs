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

const REGS_N: usize = 8;
const REGS: [&str; REGS_N] = ["rbp", "r10", "r11", "rbx", "r12", "r13", "r14", "r15"];
const REGS8: [&str; 8] = ["bpl", "r10b", "r11b", "bl", "r12b", "r13b", "r14b", "r15b"];
const REGS32: [&str; 8] = ["ebp", "r10d", "r11d", "ebx", "r12d", "r13d", "r14d", "r15d"];
