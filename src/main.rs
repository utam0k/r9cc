extern crate r9cc;

use r9cc::codegen::gen_x86;
use r9cc::ir::gen_ir;
use r9cc::parse::Node;
use r9cc::regalloc::alloc_regs;
use r9cc::token::tokenize;

use std::env;
fn main() {
    let mut args = env::args();
    if args.len() != 2 {
        eprint!("Usage: 9cc <code>\n");
        return;
    }

    // Tokenize and parse.
    let tokens = tokenize(args.nth(1).unwrap());
    let node = Node::parse(&tokens);
    let irv = gen_ir(node);
    let irv_alloced = alloc_regs(irv);

    print!(".intel_syntax noprefix\n");
    print!(".global main\n");
    print!("main:\n");
    gen_x86(irv_alloced);
}
