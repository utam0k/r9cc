extern crate r9cc;

use r9cc::codegen::gen_x86;
use r9cc::ir::{gen_ir, dump_ir};
use r9cc::parse::Node;
use r9cc::regalloc::alloc_regs;
use r9cc::token::tokenize;

use std::env;
fn main() {
    let mut args = env::args();
    let input: String;
    let mut dump_ir1 = false;
    let mut dump_ir2 = false;

    if args.len() == 3 && args.nth(1).unwrap() == "-dump-ir1" {
        dump_ir1 = true;
        input = args.nth(0).unwrap();
    } else if args.len() == 3 && args.nth(1).unwrap() == "-dump-ir2" {
        dump_ir2 = true;
        input = args.nth(0).unwrap();
    } else {
        if args.len() != 2 {
            eprint!("Usage: 9cc [-dump-ir1] [-dump-ir2] <code>\n");
            return;
        }
        input = args.nth(1).unwrap();
    }

    // Tokenize and parse.
    let tokens = tokenize(input);
    let node = Node::parse(&tokens);

    let mut irv = gen_ir(node);

    if dump_ir1 {
        dump_ir(&irv);
    }

    alloc_regs(&mut irv);

    if dump_ir2 {
        dump_ir(&irv);
    }

    print!(".intel_syntax noprefix\n");
    print!(".global main\n");
    print!("main:\n");
    gen_x86(irv);
}
