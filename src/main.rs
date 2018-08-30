extern crate r9cc;

use r9cc::gen_x86::gen_x86;
use r9cc::gen_ir::{gen_ir, dump_ir};
use r9cc::parse::Node;
use r9cc::regalloc::alloc_regs;
use r9cc::token::tokenize;

use std::env;
fn main() {
    let args: Vec<String> = env::args().collect();
    let input: String;
    let mut dump_tokens = false;
    let mut dump_ir1 = false;
    let mut dump_ir2 = false;

    if args.len() == 3 && args[1] == "-dump-ir1" {
        dump_ir1 = true;
        input = args[2].clone();
    } else if args.len() == 3 && args[1] == "-dump-ir2" {
        dump_ir2 = true;
        input = args[2].clone();
    } else if args.len() == 3 && args[1] == "-dump-tokens" {
        dump_tokens = true;
        input = args[2].clone();
    } else {
        if args.len() != 2 {
            eprint!("Usage: 9cc [-dump-ir1] [-dump-ir2] <code>\n");
            return;
        }
        input = args[1].clone();
    }

    // Tokenize and parse.
    let tokens = tokenize(input);

    if dump_tokens {
        println!("tokens: {:?}", tokens);
    }

    let mut fns = gen_ir(Node::parse(&tokens));

    if dump_ir1 {
        dump_ir(&fns);
    }

    alloc_regs(&mut fns);

    if dump_ir2 {
        dump_ir(&fns);
    }

    gen_x86(fns);
}
