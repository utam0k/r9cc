extern crate r9cc;

use r9cc::gen_x86::gen_x86;
use r9cc::gen_ir::{gen_ir, dump_ir};
use r9cc::parse::parse;
use r9cc::regalloc::alloc_regs;
use r9cc::token::tokenize;
use r9cc::sema::sema;

use std::env;
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut input: String;
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
    } else if args.len() == 3 && args[1] == "-path" {
        input = String::new();
        let mut file = File::open(args[2].clone()).unwrap();
        file.read_to_string(&mut input).unwrap();
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
        for token in &tokens {
            println!("{:?}", token.ty);
        }
    }

    let nodes = parse(&tokens);
    let (nodes, globals) = sema(nodes);
    let mut fns = gen_ir(nodes);

    if dump_ir1 {
        dump_ir(&fns);
    }

    alloc_regs(&mut fns);

    if dump_ir2 {
        dump_ir(&fns);
    }

    gen_x86(globals, fns);
}
