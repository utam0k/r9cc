extern crate r9cc;

use r9cc::gen_x86::gen_x86;
use r9cc::gen_ir::gen_ir;
use r9cc::irdump::dump_ir;
use r9cc::parse::parse;
use r9cc::regalloc::alloc_regs;
use r9cc::token::tokenize;
use r9cc::sema::sema;

use std::env;
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut input = String::new();
    let mut dump_tokens = false;
    let mut dump_ir1 = false;
    let mut dump_ir2 = false;
    let filename: String;

    if args.len() == 3 && args[1] == "-dump-ir1" {
        dump_ir1 = true;
        filename = args[2].clone();
    } else if args.len() == 3 && args[1] == "-dump-ir2" {
        dump_ir2 = true;
        filename = args[2].clone();
    } else if args.len() == 3 && args[1] == "-dump-tokens" {
        dump_tokens = true;
        filename = args[2].clone();
    } else {
        if args.len() != 2 {
            eprint!("Usage: 9cc [-dump-ir1] [-dump-ir2] <file>\n");
            return;
        }
        filename = args[1].clone();
    }
    let mut file = File::open(filename).unwrap();
    file.read_to_string(&mut input).unwrap();

    // Tokenize and parse.
    let tokens = tokenize(input.chars().collect());

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
