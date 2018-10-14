extern crate r9cc;

use r9cc::gen_x86::gen_x86;
use r9cc::gen_ir::gen_ir;
use r9cc::irdump::dump_ir;
use r9cc::parse::parse;
use r9cc::regalloc::alloc_regs;
use r9cc::token::tokenize;
use r9cc::sema::sema;
use r9cc::preprocess;

use std::env;
use std::process;

fn usage() -> ! {
    eprint!("Usage: 9cc [-dump-ir1] [-dump-ir2] <file>\n");
    process::exit(1)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        usage();
    }

    let mut dump_ir1 = false;
    let mut dump_ir2 = false;
    let path;

    if args.len() == 3 && args[1] == "-dump-ir1" {
        dump_ir1 = true;
        path = args[2].clone();
    } else if args.len() == 3 && args[1] == "-dump-ir2" {
        dump_ir2 = true;
        path = args[2].clone();
    } else {
        if args.len() != 2 {
            usage();
        }
        path = args[1].clone();
    }

    let mut context = preprocess::Context::new(vec![].into_iter());

    // Tokenize and parse.
    let tokens = tokenize(path, &mut context);

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
