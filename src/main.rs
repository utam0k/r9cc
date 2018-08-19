extern crate r9cc;
use r9cc::strtol;

use std::env;

fn main() {
    let mut args = env::args();
    if args.len() != 2 {
        eprint!("Usage: 9cc <code>\n");
        return;
    }

    let p = args.nth(1).unwrap();

    print!(".intel_syntax noprefix\n");
    print!(".global main\n");
    print!("main:\n");


    let (n, mut p) = strtol(&p);
    print!("  mov rax, {}\n", n.unwrap());

    while let Some(c) = p.chars().nth(0) {
        let s = p.split_off(1);

        if c == '+' {
            let (n, remaining) = strtol(&s);
            p = remaining;
            print!("  add rax, {}\n", n.unwrap());
            continue;
        }

        if c == '-' {
            let (n, remaining) = strtol(&s);
            p = remaining;
            print!("  sub rax, {}\n", n.unwrap());
            continue;
        }

        eprint!("unexpected character: {}\n", p);
        return;
    }

    print!("  ret\n");
    return;
}
