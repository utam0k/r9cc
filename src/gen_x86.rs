use gen_ir::{IROp, Function};
use REGS;

use std::sync::Mutex;

const ARGREG: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

lazy_static! {
    static ref LABEL: Mutex<usize> = Mutex::new(0);
}


fn gen(f: Function) {
    use self::IROp::*;
    let ret = format!(".Lend{}", *LABEL.lock().unwrap());
    *LABEL.lock().unwrap() += 1;

    print!(".global {}\n", f.name);
    print!("{}:\n", f.name);
    print!("  push rbp\n");
    print!("  mov rbp, rsp\n");
    print!("  sub rsp, {}\n", f.stacksize);
    print!("  push r12\n");
    print!("  push r13\n");
    print!("  push r14\n");
    print!("  push r15\n");

    for ir in f.ir {
        let lhs = ir.lhs.unwrap();
        match ir.op {
            Imm => print!("  mov {}, {}\n", REGS[lhs], ir.rhs.unwrap() as i32),
            Mov => print!("  mov {}, {}\n", REGS[lhs], REGS[ir.rhs.unwrap()]),
            Return => {
                print!("  mov rax, {}\n", REGS[lhs]);
                print!("  jmp {}\n", ret);
            }
            Call(name, nargs, args) => {
                for i in 0..nargs {
                    print!("  mov {}, {}\n", ARGREG[i], REGS[args[i]]);
                }
                print!("  push r10\n");
                print!("  push r11\n");
                print!("  mov rax, 0\n");
                print!("  call {}\n", name);
                print!("  pop r11\n");
                print!("  pop r10\n");

                print!("  mov {}, rax\n", REGS[lhs]);
            }
            Label => print!(".L{}:\n", lhs),
            Jmp => print!("  jmp .L{}\n", lhs),
            Unless => {
                print!("  cmp {}, 0\n", REGS[lhs]);
                print!("  je .L{}\n", ir.rhs.unwrap());
            }
            Load => print!("  mov {}, [{}]\n", REGS[lhs], REGS[ir.rhs.unwrap()]),
            Store => print!("  mov [{}], {}\n", REGS[lhs], REGS[ir.rhs.unwrap()]),
            SaveArgs => {
                for i in 0..lhs {
                    print!("  mov [rbp-{}], {}\n", (i + 1) * 8, ARGREG[i]);
                }
            }
            Add => print!("  add {}, {}\n", REGS[lhs], REGS[ir.rhs.unwrap()]),
            Sub => print!("  sub {}, {}\n", REGS[lhs], REGS[ir.rhs.unwrap()]),
            SubImm => print!("  sub {}, {}\n", REGS[lhs], ir.rhs.unwrap()),
            Mul => {
                print!("  mov rax, {}\n", REGS[ir.rhs.unwrap()]);
                print!("  mul {}\n", REGS[lhs]);
                print!("  mov {}, rax\n", REGS[lhs]);
            }
            Div => {
                print!("  mov rax, {}\n", REGS[lhs]);
                print!("  cqo\n");
                print!("  div {}\n", REGS[ir.rhs.unwrap()]);
                print!("  mov {}, rax\n", REGS[lhs]);
            }
            Nop | Kill => (),
        }
    }

    print!("{}:\n", ret);
    print!("  pop r15\n");
    print!("  pop r14\n");
    print!("  pop r13\n");
    print!("  pop r12\n");
    print!("  mov rsp, rbp\n");
    print!("  pop rbp\n");
    print!("  ret\n");
}

pub fn gen_x86(fns: Vec<Function>) {
    print!(".intel_syntax noprefix\n");
    for f in fns {
        gen(f);
    }
}
