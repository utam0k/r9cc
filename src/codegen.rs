use ir::{IROp, IR};
use REGS;

use std::sync::Mutex;

lazy_static!{
    static ref n: Mutex<usize> = Mutex::new(0);
}

pub fn gen_x86(irv: Vec<IR>) {
    use self::IROp::*;
    let ret = ".Lend";

    print!("  push rbp\n");
    print!("  mov rbp, rsp\n");

    for ir in irv {
        let lhs = ir.lhs.unwrap();
        match ir.op {
            Imm => print!("  mov {}, {}\n", REGS[lhs], ir.rhs.unwrap()),
            Mov => print!("  mov {}, {}\n", REGS[lhs], REGS[ir.rhs.unwrap()]),
            Return => {
                print!("  mov rax, {}\n", REGS[lhs]);
                print!("  jmp {}\n", ret);
            }
            Label => print!(".L{}:\n", lhs),
            Unless => {
                print!("  cmp {}, 0\n", REGS[lhs]);
                print!("  je .L{}\n", ir.rhs.unwrap());
            }
            Alloca => {
                if ir.rhs.is_some() {
                    print!("  sub rsp, {}\n", ir.rhs.unwrap());
                }
                print!("  mov {}, rsp\n", REGS[lhs]);
            }
            Load => print!("  mov {}, [{}]\n", REGS[lhs], REGS[ir.rhs.unwrap()]),
            Store => print!("  mov [{}], {}\n", REGS[lhs], REGS[ir.rhs.unwrap()]),
            Add => print!("  add {}, {}\n", REGS[lhs], REGS[ir.rhs.unwrap()]),
            AddImm => print!("  add {}, {}\n", REGS[lhs], ir.rhs.unwrap()),
            Sub => print!("  sub {}, {}\n", REGS[lhs], REGS[ir.rhs.unwrap()]),
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
    print!("  mov rsp, rbp\n");
    print!("  mov rsp, rbp\n");
    print!("  pop rbp\n");
    print!("  ret\n");
}
