use ir::{IRType, IR};
use REGS;

use std::sync::Mutex;

lazy_static!{
    static ref n: Mutex<usize> = Mutex::new(0);
}

fn gen_label() -> String {
    let label = format!(".L{}", *n.lock().unwrap());
    *n.lock().unwrap() += 1;
    return label;
}

pub fn gen_x86(irv: Vec<IR>) {
    use self::IRType::*;
    let ret = gen_label();

    print!("  push rbp\n");
    print!("  mov rbp, rsp\n");

    for ir in irv {
        let lhs = ir.lhs.unwrap();
        match ir.op {
            IMM => print!("  mov {}, {}\n", REGS[lhs], ir.rhs.unwrap()),
            MOV => print!("  mov {}, {}\n", REGS[lhs], REGS[ir.rhs.unwrap()]),
            RETURN => {
                print!("  mov rax, {}\n", REGS[lhs]);
                print!("  jmp {}\n", ret);
            }
            ALLOCA => {
                if ir.rhs.is_some() {
                    print!("  sub rsp, {}\n", ir.rhs.unwrap());
                }
                print!("  mov {}, rsp\n", REGS[lhs]);
            }
            LOAD => print!("  mov {}, [{}]\n", REGS[lhs], REGS[ir.rhs.unwrap()]),
            STORE => print!("  mov [{}], {}\n", REGS[lhs], REGS[ir.rhs.unwrap()]),
            ADD(imm) => {
                match imm {
                    Some(val) => print!("  add {}, {}\n", REGS[lhs], val),
                    None => print!("  add {}, {}\n", REGS[lhs], REGS[ir.rhs.unwrap()]),
                }
            }
            SUB => print!("  sub {}, {}\n", REGS[lhs], REGS[ir.rhs.unwrap()]),
            MUL => {
                print!("  mov rax, {}\n", REGS[ir.rhs.unwrap()]);
                print!("  mul {}\n", REGS[lhs]);
                print!("  mov {}, rax\n", REGS[lhs]);
            }
            DIV => {
                print!("  mov rax, {}\n", REGS[lhs]);
                print!("  cqo\n");
                print!("  div {}\n", REGS[ir.rhs.unwrap()]);
                print!("  mov {}, rax\n", REGS[lhs]);
            }
            NOP | KILL => (),
        }
    }

    print!("{}:\n", ret);
    print!("  mov rsp, rbp\n");
    print!("  mov rsp, rbp\n");
    print!("  pop rbp\n");
    print!("  ret\n");
}
