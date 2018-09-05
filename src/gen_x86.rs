use gen_ir::{IROp, Function, IR};
use sema::{Scope, Var};
use REGS;
use REGS8;
use REGS32;

use std::sync::Mutex;

const ARGREG8: [&str; 6] = ["dil", "sil", "dl", "cl", "r8b", "r9b"];
const ARGREG32: [&str; 6] = ["edi", "esi", "edx", "ecx", "r8d", "r9d"];
const ARGREG64: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

lazy_static! {
    static ref LABEL: Mutex<usize> = Mutex::new(0);
}

fn escape(s: String, len: usize) -> String {
    let mut buf = String::new();
    for i in 0..len {
        if let Some(c) = s.chars().collect::<Vec<char>>().get(i) {
            if c == &'\\' || c == &'"' {
                buf.push('\\');
                buf.push(c.clone());
            } else if c.is_ascii_graphic() || c == &' ' {
                buf.push(c.clone());
            } else {
                buf.push_str(&format!("\\{:o}", c.clone() as i8));
            }
        } else {
            buf.push_str("\\000");
        }
    }
    return buf;
}

fn emit_cmp(ir: IR, insn: &'static str) {
    let lhs = ir.lhs.unwrap();
    let rhs = ir.rhs.unwrap();
    print!("  cmp {}, {}\n", REGS[lhs], REGS[rhs]);
    print!("  {} {}\n", insn, REGS8[lhs]);
    print!("  movzb {}, {}\n", REGS[lhs], REGS8[lhs]);
}

fn gen(f: Function) {
    use self::IROp::*;
    let ret = format!(".Lend{}", *LABEL.lock().unwrap());
    *LABEL.lock().unwrap() += 1;

    print!(".text\n");
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
                    print!("  mov {}, {}\n", ARGREG64[i], REGS[args[i]]);
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
            LabelAddr(name) => print!("  lea {}, {}\n", REGS[lhs], name),
            EQ => emit_cmp(ir, "sete"),
            NE => emit_cmp(ir, "setne"),
            LT => emit_cmp(ir, "setl"),
            Jmp => print!("  jmp .L{}\n", lhs),
            If => {
                print!("  cmp {}, 0\n", REGS[lhs]);
                print!("  jne .L{}\n", ir.rhs.unwrap());
            }
            Unless => {
                print!("  cmp {}, 0\n", REGS[lhs]);
                print!("  je .L{}\n", ir.rhs.unwrap());
            }
            Load8 => {
                print!("  mov {}, [{}]\n", REGS8[lhs], REGS[ir.rhs.unwrap()]);
                print!("  movzb {}, {}\n", REGS[lhs], REGS8[lhs]);
            }
            Load32 => print!("  mov {}, [{}]\n", REGS32[lhs], REGS[ir.rhs.unwrap()]),
            Load64 => print!("  mov {}, [{}]\n", REGS[lhs], REGS[ir.rhs.unwrap()]),
            Store8 => print!("  mov [{}], {}\n", REGS[lhs], REGS8[ir.rhs.unwrap()]),
            Store32 => print!("  mov [{}], {}\n", REGS[lhs], REGS32[ir.rhs.unwrap()]),
            Store64 => print!("  mov [{}], {}\n", REGS[lhs], REGS[ir.rhs.unwrap()]),
            Store8Arg => print!("  mov [rbp-{}], {}\n", lhs, ARGREG8[ir.rhs.unwrap()]),
            Store32Arg => print!("  mov [rbp-{}], {}\n", lhs, ARGREG32[ir.rhs.unwrap()]),
            Store64Arg => print!("  mov [rbp-{}], {}\n", lhs, ARGREG64[ir.rhs.unwrap()]),
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

pub fn gen_x86(globals: Vec<Var>, fns: Vec<Function>) {
    print!(".intel_syntax noprefix\n");
    print!(".data\n");
    for var in globals {
        if let Scope::Global(data, len, is_extern) = var.scope {
            if is_extern {
                continue;
            }
            print!("{}:\n", var.name);
            print!("  .ascii \"{}\"\n", escape(data, len));
            continue;
        }
        unreachable!();
    }

    for f in fns {
        gen(f);
    }
}
