use gen_ir::{Function, IROp, IR};
use util::roundup;
use {Scope, Var, REGS_N};

const REGS: [&str; REGS_N] = ["r10", "r11", "rbx", "r12", "r13", "r14", "r15"];
const REGS8: [&str; REGS_N] = ["r10b", "r11b", "bl", "r12b", "r13b", "r14b", "r15b"];
const REGS32: [&str; REGS_N] = ["r10d", "r11d", "ebx", "r12d", "r13d", "r14d", "r15d"];

use std::sync::Mutex;

// Quoted from 9cc
// > This pass generates x86-64 assembly from IR.

const ARGREGS: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
const ARGREGS8: [&str; 6] = ["dil", "sil", "dl", "cl", "r8b", "r9b"];
const ARGREGS32: [&str; 6] = ["edi", "esi", "edx", "ecx", "r8d", "r9d"];

lazy_static! {
    static ref LABEL: Mutex<usize> = Mutex::new(0);
}

fn backslash_escape(s: String, len: usize) -> String {
    let mut sb = String::new();
    for i in 0..len {
        if let Some(c) = s.chars().collect::<Vec<char>>().get(i) {
            // Issue: https://github.com/rust-lang/rfcs/issues/751
            let escaped = match c {
                // 'b' => Some("\b"),
                // 'f' => Some("\f"),
                '\n' => Some('n'),
                '\r' => Some('r'),
                '\t' => Some('t'),
                '\\' => Some('\\'),
                '\'' => Some('\''),
                '\"' => Some('\"'),
                _ => None,
            };
            if let Some(esc) = escaped {
                sb.push('\\');
                sb.push(esc);
            } else if c.is_ascii_graphic() || c == &' ' {
                sb.push(c.clone());
            } else {
                sb.push_str(&format!("\\{:o}", c.clone() as i8));
            }
            if i == len - 1 {
                sb.push_str("\\000");
            }
        } else {
            sb.push_str("\\000");
        }
    }
    sb
}

macro_rules! emit{
    ($fmt:expr) => (print!(concat!("\t", $fmt, "\n")));
    ($fmt:expr, $($arg:tt)*) => (print!(concat!("\t", $fmt, "\n"), $($arg)*));
}

fn emit_cmp(ir: IR, insn: &'static str) {
    let lhs = ir.lhs.unwrap();
    let rhs = ir.rhs.unwrap();
    emit!("cmp {}, {}", REGS[lhs], REGS[rhs]);
    emit!("{} {}", insn, REGS8[lhs]);
    emit!("movzb {}, {}", REGS[lhs], REGS8[lhs]);
}

fn reg(r: usize, size: u8) -> &'static str {
    match size {
        1 => REGS8[r],
        4 => REGS32[r],
        8 => REGS[r],
        _ => unreachable!(),
    }
}

fn argreg(r: usize, size: u8) -> &'static str {
    match size {
        1 => ARGREGS8[r],
        4 => ARGREGS32[r],
        8 => ARGREGS[r],
        _ => unreachable!(),
    }
}

fn gen(f: Function) {
    use self::IROp::*;
    let ret = format!(".Lend{}", *LABEL.lock().unwrap());
    *LABEL.lock().unwrap() += 1;

    println!(".text");
    println!(".global {}", f.name);
    println!("{}:", f.name);
    emit!("push rbp");
    emit!("mov rbp, rsp");
    emit!("sub rsp, {}", roundup(f.stacksize, 16));
    emit!("push r12");
    emit!("push r13");
    emit!("push r14");
    emit!("push r15");

    for ir in f.ir {
        let lhs = ir.lhs.unwrap();
        let rhs = ir.rhs.unwrap_or(0);
        match ir.op {
            Imm => emit!("mov {}, {}", REGS[lhs], rhs as i32),
            Mov => emit!("mov {}, {}", REGS[lhs], REGS[rhs]),
            Return => {
                emit!("mov rax, {}", REGS[lhs]);
                emit!("jmp {}", ret);
            }
            Call(name, nargs, args) => {
                for i in 0..nargs {
                    emit!("mov {}, {}", ARGREGS[i], REGS[args[i]]);
                }
                emit!("push r10");
                emit!("push r11");
                emit!("mov rax, 0");
                emit!("call {}", name);
                emit!("pop r11");
                emit!("pop r10");

                emit!("mov {}, rax", REGS[lhs]);
            }
            Label => println!(".L{}:", lhs),
            LabelAddr(name) => emit!("lea {}, {}", REGS[lhs], name),
            Neg => emit!("neg {}", REGS[lhs]),
            EQ => emit_cmp(ir, "sete"),
            NE => emit_cmp(ir, "setne"),
            LT => emit_cmp(ir, "setl"),
            LE => emit_cmp(ir, "setle"),
            AND => emit!("and {}, {}", REGS[lhs], REGS[rhs]),
            OR => emit!("or {}, {}", REGS[lhs], REGS[rhs]),
            XOR => emit!("xor {}, {}", REGS[lhs], REGS[rhs]),
            SHL => {
                emit!("mov cl, {}", REGS8[rhs]);
                emit!("shl {}, cl", REGS[lhs]);
            }
            SHR => {
                emit!("mov cl, {}", REGS8[rhs]);
                emit!("shr {}, cl", REGS[lhs]);
            }
            Mod => {
                /* Same mean(?).
                 * emit!("mov rdx, 0");
                 * emit!("mov rax, {}", REGS[lhs]);
                 */
                emit!("mov rax, {}", REGS[lhs]);
                emit!("cqo"); // rax -> rdx:rax
                emit!("div {}", REGS[rhs]);
                emit!("mov {}, rdx", REGS[lhs]);
            }
            Jmp => emit!("jmp .L{}", lhs),
            If => {
                emit!("cmp {}, 0", REGS[lhs]);
                emit!("jne .L{}", rhs);
            }
            Unless => {
                emit!("cmp {}, 0", REGS[lhs]);
                emit!("je .L{}", rhs);
            }
            Load(size) => {
                emit!("mov {}, [{}]", reg(lhs, size), REGS[rhs]);
                if size == 1 {
                    emit!("movzb {}, {}", REGS[lhs], REGS8[lhs]);
                }
            }
            Store(size) => emit!("mov [{}], {}", REGS[lhs], reg(rhs, size)),
            StoreArg(size) => emit!("mov [rbp-{}], {}", lhs, argreg(rhs, size)),
            Add => emit!("add {}, {}", REGS[lhs], REGS[rhs]),
            AddImm => emit!("add {}, {}", REGS[lhs], rhs as i32),
            Sub => emit!("sub {}, {}", REGS[lhs], REGS[rhs]),
            SubImm => emit!("sub {}, {}", REGS[lhs], rhs as i32),
            Bprel => emit!("lea {}, [rbp-{}]", REGS[lhs], rhs),
            Mul => {
                emit!("mov rax, {}", REGS[rhs]);
                emit!("mul {}", REGS[lhs]);
                emit!("mov {}, rax", REGS[lhs]);
            }
            MulImm => {
                if rhs < 256 && rhs.count_ones() == 1 {
                    use std::intrinsics::cttz;
                    emit!("shl {}, {}", REGS[lhs], unsafe { cttz(rhs) });
                } else {
                    emit!("mov rax, {}", rhs as i32);
                    emit!("mul {}", REGS[lhs]);
                    emit!("mov {}, rax", REGS[lhs]);
                }
            }
            Div => {
                emit!("mov rax, {}", REGS[lhs]);
                emit!("cqo");
                emit!("div {}", REGS[rhs]);
                emit!("mov {}, rax", REGS[lhs]);
            }
            Nop | Kill => (),
        }
    }

    println!("{}:", ret);
    emit!("pop r15");
    emit!("pop r14");
    emit!("pop r13");
    emit!("pop r12");
    emit!("mov rsp, rbp");
    emit!("pop rbp");
    emit!("ret");
}

pub fn gen_x86(globals: Vec<Var>, fns: Vec<Function>) {
    println!(".intel_syntax noprefix");
    println!(".data");
    for var in globals {
        if let Scope::Global(data, len, is_extern) = var.scope {
            if is_extern {
                continue;
            }
            println!("{}:", var.name);
            emit!(".ascii \"{}\"", backslash_escape(data, len));
            continue;
        }
        unreachable!();
    }

    for f in fns {
        gen(f);
    }
}
