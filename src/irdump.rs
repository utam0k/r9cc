use crate::gen_ir::{Function, IROp, IRType, IR};

use std::fmt;

#[derive(Clone, Debug)]
pub struct IRInfo {
    name: &'static str,
    pub ty: IRType,
}

impl IRInfo {
    pub fn new(name: &'static str, ty: IRType) -> Self {
        IRInfo { name, ty }
    }
}

impl<'a> From<&'a IROp> for IRInfo {
    fn from(op: &'a IROp) -> IRInfo {
        use self::IROp::*;
        match op {
            Add => IRInfo::new("ADD", IRType::RegReg),
            AddImm => IRInfo::new("ADD", IRType::RegImm),
            Call(_, _, _) => IRInfo::new("CALL", IRType::Call),
            Div => IRInfo::new("DIV", IRType::RegReg),
            Imm => IRInfo::new("MOV", IRType::RegImm),
            Jmp => IRInfo::new("JMP", IRType::Jmp),
            Kill => IRInfo::new("KILL", IRType::Reg),
            Label => IRInfo::new("", IRType::Label),
            LabelAddr(_) => IRInfo::new("LABEL_ADDR", IRType::LabelAddr),
            EQ => IRInfo::new("EQ", IRType::RegReg),
            NE => IRInfo::new("NE", IRType::RegReg),
            LE => IRInfo::new("LE", IRType::RegReg),
            LT => IRInfo::new("LT", IRType::RegReg),
            AND => IRInfo::new("AND", IRType::RegReg),
            OR => IRInfo::new("OR", IRType::RegReg),
            XOR => IRInfo::new("XOR", IRType::RegReg),
            SHL => IRInfo::new("SHL", IRType::RegReg),
            SHR => IRInfo::new("SHR", IRType::RegReg),
            Mod => IRInfo::new("MOD", IRType::RegReg),
            Neg => IRInfo::new("NEG", IRType::Reg),
            Load(_) => IRInfo::new("LOAD", IRType::Mem),
            Mov => IRInfo::new("MOV", IRType::RegReg),
            Mul => IRInfo::new("MUL", IRType::RegReg),
            MulImm => IRInfo::new("MUL", IRType::RegImm),
            Nop => IRInfo::new("NOP", IRType::Noarg),
            Return => IRInfo::new("RET", IRType::Reg),
            Store(_) => IRInfo::new("STORE", IRType::Mem),
            StoreArg(_) => IRInfo::new("STORE_ARG", IRType::StoreArg),
            Sub => IRInfo::new("SUB", IRType::RegReg),
            SubImm => IRInfo::new("SUB", IRType::RegImm),
            Bprel => IRInfo::new("BPREL", IRType::RegImm),
            If => IRInfo::new("IF", IRType::RegLabel),
            Unless => IRInfo::new("UNLESS", IRType::RegLabel),
        }
    }
}

impl fmt::Display for IR {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::IRType::*;

        let info = &IRInfo::from(&self.op);

        let lhs = self.lhs.unwrap();
        match info.ty {
            Label => write!(f, ".L{}:", lhs),
            LabelAddr => match self.op {
                IROp::LabelAddr(ref name) => write!(f, "  {} r{}, {}", info.name, lhs, name),
                _ => unreachable!(),
            },
            Imm => write!(f, "  {} {}", info.name, lhs),
            Reg => write!(f, "  {} r{}", info.name, lhs),
            Jmp => write!(f, "  {} .L{}", info.name, lhs),
            RegReg => write!(f, "  {} r{}, r{}", info.name, lhs, self.rhs.unwrap()),
            Mem | StoreArg => match self.op {
                IROp::Load(ref size) | IROp::Store(ref size) => {
                    write!(f, "  {}{} r{}, {}", info.name, size, lhs, self.rhs.unwrap())
                }
                IROp::StoreArg(ref size) => {
                    write!(f, "  {}{} {}, {}", info.name, size, lhs, self.rhs.unwrap())
                }
                _ => unreachable!(),
            },
            RegImm => write!(f, "  {} r{}, {}", info.name, lhs, self.rhs.unwrap() as i32),
            RegLabel => write!(f, "  {} r{}, .L{}", info.name, lhs, self.rhs.unwrap()),
            Call => match self.op {
                IROp::Call(ref name, nargs, args) => {
                    let mut sb: String = format!("  r{} = {}(", lhs, name);
                    for (i, arg) in args.iter().enumerate().take(nargs) {
                        if i != 0 {
                            sb.push_str(&", ".to_string());
                        }
                        sb.push_str(&format!("r{}", *arg));
                    }
                    sb.push_str(")");
                    write!(f, "{}", sb)
                }
                _ => unreachable!(),
            },
            Noarg => write!(f, "  {}", info.name),
        }
    }
}

pub fn dump_ir(fns: &[Function]) {
    for f in fns {
        eprintln!("{}(): ", f.name);
        for ir in &f.ir {
            eprintln!("{}", ir);
        }
    }
}
