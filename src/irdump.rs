use gen_ir::{IROp, Function, IRType, IR};

use std::fmt;

#[derive(Clone, Debug)]
pub struct IRInfo {
    name: &'static str,
    pub ty: IRType,
}

impl IRInfo {
    pub fn new(name: &'static str, ty: IRType) -> Self {
        IRInfo { name: name, ty: ty }
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
            Load8 => IRInfo::new("LOAD8", IRType::RegReg),
            Load32 => IRInfo::new("LOAD32", IRType::RegReg),
            Load64 => IRInfo::new("LOAD64", IRType::RegReg),
            Mov => IRInfo::new("MOV", IRType::RegReg),
            Mul => IRInfo::new("MUL", IRType::RegReg),
            MulImm => IRInfo::new("MUL", IRType::RegImm),
            Nop => IRInfo::new("NOP", IRType::Noarg),
            Return => IRInfo::new("RET", IRType::Reg),
            Store8 => IRInfo::new("STORE8", IRType::RegReg),
            Store32 => IRInfo::new("STORE32", IRType::RegReg),
            Store64 => IRInfo::new("STORE64", IRType::RegReg),
            Store8Arg => IRInfo::new("STORE8_ARG", IRType::ImmImm),
            Store32Arg => IRInfo::new("STORE32_ARG", IRType::ImmImm),
            Store64Arg => IRInfo::new("STORE64_ARG", IRType::ImmImm),
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
            LabelAddr => {
                match self.op {
                    IROp::LabelAddr(ref name) => write!(f, "  {} r{}, {}", info.name, lhs, name),
                    _ => unreachable!(),
                }
            }
            Imm => write!(f, "  {} {}", info.name, lhs),
            Reg => write!(f, "  {} r{}", info.name, lhs),
            Jmp => write!(f, "  {} .L{}", info.name, lhs),
            RegReg => write!(f, "  {} r{}, r{}", info.name, lhs, self.rhs.unwrap()),
            RegImm => write!(f, "  {} r{}, {}", info.name, lhs, self.rhs.unwrap() as i32),
            ImmImm => write!(f, "  {} {}, {}", info.name, lhs, self.rhs.unwrap()),
            RegLabel => write!(f, "  {} r{}, .L{}", info.name, lhs, self.rhs.unwrap()),
            Call => {
                match self.op {
                    IROp::Call(ref name, nargs, args) => {
                        let mut sb: String = format!("  r{} = {}(", lhs, name);
                        for i in 0..nargs {
                            if i != 0 {
                                sb.push_str(&format!(", "));
                            }
                            sb.push_str(&format!("r{}", args[i]));
                        }
                        sb.push_str(")");
                        write!(f, "{}", sb)
                    }
                    _ => unreachable!(),
                }
            }
            Noarg => write!(f, "  {}", info.name),
        }
    }
}

pub fn dump_ir(fns: &Vec<Function>) {
    for f in fns {
        print!("{}(): \n", f.name);
        for ir in &f.ir {
            print!("{}\n", ir);
        }
    }
}
