// Compile AST to intermediate code that has infinite number of registers.
// Base pointer is always assigned to r0.

use parse::{Node, NodeType};
use token::TokenType;

use std::sync::Mutex;
use std::fmt;
use std::collections::HashMap;

lazy_static!{
    static ref VARS: Mutex<HashMap<String, usize>> = Mutex::new(HashMap::new());

    static ref REGNO: Mutex<usize> = Mutex::new(1);
    static ref STACKSIZE: Mutex<usize> = Mutex::new(0);
    static ref LABEL: Mutex<usize> = Mutex::new(0);
}

#[derive(Clone, Debug)]
pub enum IRType {
    Noarg,
    Reg,
    Imm,
    Jmp,
    Label,
    RegReg,
    RegImm,
    RegLabel,
    Call,
}

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

pub fn dump_ir(fns: &Vec<Function>) {
    for f in fns {
        print!("{}(): \n", f.name);
        for ir in &f.ir {
            print!("{}\n", ir);
        }
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub ir: Vec<IR>,
    pub stacksize: usize,
}

impl Function {
    fn new(name: String, ir: Vec<IR>, stacksize: usize) -> Self {
        Function {
            name: name,
            ir: ir,
            stacksize: stacksize,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IROp {
    Add,
    Sub,
    Mul,
    Div,
    Imm,
    SubImm,
    Mov,
    Return,
    Call(String, usize, [usize; 6]),
    Label,
    LT,
    Jmp,
    Unless,
    Load,
    Store,
    Kill,
    SaveArgs,
    Nop,
}

impl<'a> From<&'a IROp> for IRInfo {
    fn from(op: &'a IROp) -> IRInfo {
        use self::IROp::*;
        match op {
            Add => IRInfo::new("ADD", IRType::RegReg),
            Sub => IRInfo::new("SUB", IRType::RegReg),
            Mul => IRInfo::new("MUL", IRType::RegReg),
            Div => IRInfo::new("DIV", IRType::RegReg),
            Imm => IRInfo::new("MOV", IRType::RegImm),
            SubImm => IRInfo::new("SUB", IRType::RegImm),
            Mov => IRInfo::new("MOV", IRType::RegReg),
            Return => IRInfo::new("RET", IRType::Reg),
            Call(_, _, _) => IRInfo::new("CALL", IRType::Call),
            Label => IRInfo::new("", IRType::Label),
            LT => IRInfo::new("LT", IRType::RegReg),
            Jmp => IRInfo::new("JMP", IRType::Jmp),
            Unless => IRInfo::new("UNLESS", IRType::RegLabel),
            Load => IRInfo::new("LOAD", IRType::RegReg),
            Store => IRInfo::new("STORE", IRType::RegReg),
            Kill => IRInfo::new("KILL", IRType::Reg),
            SaveArgs => IRInfo::new("SAVE_ARGS", IRType::Imm),
            Nop => IRInfo::new("NOP", IRType::Noarg),
        }
    }
}

impl fmt::Display for IR {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::IRType::*;

        let info = &IRInfo::from(&self.op);

        let lhs = self.lhs.unwrap();
        match info.ty {
            Label => write!(f, ".L{}=>", lhs),
            Imm => write!(f, "  {} {}", info.name, lhs),
            Reg => write!(f, "  {} r{}", info.name, lhs),
            Jmp => write!(f, "  {} .L{}", info.name, lhs),
            RegReg => write!(f, "  {} r{}, r{}", info.name, lhs, self.rhs.unwrap()),
            RegImm => write!(f, "  {} r{}, {}", info.name, lhs, self.rhs.unwrap()),
            RegLabel => write!(f, "  {} r{}, .L{}", info.name, lhs, self.rhs.unwrap()),
            Call => {
                match self.op {
                    IROp::Call(ref name, nargs, args) => {
                        let mut sb: String = format!("  r{} = {}(", lhs, name);
                        for i in 0..nargs {
                            sb.push_str(&format!(", r{}", args[i]));
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

impl From<NodeType> for IROp {
    fn from(node_type: NodeType) -> Self {
        match node_type {
            NodeType::BinOp(op, _, _) => Self::from(op),
            e => panic!("cannot convert: {:?}", e),
        }
    }
}

impl From<TokenType> for IROp {
    fn from(token_type: TokenType) -> Self {
        match token_type {
            TokenType::Plus => IROp::Add,
            TokenType::Minus => IROp::Sub,
            TokenType::Mul => IROp::Mul,
            TokenType::Div => IROp::Div,
            TokenType::LeftAngleBracket |
            TokenType::RightAngleBracket => IROp::LT,
            e => panic!("cannot convert: {:?}", e),
        }
    }
}

#[derive(Debug, Clone)]
pub struct IR {
    pub op: IROp,
    pub lhs: Option<usize>,
    pub rhs: Option<usize>,
}

impl IR {
    fn new(op: IROp, lhs: Option<usize>, rhs: Option<usize>) -> Self {
        Self {
            op: op,
            lhs: lhs,
            rhs: rhs,
        }
    }
}

fn gen_lval(code: &mut Vec<IR>, node: Node) -> Option<usize> {
    match node.ty {
        NodeType::Ident(name) => {
            if VARS.lock().unwrap().get(&name).is_none() {
                VARS.lock().unwrap().insert(
                    name.clone(),
                    *STACKSIZE.lock().unwrap(),
                );
                *STACKSIZE.lock().unwrap() += 8;
            }
            let r = Some(*REGNO.lock().unwrap());
            *REGNO.lock().unwrap() += 1;
            let off = *VARS.lock().unwrap().get(&name).unwrap();
            code.push(IR::new(IROp::Mov, r, Some(0)));
            code.push(IR::new(IROp::SubImm, r, Some(off)));
            return r;
        }
        _ => panic!("not an lvalue"),
    }
}

fn gen_expr(code: &mut Vec<IR>, node: Node) -> Option<usize> {
    match node.ty {
        NodeType::Num(val) => {
            let r = Some(*REGNO.lock().unwrap());
            *REGNO.lock().unwrap() += 1;
            code.push(IR::new(IROp::Imm, r, Some(val as usize)));
            return r;
        }
        NodeType::Logand(lhs, rhs) => {
            let x = Some(*LABEL.lock().unwrap());
            *LABEL.lock().unwrap() += 1;

            let r1 = gen_expr(code, *lhs);
            code.push(IR::new(IROp::Unless, r1, x));
            let r2 = gen_expr(code, *rhs);
            code.push(IR::new(IROp::Mov, r1, r2));
            code.push(IR::new(IROp::Kill, r2, None));
            code.push(IR::new(IROp::Unless, r1, x));
            code.push(IR::new(IROp::Imm, r1, Some(1)));
            code.push(IR::new(IROp::Label, x, None));
            return r1;
        }
        NodeType::Logor(lhs, rhs) => {
            let x = Some(*LABEL.lock().unwrap());
            *LABEL.lock().unwrap() += 1;
            let y = Some(*LABEL.lock().unwrap());
            *LABEL.lock().unwrap() += 1;

            let r1 = gen_expr(code, *lhs);
            code.push(IR::new(IROp::Unless, r1, x));
            code.push(IR::new(IROp::Imm, r1, Some(1)));
            code.push(IR::new(IROp::Jmp, y, None));
            code.push(IR::new(IROp::Label, x, None));

            let r2 = gen_expr(code, *rhs);
            code.push(IR::new(IROp::Mov, r1, r2));
            code.push(IR::new(IROp::Kill, r2, None));
            code.push(IR::new(IROp::Unless, r1, y));
            code.push(IR::new(IROp::Imm, r1, Some(1)));
            code.push(IR::new(IROp::Label, y, None));
            return r1;
        }
        NodeType::Ident(_) => {
            let r = gen_lval(code, node);
            code.push(IR::new(IROp::Load, r, r));
            return r;
        }
        NodeType::Call(name, args) => {
            let mut args_ir: [usize; 6] = [0; 6];
            for i in 0..args.len() {
                args_ir[i] = gen_expr(code, args[i].clone()).unwrap();
            }

            let r = Some(*REGNO.lock().unwrap());
            *REGNO.lock().unwrap() += 1;

            code.push(IR::new(IROp::Call(name, args.len(), args_ir), r, None));

            for i in 0..args.len() {
                code.push(IR::new(IROp::Kill, Some(args_ir[i]), None));
            }
            return r;
        }
        NodeType::BinOp(op, lhs, rhs) => {
            match op {
                TokenType::Equal => {
                    let rhs = gen_expr(code, *rhs);
                    let lhs = gen_lval(code, *lhs);
                    code.push(IR::new(IROp::Store, lhs, rhs));
                    code.push(IR::new(IROp::Kill, rhs, None));
                    return lhs;
                }
                _ => {
                    // gen_binop
                    let lhs = gen_expr(code, *lhs);
                    let rhs = gen_expr(code, *rhs);

                    code.push(IR::new(IROp::from(op), lhs, rhs));
                    code.push(IR::new(IROp::Kill, rhs, None));
                    return lhs;
                }
            }
        }
        _ => unreachable!(),
    }
}

fn gen_stmt(code: &mut Vec<IR>, node: Node) {
    match node.ty {
        NodeType::If(cond, then, els_may) => {
            let r = gen_expr(code, *cond);
            let x = Some(*LABEL.lock().unwrap());
            *LABEL.lock().unwrap() += 1;
            code.push(IR::new(IROp::Unless, r, x));
            code.push(IR::new(IROp::Kill, r, None));
            gen_stmt(code, *then);

            if let Some(els) = els_may {
                let y = Some(*LABEL.lock().unwrap());
                *LABEL.lock().unwrap() += 1;
                code.push(IR::new(IROp::Jmp, y, None));
                code.push(IR::new(IROp::Label, x, None));
                gen_stmt(code, *els);
                code.push(IR::new(IROp::Label, y, None));
                return;
            } else {
                code.push(IR::new(IROp::Label, x, None));
                return;
            }
        }
        NodeType::For(init, cond, inc, body) => {
            let x = Some(*LABEL.lock().unwrap());
            *LABEL.lock().unwrap() += 1;
            let y = Some(*LABEL.lock().unwrap());
            *LABEL.lock().unwrap() += 1;

            let r1 = gen_expr(code, *init);
            code.push(IR::new(IROp::Kill, r1, None));
            code.push(IR::new(IROp::Label, x, None));
            let r2 = gen_expr(code, *cond);
            code.push(IR::new(IROp::Unless, r2, y));
            code.push(IR::new(IROp::Kill, r2, None));
            gen_stmt(code, *body);
            let r3 = gen_expr(code, *inc);
            code.push(IR::new(IROp::Kill, r3, None));
            code.push(IR::new(IROp::Jmp, x, None));
            code.push(IR::new(IROp::Label, y, None));
        }
        NodeType::Return(expr) => {
            let r = gen_expr(code, *expr);
            code.push(IR::new(IROp::Return, r, None));
            code.push(IR::new(IROp::Kill, r, None));
        }
        NodeType::ExprStmt(expr) => {
            let r = gen_expr(code, *expr);
            code.push(IR::new(IROp::Kill, r, None));
        }
        NodeType::CompStmt(stmts) => {
            for n in stmts {
                gen_stmt(code, n);
            }
        }
        e => panic!("unknown node: {:?}", e),
    }
}

fn gen_args(code: &mut Vec<IR>, nodes: Vec<Node>) {
    if nodes.len() == 0 {
        return;
    }

    code.push(IR::new(IROp::SaveArgs, Some(nodes.len()), None));

    for node in nodes {
        match node.ty {
            NodeType::Ident(name) => {
                *STACKSIZE.lock().unwrap() += 8;
                VARS.lock().unwrap().insert(
                    name.clone(),
                    *STACKSIZE.lock().unwrap(),
                );
            }
            _ => panic!("bad parameter"),
        }
    }
}

pub fn gen_ir(nodes: Vec<Node>) -> Vec<Function> {
    let mut v = vec![];
    for node in nodes {
        match node.ty {
            NodeType::Func(name, args, body) => {
                let mut code = vec![];
                *VARS.lock().unwrap() = HashMap::new();

                *REGNO.lock().unwrap() = 1;
                *STACKSIZE.lock().unwrap() = 0;

                gen_args(&mut code, args);
                gen_stmt(&mut code, *body);

                v.push(Function::new(name, code, *STACKSIZE.lock().unwrap()));
            }
            _ => panic!("parse error."),
        }
    }
    v
}
