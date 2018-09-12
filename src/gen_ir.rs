// Compile AST to intermediate code that has infinite number of registers.
// Base pointer is always assigned to r0.

use parse::{Node, NodeType, Ctype, Type};
use token::TokenType;
use util::size_of;
use sema::Scope;

use std::sync::Mutex;

lazy_static!{
    static ref NREG: Mutex<usize> = Mutex::new(1);
    static ref NLABEL: Mutex<usize> = Mutex::new(1);
    static ref RETURN_LABEL: Mutex<usize> = Mutex::new(0);
    static ref RETURN_REG: Mutex<usize> = Mutex::new(0);
    static ref CODE: Mutex<Vec<IR>> = Mutex::new(vec![]);
}

fn add(op: IROp, lhs: Option<usize>, rhs: Option<usize>) {
    let ir = IR::new(op, lhs, rhs);
    CODE.lock().unwrap().push(ir)
}

#[derive(Clone, Debug)]
pub enum IRType {
    Noarg,
    Reg,
    Imm,
    Jmp,
    Label,
    LabelAddr,
    RegReg,
    RegImm,
    ImmImm,
    RegLabel,
    Call,
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
    Bprel,
    Mov,
    Return,
    Call(String, usize, [usize; 6]),
    Label,
    LabelAddr(String),
    EQ,
    NE,
    LT,
    Jmp,
    If,
    Unless,
    Load8,
    Load32,
    Load64,
    Store8,
    Store32,
    Store64,
    Store8Arg,
    Store32Arg,
    Store64Arg,
    Kill,
    Nop,
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

fn kill(r: Option<usize>) {
    add(IROp::Kill, r, None);
}

fn label(x: Option<usize>) {
    add(IROp::Label, x, None);
}

fn choose_insn(ty: Box<&Type>, op8: IROp, op32: IROp, op64: IROp) -> IROp {
    match size_of(ty) {
        1 => op8,
        4 => op32,
        8 => op64,
        _ => unreachable!(),
    }
}

fn load_insn(node: Box<&Type>) -> IROp {
    use self::IROp::*;
    choose_insn(node, Load8, Load32, Load64)
}

fn store_insn(node: Box<&Type>) -> IROp {
    use self::IROp::*;
    choose_insn(node, Store8, Store32, Store64)
}

fn store_arg_insn(node: Box<&Type>) -> IROp {
    use self::IROp::*;
    choose_insn(node, Store8Arg, Store32Arg, Store64Arg)
}

// In C, all expressions that can be written on the left-hand side of
// the '=' operator must have an address in memory. In other words, if
// you can apply the '&' operator to take an address of some
// expression E, you can assign E to a new value.
//
// Other expressions, such as `1+2`, cannot be written on the lhs of
// '=', since they are just temporary values that don't have an address.
//
// The stuff that can be written on the lhs of '=' is called lvalue.
// Other values are called rvalue. An lvalue is essentially an address.
//
// When lvalues appear on the rvalue context, they are converted to
// rvalues by loading their values from their addresses. You can think
// '&' as an operator that suppresses such automatic lvalue-to-rvalue
// conversion.
//
// This function evaluates a given node as an lvalue.
fn gen_lval(node: Box<Node>) -> Option<usize> {
    match node.op {
        NodeType::Deref(expr) => gen_expr(expr),
        NodeType::Lvar(Scope::Local(offset)) => {
            let r = Some(*NREG.lock().unwrap());
            *NREG.lock().unwrap() += 1;
            add(IROp::Bprel, r, Some(offset));
            r
        }
        NodeType::Gvar(name, _, _) => {
            let r = Some(*NREG.lock().unwrap());
            *NREG.lock().unwrap() += 1;
            add(IROp::LabelAddr(name), r, None);
            r
        }
        _ => unreachable!(),
    }
}

fn gen_binop(ty: IROp, lhs: Box<Node>, rhs: Box<Node>) -> Option<usize> {
    let r1 = gen_expr(lhs);
    let r2 = gen_expr(rhs);

    add(ty, r1, r2);
    kill(r2);
    return r1;
}

fn gen_expr(node: Box<Node>) -> Option<usize> {
    let node = *node;
    match node.op {
        NodeType::Num(val) => {
            let r = Some(*NREG.lock().unwrap());
            *NREG.lock().unwrap() += 1;
            add(IROp::Imm, r, Some(val as usize));
            return r;
        }
        NodeType::Logand(lhs, rhs) => {
            let x = Some(*NLABEL.lock().unwrap());
            *NLABEL.lock().unwrap() += 1;

            let r1 = gen_expr(lhs);
            add(IROp::Unless, r1, x);
            let r2 = gen_expr(rhs);
            add(IROp::Mov, r1, r2);
            kill(r2);
            add(IROp::Unless, r1, x);
            add(IROp::Imm, r1, Some(1));
            label(x);
            return r1;
        }
        NodeType::Logor(lhs, rhs) => {
            let x = Some(*NLABEL.lock().unwrap());
            *NLABEL.lock().unwrap() += 1;
            let y = Some(*NLABEL.lock().unwrap());
            *NLABEL.lock().unwrap() += 1;

            let r1 = gen_expr(lhs);
            add(IROp::Unless, r1, x);
            add(IROp::Imm, r1, Some(1));
            add(IROp::Jmp, y, None);
            label(x);

            let r2 = gen_expr(rhs);
            add(IROp::Mov, r1, r2);
            kill(r2);
            add(IROp::Unless, r1, y);
            add(IROp::Imm, r1, Some(1));
            label(y);
            return r1;
        }
        NodeType::Lvar(_) |
        NodeType::Gvar(_, _, _) => {
            let op = load_insn(Box::new(&node.ty));
            let r = gen_lval(Box::new(node));
            add(op, r, r);
            return r;
        }
        NodeType::Call(name, args) => {
            let mut args_ir: [usize; 6] = [0; 6];
            for i in 0..args.len() {
                args_ir[i] = gen_expr(Box::new(args[i].clone())).unwrap();
            }

            let r = Some(*NREG.lock().unwrap());
            *NREG.lock().unwrap() += 1;

            add(IROp::Call(name, args.len(), args_ir), r, None);

            for i in 0..args.len() {
                kill(Some(args_ir[i]));
            }
            return r;
        }
        NodeType::Addr(expr) => gen_lval(expr),
        NodeType::Deref(expr) => {
            let op = load_insn(Box::new(&node.ty));
            let r = gen_expr(expr);
            add(op, r, r);
            return r;
        }
        NodeType::StmtExpr(body) => {
            let orig_label = *RETURN_LABEL.lock().unwrap();
            let orig_reg = *RETURN_REG.lock().unwrap();
            *RETURN_LABEL.lock().unwrap() = *NLABEL.lock().unwrap();
            *NLABEL.lock().unwrap() += 1;
            let r = *NREG.lock().unwrap();
            *NREG.lock().unwrap() += 1;
            *RETURN_REG.lock().unwrap() = r;

            gen_stmt(*body);
            label(Some(*RETURN_LABEL.lock().unwrap()));

            *RETURN_LABEL.lock().unwrap() = orig_label;
            *RETURN_REG.lock().unwrap() = orig_reg;
            return Some(r);
        }
        NodeType::BinOp(op, lhs, rhs) => {
            match op {
                TokenType::Equal => {
                    let rhs = gen_expr(rhs);
                    let lhs = gen_lval(lhs);
                    add(store_insn(Box::new(&node.ty)), lhs, rhs);
                    kill(rhs);
                    return lhs;
                }
                TokenType::Plus | TokenType::Minus => {
                    let insn = IROp::from(op);
                    if let Ctype::Ptr(ref ptr_to) = lhs.ty.ty.clone() {
                        let rhs = gen_expr(rhs);
                        let r = Some(*NREG.lock().unwrap());
                        *NREG.lock().unwrap() += 1;
                        add(IROp::Imm, r, Some(size_of(Box::new(ptr_to))));
                        add(IROp::Mul, rhs, r);
                        kill(r);

                        let lhs = gen_expr(lhs);
                        add(insn, lhs, rhs);
                        kill(rhs);
                        lhs
                    } else {
                        gen_binop(insn, lhs, rhs)
                    }
                }
                TokenType::EQ => gen_binop(IROp::EQ, lhs, rhs),
                TokenType::NE => gen_binop(IROp::NE, lhs, rhs),
                _ => gen_binop(IROp::from(op), lhs, rhs),
            }
        }
        e => unreachable!("{:?}", e),
    }
}

fn gen_stmt(node: Node) {
    match node.op {
        NodeType::Null => return,
        NodeType::Vardef(_, init_may, Scope::Local(offset)) => {
            if let Some(init) = init_may {
                let rhs = gen_expr(init);
                let lhs = Some(*NREG.lock().unwrap());
                *NREG.lock().unwrap() += 1;
                add(IROp::Bprel, lhs, Some(offset));
                add(store_insn(Box::new(&node.ty)), lhs, rhs);
                kill(lhs);
                kill(rhs);
            }
            return;
        }
        NodeType::If(cond, then, els_may) => {
            if let Some(els) = els_may {
                let x = Some(*NLABEL.lock().unwrap());
                *NLABEL.lock().unwrap() += 1;
                let y = Some(*NLABEL.lock().unwrap());
                *NLABEL.lock().unwrap() += 1;
                let r = gen_expr(cond.clone());
                add(IROp::Unless, r, x);
                kill(r);
                gen_stmt(*then.clone());
                add(IROp::Jmp, y, None);
                label(x);
                gen_stmt(*els);
                label(y);
                return;
            }

            let x = Some(*NLABEL.lock().unwrap());
            *NLABEL.lock().unwrap() += 1;
            let r = gen_expr(cond);
            add(IROp::Unless, r, x);
            kill(r);
            gen_stmt(*then);
            label(x);
        }
        NodeType::For(init, cond, inc, body) => {
            let x = Some(*NLABEL.lock().unwrap());
            *NLABEL.lock().unwrap() += 1;
            let y = Some(*NLABEL.lock().unwrap());
            *NLABEL.lock().unwrap() += 1;

            gen_stmt(*init);
            label(x);
            let r2 = gen_expr(cond);
            add(IROp::Unless, r2, y);
            kill(r2);
            gen_stmt(*body);
            gen_stmt(*inc);
            add(IROp::Jmp, x, None);
            label(y);
        }
        NodeType::DoWhile(body, cond) => {
            let x = Some(*NLABEL.lock().unwrap());
            *NLABEL.lock().unwrap() += 1;
            label(x);
            gen_stmt(*body);
            let r = gen_expr(cond);
            add(IROp::If, r, x);
            kill(r);
        }
        NodeType::Return(expr) => {
            let r = gen_expr(expr);

            // Statement expression (GNU extension)
            if *RETURN_LABEL.lock().unwrap() != 0 {
                add(IROp::Mov, Some(*RETURN_REG.lock().unwrap()), r);
                kill(r);
                add(IROp::Jmp, Some(*RETURN_LABEL.lock().unwrap()), None);
                return;
            }

            add(IROp::Return, r, None);
            kill(r);
        }
        NodeType::ExprStmt(expr) => {
            let r = gen_expr(expr);
            kill(r);
        }
        NodeType::CompStmt(stmts) => {
            for n in stmts {
                gen_stmt(n);
            }
        }
        e => panic!("unknown node: {:?}", e),
    }
}

pub fn gen_ir(nodes: Vec<Node>) -> Vec<Function> {
    let mut v = vec![];
    for node in nodes {
        match node.op {
            NodeType::Func(name, args, body, stacksize) => {
                *CODE.lock().unwrap() = vec![];
                *NREG.lock().unwrap() = 1;

                for i in 0..args.len() {
                    let arg = &args[i];
                    let op = store_arg_insn(Box::new(&arg.ty));
                    if let NodeType::Vardef(_, _, Scope::Local(offset)) = arg.op {
                        add(op, Some(offset), Some(i));
                    } else {
                        unreachable!();
                    }
                }
                gen_stmt(*body);

                v.push(Function::new(name, CODE.lock().unwrap().clone(), stacksize));
            }
            NodeType::Vardef(_, _, _) => (),
            _ => panic!("parse error."),
        }
    }
    v
}
