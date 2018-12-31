// Quoted from 9cc
// > 9cc's code generation is two-pass. In the first pass, abstract
// > syntax trees are compiled to IR (intermediate representation).
//
// > IR resembles the real x86-64 instruction set, but it has infinite
// > number of registers. We don't try too hard to reuse registers in
// > this pass. Instead, we "kill" registers to mark them as dead when
// > we are done with them and use new registers.
//
// > Such infinite number of registers are mapped to a finite registers
// > in a later pass.

use crate::parse::{Node, NodeType};
use crate::{Ctype, Scope, TokenType, Type};

use std::sync::Mutex;

lazy_static! {
    static ref NUM_REGS: Mutex<usize> = Mutex::new(0);
    static ref NLABEL: Mutex<usize> = Mutex::new(1);
    static ref RETURN_LABEL: Mutex<usize> = Mutex::new(0);
    static ref RETURN_REG: Mutex<usize> = Mutex::new(0);
    static ref BREAK_LABEL: Mutex<usize> = Mutex::new(0);
    static ref CODE: Mutex<Vec<IR>> = Mutex::new(vec![]);
}

fn add(op: IROp, lhs: Option<usize>, rhs: Option<usize>) {
    let ir = IR::new(op, lhs, rhs);
    CODE.lock().unwrap().push(ir.clone());
}

#[derive(Clone, Debug)]
pub enum IRType {
    Noarg,
    Reg,
    Imm,
    Mem,
    Jmp,
    Label,
    LabelAddr,
    RegReg,
    RegImm,
    StoreArg,
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
            name,
            ir,
            stacksize,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IROp {
    Add,
    AddImm,
    Sub,
    SubImm,
    Mul,
    MulImm,
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
    LE,
    LT,
    AND,
    OR,
    XOR,
    SHL,
    SHR,
    Mod,
    Neg,
    Jmp,
    If,
    Unless,
    Load(u8),
    Store(u8),
    StoreArg(u8),
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
            TokenType::LeftAngleBracket | TokenType::RightAngleBracket => IROp::LT,
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
        Self { op, lhs, rhs }
    }
}

fn kill(r: Option<usize>) {
    add(IROp::Kill, r, None);
}

fn label(x: Option<usize>) {
    add(IROp::Label, x, None);
}

fn jmp(x: Option<usize>) {
    add(IROp::Jmp, x, None);
}

fn load(ty: &Type, dst: Option<usize>, src: Option<usize>) {
    add(IROp::Load(ty.size as u8), dst, src);
}

fn store(ty: &Type, dst: Option<usize>, src: Option<usize>) {
    add(IROp::Store(ty.size as u8), dst, src);
}

fn store_arg(ty: &Type, bpoff: Option<usize>, argreg: Option<usize>) {
    add(IROp::StoreArg(ty.size as u8), bpoff, argreg);
}

// Quoted from 9cc
// > In C, all expressions that can be written on the left-hand side of
// > the '=' operator must have an address in memory. In other words, if
// > you can apply the '&' operator to take an address of some
// > expression E, you can assign E to a new value.
//
// > Other expressions, such as `1+2`, cannot be written on the lhs of
// > '=', since they are just temporary values that don't have an address.
//
// > The stuff that can be written on the lhs of '=' is called lvalue.
// > Other values are called rvalue. An lvalue is essentially an address.
//
// > When lvalues appear on the rvalue context, they are converted to
// > rvalues by loading their values from their addresses. You can think
// > '&' as an operator that suppresses such automatic lvalue-to-rvalue
// > conversion.
//
// > This function evaluates a given node as an lvalue.

fn gen_lval(node: Box<Node>) -> Option<usize> {
    match node.op {
        NodeType::Deref(expr) => gen_expr(expr),
        NodeType::Dot(ref expr, _, ref offset) => {
            let r = gen_lval(expr.clone());
            add(IROp::AddImm, r, Some(*offset));
            r
        }
        NodeType::Lvar(Scope::Local(offset)) => {
            let r = Some(*NUM_REGS.lock().unwrap());
            *NUM_REGS.lock().unwrap() += 1;
            add(IROp::Bprel, r, Some(offset));
            r
        }
        NodeType::Gvar(name, _, _) => {
            let r = Some(*NUM_REGS.lock().unwrap());
            *NUM_REGS.lock().unwrap() += 1;
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
    r1
}

fn get_inc_scale(ty: &Type) -> usize {
    match ty.ty {
        Ctype::Ptr(ref ptr_to) => ptr_to.size,
        _ => 1,
    }
}

fn gen_pre_inc(ty: &Type, expr: Box<Node>, num: i32) -> i32 {
    let addr = gen_lval(expr);
    let val = *NUM_REGS.lock().unwrap();
    *NUM_REGS.lock().unwrap() += 1;
    load(ty, Some(val), addr);
    add(
        IROp::AddImm,
        Some(val),
        Some(num as usize * get_inc_scale(ty)),
    );
    store(ty, addr, Some(val));
    kill(addr);
    val as i32
}

fn gen_post_inc(ty: &Type, expr: Box<Node>, num: i32) -> i32 {
    let val = gen_pre_inc(ty, expr, num);
    add(
        IROp::SubImm,
        Some(val as usize),
        Some(num as usize * get_inc_scale(ty)),
    );
    val as i32
}

fn to_assign_op(op: &TokenType) -> IROp {
    use self::TokenType::*;
    match op {
        MulEQ => IROp::Mul,
        DivEQ => IROp::Div,
        ModEQ => IROp::Mod,
        AddEQ => IROp::Add,
        SubEQ => IROp::Sub,
        ShlEQ => IROp::SHL,
        ShrEQ => IROp::SHR,
        BitandEQ => IROp::AND,
        XorEQ => IROp::XOR,
        BitorEQ => IROp::OR,
        e => panic!("unexpected op: {:?}", e),
    }
}

fn gen_assign_op(op: &TokenType, ty: &Type, lhs: Box<Node>, rhs: Box<Node>) -> Option<usize> {
    let src = gen_expr(rhs);
    let dst = gen_lval(lhs);
    let val = Some(*NUM_REGS.lock().unwrap());
    *NUM_REGS.lock().unwrap() += 1;

    load(ty, val, dst);
    add(to_assign_op(op), val, src);
    kill(src);
    store(ty, dst, val);
    kill(dst);
    val
}

fn gen_expr(node: Box<Node>) -> Option<usize> {
    let node = *node;
    match node.op {
        NodeType::Num(val) => {
            let r = Some(*NUM_REGS.lock().unwrap());
            *NUM_REGS.lock().unwrap() += 1;
            add(IROp::Imm, r, Some(val as usize));
            r
        }
        NodeType::Lvar(_) | NodeType::Dot(_, _, _) | NodeType::Gvar(_, _, _) => {
            let r = gen_lval(Box::new(node.clone()));
            load(&node.ty, r, r);
            r
        }
        NodeType::Call(name, args) => {
            let mut args_ir: [usize; 6] = [0; 6];
            for i in 0..args.len() {
                args_ir[i] = gen_expr(Box::new(args[i].clone())).unwrap();
            }

            let r = Some(*NUM_REGS.lock().unwrap());
            *NUM_REGS.lock().unwrap() += 1;

            add(IROp::Call(name, args.len(), args_ir), r, None);

            for arg in args_ir.iter().take(args.len()) {
                kill(Some(*arg));
            }
            r
        }
        NodeType::Addr(expr) => gen_lval(expr),
        NodeType::Deref(expr) => {
            let r = gen_expr(expr);
            load(&node.ty, r, r);
            r
        }
        NodeType::StmtExpr(body) => {
            let orig_label = *RETURN_LABEL.lock().unwrap();
            let orig_reg = *RETURN_REG.lock().unwrap();
            *RETURN_LABEL.lock().unwrap() = *NLABEL.lock().unwrap();
            *NLABEL.lock().unwrap() += 1;
            let r = *NUM_REGS.lock().unwrap();
            *NUM_REGS.lock().unwrap() += 1;
            *RETURN_REG.lock().unwrap() = r;

            gen_stmt(*body);
            label(Some(*RETURN_LABEL.lock().unwrap()));

            *RETURN_LABEL.lock().unwrap() = orig_label;
            *RETURN_REG.lock().unwrap() = orig_reg;
            Some(r)
        }
        NodeType::BinOp(op, lhs, rhs) => {
            use self::TokenType::*;
            match op {
                Equal => {
                    let rhs = gen_expr(rhs);
                    let lhs = gen_lval(lhs);
                    store(&node.ty, lhs, rhs);
                    kill(lhs);
                    rhs
                }
                Plus => gen_binop(IROp::Add, lhs, rhs),
                Minus => gen_binop(IROp::Sub, lhs, rhs),
                Logand => {
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
                    r1
                }
                Logor => {
                    let x = Some(*NLABEL.lock().unwrap());
                    *NLABEL.lock().unwrap() += 1;
                    let y = Some(*NLABEL.lock().unwrap());
                    *NLABEL.lock().unwrap() += 1;

                    let r1 = gen_expr(lhs);
                    add(IROp::Unless, r1, x);
                    add(IROp::Imm, r1, Some(1));
                    jmp(y);
                    label(x);

                    let r2 = gen_expr(rhs);
                    add(IROp::Mov, r1, r2);
                    kill(r2);
                    add(IROp::Unless, r1, y);
                    add(IROp::Imm, r1, Some(1));
                    label(y);
                    r1
                }
                MulEQ | DivEQ | ModEQ | AddEQ | SubEQ | ShlEQ | ShrEQ | BitandEQ | XorEQ
                | BitorEQ => gen_assign_op(&op, &node.ty, lhs, rhs),
                EQ => gen_binop(IROp::EQ, lhs, rhs),
                NE => gen_binop(IROp::NE, lhs, rhs),
                LE => gen_binop(IROp::LE, lhs, rhs),
                And => gen_binop(IROp::AND, lhs, rhs),
                VerticalBar => gen_binop(IROp::OR, lhs, rhs),
                Hat => gen_binop(IROp::XOR, lhs, rhs),
                SHL => gen_binop(IROp::SHL, lhs, rhs),
                SHR => gen_binop(IROp::SHR, lhs, rhs),
                Mod => gen_binop(IROp::Mod, lhs, rhs),
                Comma => {
                    kill(gen_expr(lhs));
                    gen_expr(rhs)
                }
                _ => gen_binop(IROp::from(op), lhs, rhs),
            }
        }
        NodeType::Neg(expr) => {
            let r = gen_expr(expr);
            add(IROp::Neg, r, None);
            r
        }
        NodeType::PostInc(expr) => Some(gen_post_inc(&node.ty, expr, 1) as usize),
        NodeType::PostDec(expr) => Some(gen_post_inc(&node.ty, expr, -1) as usize),
        NodeType::Ternary(cond, then, els) => {
            //      cond then els  then
            // return 1 ? 3 : 5; => 3
            let x = Some(*NLABEL.lock().unwrap());
            *NLABEL.lock().unwrap() += 1;
            let y = Some(*NLABEL.lock().unwrap());
            *NLABEL.lock().unwrap() += 1;
            let r = gen_expr(cond);

            add(IROp::Unless, r, x);
            let r2 = gen_expr(then);
            add(IROp::Mov, r, r2);
            kill(r2);
            jmp(y);

            label(x);
            let r3 = gen_expr(els);
            add(IROp::Mov, r, r3);
            kill(r3);
            label(y);
            r
        }
        NodeType::Exclamation(expr) => {
            let lhs = gen_expr(expr);
            let rhs = Some(*NUM_REGS.lock().unwrap());
            *NUM_REGS.lock().unwrap() += 1;
            add(IROp::Imm, rhs, Some(0));
            add(IROp::EQ, lhs, rhs);
            kill(rhs);
            lhs
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
                let lhs = Some(*NUM_REGS.lock().unwrap());
                *NUM_REGS.lock().unwrap() += 1;
                add(IROp::Bprel, lhs, Some(offset));
                store(&node.ty, lhs, rhs);
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
                jmp(y);
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
            let orig = *BREAK_LABEL.lock().unwrap();
            *BREAK_LABEL.lock().unwrap() = *NLABEL.lock().unwrap();
            *NLABEL.lock().unwrap() += 1;

            gen_stmt(*init);
            label(x);
            if !cond.is_null() {
                let r2 = gen_expr(cond);
                add(IROp::Unless, r2, y);
                kill(r2);
            }
            gen_stmt(*body);
            if !inc.is_null() {
                gen_stmt(*inc);
            }
            jmp(x);
            label(y);
            label(Some(*BREAK_LABEL.lock().unwrap()));
            *BREAK_LABEL.lock().unwrap() = orig;
        }
        NodeType::DoWhile(body, cond) => {
            let x = Some(*NLABEL.lock().unwrap());
            *NLABEL.lock().unwrap() += 1;
            let orig = *BREAK_LABEL.lock().unwrap();
            *BREAK_LABEL.lock().unwrap() = *NLABEL.lock().unwrap();
            *NLABEL.lock().unwrap() += 1;
            label(x);
            gen_stmt(*body);
            let r = gen_expr(cond);
            add(IROp::If, r, x);
            kill(r);
            label(Some(*BREAK_LABEL.lock().unwrap()));
            *BREAK_LABEL.lock().unwrap() = orig;
        }
        NodeType::Break => {
            let break_label = *BREAK_LABEL.lock().unwrap();
            if break_label == 0 {
                panic!("stray 'break' statement");
            }
            jmp(Some(break_label));
        }
        NodeType::Return(expr) => {
            let r = gen_expr(expr);

            // Statement expression (GNU extension)
            if *RETURN_LABEL.lock().unwrap() != 0 {
                add(IROp::Mov, Some(*RETURN_REG.lock().unwrap()), r);
                kill(r);
                jmp(Some(*RETURN_LABEL.lock().unwrap()));
                return;
            }

            add(IROp::Return, r, None);
            kill(r);
        }
        NodeType::ExprStmt(expr) => {
            let r = gen_expr(expr);
            kill(r);
        }
        NodeType::VecStmt(stmts) | NodeType::CompStmt(stmts) => {
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
                // *NUM_REGS.lock().unwrap() = 0;

                for (i, arg) in args.iter().enumerate() {
                    if let NodeType::Vardef(_, _, Scope::Local(offset)) = arg.op {
                        store_arg(&arg.ty, Some(offset), Some(i));
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
