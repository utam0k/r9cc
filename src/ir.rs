use parse::{Node, NodeType};
use token::TokenType;

use std::sync::Mutex;
use std::collections::HashMap;

lazy_static!{
    static ref VARS: Mutex<HashMap<String, usize>> = Mutex::new(HashMap::new());
    static ref REGNO: Mutex<usize> = Mutex::new(1);
    static ref BASE_REG: Mutex<usize> = Mutex::new(0);
    static ref BPOFF: Mutex<usize> = Mutex::new(0);
}

#[derive(Debug, Clone)]
pub enum IRType {
    Imm,
    Mov,
    Add,
    AddImm,
    Sub,
    Mul,
    Div,
    Return,
    Alloca,
    Load,
    Store,
    Kill,
    Nop,
}

impl From<NodeType> for IRType {
    fn from(node_type: NodeType) -> Self {
        match node_type {
            NodeType::BinOp(op, _, _) => Self::from(op),
            e => panic!("cannot convert: {:?}", e),
        }
    }
}

impl From<TokenType> for IRType {
    fn from(token_type: TokenType) -> Self {
        match token_type {
            TokenType::Plus => IRType::Add,
            TokenType::Minus => IRType::Sub,
            TokenType::Mul => IRType::Mul,
            TokenType::Div => IRType::Div,
            e => panic!("cannot convert: {:?}", e),
        }
    }
}

#[derive(Debug, Clone)]
pub struct IR {
    pub op: IRType,
    pub lhs: Option<usize>,
    pub rhs: Option<usize>,
}

impl IR {
    fn new(op: IRType, lhs: Option<usize>, rhs: Option<usize>) -> Self {
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
                    *BPOFF.lock().unwrap(),
                );
                *BPOFF.lock().unwrap() += 8;
            }
            let r = Some(*REGNO.lock().unwrap());
            *REGNO.lock().unwrap() += 1;
            let off = *VARS.lock().unwrap().get(&name).unwrap();
            code.push(IR::new(IRType::Mov, r, Some(*BASE_REG.lock().unwrap())));
            code.push(IR::new(IRType::AddImm, r, Some(off)));
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
            code.push(IR::new(IRType::Imm, r, Some(val as usize)));
            return r;
        }
        NodeType::Ident(_) => {
            let r = gen_lval(code, node);
            code.push(IR::new(IRType::Load, r, r));
            return r;
        }
        NodeType::BinOp(op, lhs, rhs) => {
            match op {
                TokenType::Equal => {
                    let rhs = gen_expr(code, *rhs);
                    let lhs = gen_lval(code, *lhs);
                    code.push(IR::new(IRType::Store, lhs, rhs));
                    code.push(IR::new(IRType::Kill, rhs, None));
                    return lhs;
                }
                _ => {
                    let lhs = gen_expr(code, *lhs);
                    let rhs = gen_expr(code, *rhs);

                    code.push(IR::new(IRType::from(op), lhs, rhs));
                    code.push(IR::new(IRType::Kill, rhs, None));
                    return lhs;
                }
            }
        }
        _ => unreachable!(),
    }
}

fn gen_stmt(code: &mut Vec<IR>, node: Node) {
    match node.ty {
        NodeType::Return(expr) => {
            let r = gen_expr(code, *expr);
            code.push(IR::new(IRType::Return, r, None));
            code.push(IR::new(IRType::Kill, r, None));
        }
        NodeType::ExprStmt(expr) => {
            let r = gen_expr(code, *expr);
            code.push(IR::new(IRType::Kill, r, None));
        }
        NodeType::CompStmt(stmts) => {
            for n in stmts {
                gen_stmt(code, n);
            }
        }
        e => panic!("unknown node: {:?}", e),
    }
}

pub fn gen_ir(node: Node) -> Vec<IR> {
    let mut code = vec![];

    code.push(IR::new(
        IRType::Alloca,
        Some(*BASE_REG.lock().unwrap()),
        None,
    ));
    gen_stmt(&mut code, node);
    code[0].rhs = Some(*BPOFF.lock().unwrap());
    code.push(IR::new(IRType::Kill, Some(*BASE_REG.lock().unwrap()), None));
    code
}
