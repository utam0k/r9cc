use parse::{Node, NodeType};
use token::TokenType;

#[derive(Debug, Clone)]
pub enum IRType {
    IMM,
    MOV,
    RETURN,
    KILL,
    NOP,
    ADD,
    SUB,
    MUL,
    DIV,
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
            TokenType::Plus => IRType::ADD,
            TokenType::Minus => IRType::SUB,
            TokenType::Mul => IRType::MUL,
            TokenType::Div => IRType::DIV,
            e => panic!("cannot convert: {:?}", e),
        }
    }
}

#[derive(Debug, Clone)]
pub struct IR {
    pub op: IRType,
    pub lhs: usize,
    pub rhs: usize,
}

impl IR {
    fn new(op: IRType, lhs: usize, rhs: usize) -> Self {
        Self {
            op: op,
            lhs: lhs,
            rhs: rhs,
        }
    }
}

fn gen_expr(code: &mut Vec<IR>, node: Node) -> usize {
    match node.ty {
        NodeType::Num(val) => {
            let r = code.len();
            code.push(IR::new(IRType::IMM, r, val as usize));
            return r;
        }
        NodeType::BinOp(op, lhs, rhs) => {

            let lhs = gen_expr(code, *lhs);
            let rhs = gen_expr(code, *rhs);

            code.push(IR::new(IRType::from(op), lhs, rhs));
            code.push(IR::new(IRType::KILL, rhs, 0));
            return lhs;
        }
        _ => unreachable!(),
    }
}

fn gen_stmt(code: &mut Vec<IR>, node: Node) {
    match node.ty {
        NodeType::Return(expr) => {
            let r = gen_expr(code, *expr);
            code.push(IR::new(IRType::RETURN, r, 0));
            code.push(IR::new(IRType::KILL, r, 0));
            return;
        }
        NodeType::ExprStmt(expr) => {
            let r = gen_expr(code, *expr);
            code.push(IR::new(IRType::KILL, r, 0));
            return;
        }
        NodeType::CompStmt(stmts) => {
            for n in stmts {
                gen_stmt(code, n);
            }
            return;
        }
        e => panic!("unknown node: {:?}", e),
    }
}

pub fn gen_ir(node: Node) -> Vec<IR> {
    let mut code = vec![];
    gen_stmt(&mut code, node);
    code
}
