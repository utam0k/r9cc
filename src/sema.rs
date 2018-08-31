use parse::{Node, NodeType, Type};
use token::TokenType;

use std::sync::Mutex;
use std::collections::HashMap;

macro_rules! matches(
    ($e:expr, $p:pat) => (
        match $e {
            $p => true,
            _ => false
        }
    )
);

lazy_static!{
    static ref STACKSIZE: Mutex<usize> = Mutex::new(0);
    static ref VARS: Mutex<HashMap<String, Var>> = Mutex::new(HashMap::new());
}

pub struct Var {
    ty: Box<Type>,
    offset: usize,
}

impl Var {
    fn new(ty: Box<Type>, offset: usize) -> Self {
        Var {
            ty: ty,
            offset: offset,
        }
    }
}

fn walk(mut node: Node) -> Node {
    use self::NodeType::*;
    let op = node.op.clone();
    match op {
        Num(_) => return node,
        Ident(ref name) => {
            if let Some(var) = VARS.lock().unwrap().get(name) {
                node.op = NodeType::Lvar;
                node.ty = var.ty.clone();
                node.offset = var.offset;
            } else {
                panic!("undefined variable: {}", name);
            }
        }
        Vardef(name, init_may) => {
            *STACKSIZE.lock().unwrap() += 8;
            node.offset = *STACKSIZE.lock().unwrap();

            VARS.lock().unwrap().insert(
                name.clone(),
                Var::new(node.ty.clone(), *STACKSIZE.lock().unwrap()),
            );

            if let Some(mut init) = init_may {
                node.op = Vardef(name, Some(Box::new(walk(*init))));
            }
        }
        If(cond, then, els_may) => {
            let new_cond = Box::new(walk(*cond));
            let new_then = Box::new(walk(*then));
            let mut new_els = None;
            if let Some(mut els) = els_may {
                new_els = Some(Box::new(walk(*els)));
            }
            node.op = If(new_cond, new_then, new_els);
        }
        For(init, cond, inc, body) => {
            node.op = For(
                Box::new(walk(*init)),
                Box::new(walk(*cond)),
                Box::new(walk(*inc)),
                Box::new(walk(*body)),
            );
        }
        BinOp(token_type, mut lhs, rhs) => {
            match token_type {
                TokenType::Plus => {
                    lhs = Box::new(walk(*lhs));
                    node.op = BinOp(token_type, lhs.clone(), Box::new(walk(*rhs)));
                    node.ty = lhs.ty;
                }
                _ => {
                    lhs = Box::new(walk(*lhs));
                    node.op = BinOp(token_type, lhs.clone(), Box::new(walk(*rhs)));
                    node.ty = lhs.ty;
                }
            }
        }
        Logand(mut lhs, rhs) => {
            lhs = Box::new(walk(*lhs));
            node.op = Logand(lhs.clone(), Box::new(walk(*rhs)));
            node.ty = lhs.ty;
        }
        Logor(mut lhs, rhs) => {
            lhs = Box::new(walk(*lhs));
            node.op = Logor(lhs.clone(), Box::new(walk(*rhs)));
            node.ty = lhs.ty;
        }
        Deref(expr) => {
            node.op = Deref(Box::new(walk(*expr)));
        }
        Return(expr) => {
            node.op = Return(Box::new(walk(*expr)));
        }
        Call(name, args) => {
            let mut new_args = vec![];
            for arg in args {
                new_args.push(walk(arg));
            }
            node.op = Call(name, new_args);
        }
        Func(name, args, body) => {
            let mut new_args = vec![];
            for arg in args {
                new_args.push(walk(arg));
            }
            node.op = Func(name, new_args, Box::new(walk(*body)));
        }
        CompStmt(stmts) => {
            let mut new_stmts = vec![];
            for stmt in stmts {
                new_stmts.push(walk(stmt));
            }
            node.op = CompStmt(new_stmts);
        }
        ExprStmt(expr) => node.op = ExprStmt(Box::new(walk(*expr))),
        _ => panic!("unknown node type"),
    };
    node
}

pub fn sema(nodes: Vec<Node>) -> Vec<Node> {
    let mut new_nodes = vec![];
    for mut node in nodes {
        assert!(matches!(node.op, NodeType::Func(_, _, _)));

        *VARS.lock().unwrap() = HashMap::new();
        *STACKSIZE.lock().unwrap() = 0;
        let new = walk(node);
        node.stacksize = *STACKSIZE.lock().unwrap();
        new_nodes.push(new);
    }
    new_nodes
}
