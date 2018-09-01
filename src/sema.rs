use parse::{Node, NodeType, Type, Ctype};
use token::TokenType;
use util::size_of;

use std::sync::Mutex;
use std::collections::HashMap;
use std::mem;

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

fn swap(p: &mut Box<Node>, q: &mut Box<Node>) {
    mem::swap(p, q);
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

fn addr_of(base: Node, ty: Type) -> Node {
    let ty = Type::new(Ctype::Ptr(Box::new(ty)));
    let mut node = Node::new(NodeType::Addr(Box::new(base.clone())));
    node.ty = Box::new(ty);
    node
}

fn walk(mut node: Node, decay: bool) -> Node {
    use self::NodeType::*;
    let op = node.op.clone();
    match op {
        Num(_) => (),
        Ident(ref name) => {
            if let Some(var) = VARS.lock().unwrap().get(name) {
                node.op = NodeType::Lvar;
                node.offset = var.offset;
                if decay {
                    if let Ctype::Ary(ref ary_of, _) = var.ty.ty {
                        node = addr_of(node, *ary_of.clone());
                        return node;
                    }
                }
                node.ty = var.ty.clone();
            } else {
                panic!("undefined variable: {}", name);
            }
        }
        Vardef(name, init_may) => {
            *STACKSIZE.lock().unwrap() += size_of(&*node.ty);
            node.offset = *STACKSIZE.lock().unwrap();

            VARS.lock().unwrap().insert(
                name.clone(),
                Var::new(node.ty.clone(), *STACKSIZE.lock().unwrap()),
            );

            if let Some(mut init) = init_may {
                node.op = Vardef(name, Some(Box::new(walk(*init, true))));
            }
        }
        If(cond, then, els_may) => {
            let new_cond = Box::new(walk(*cond, true));
            let new_then = Box::new(walk(*then, true));
            let mut new_els = None;
            if let Some(mut els) = els_may {
                new_els = Some(Box::new(walk(*els, true)));
            }
            node.op = If(new_cond, new_then, new_els);
        }
        For(init, cond, inc, body) => {
            node.op = For(
                Box::new(walk(*init, true)),
                Box::new(walk(*cond, true)),
                Box::new(walk(*inc, true)),
                Box::new(walk(*body, true)),
            );
        }
        BinOp(token_type, mut lhs, mut rhs) => {
            match token_type {
                TokenType::Plus | TokenType::Minus => {
                    lhs = Box::new(walk(*lhs, true));
                    rhs = Box::new(walk(*rhs, true));

                    if matches!(rhs.ty.ty , Ctype::Ptr(_)) {
                        swap(&mut lhs, &mut rhs);
                    }
                    if matches!(rhs.ty.ty , Ctype::Ptr(_)) {
                        panic!("'pointer {:?} pointer' is not defined", node.op)
                    }

                    node.op = BinOp(token_type, lhs.clone(), rhs);
                    node.ty = lhs.ty;
                }
                TokenType::Equal => {
                    lhs = Box::new(walk(*lhs, false));
                    if !matches!(lhs.op, NodeType::Lvar) && !matches!(lhs.op, NodeType::Deref(_)) {
                        panic!("not an lvalue: {:?}", node.op);
                    }
                    node.op = BinOp(token_type, lhs.clone(), Box::new(walk(*rhs, true)));
                    node.ty = lhs.ty;
                }
                _ => {
                    lhs = Box::new(walk(*lhs, true));
                    node.op = BinOp(token_type, lhs.clone(), Box::new(walk(*rhs, true)));
                    node.ty = lhs.ty;
                }
            }
        }
        Logand(mut lhs, rhs) => {
            lhs = Box::new(walk(*lhs, true));
            node.op = Logand(lhs.clone(), Box::new(walk(*rhs, true)));
            node.ty = lhs.ty;
        }
        Logor(mut lhs, rhs) => {
            lhs = Box::new(walk(*lhs, true));
            node.op = Logor(lhs.clone(), Box::new(walk(*rhs, true)));
            node.ty = lhs.ty;
        }
        Addr(mut expr) => {
            expr = Box::new(walk(*expr, true));
            node.ty = Box::new(Type::new(Ctype::Ptr(expr.ty.clone())));
            node.op = Addr(expr);
        }
        Deref(mut expr) => {
            expr = Box::new(walk(*expr, true));
            match expr.ty.ty {
                Ctype::Ptr(ref ptr_of) => node.ty = ptr_of.clone(),
                _ => panic!("operand must be a pointer"),
            }
            node.op = Deref(expr);
        }
        Return(expr) => node.op = Return(Box::new(walk(*expr, true))),
        Sizeof(mut expr) => {
            expr = Box::new(walk(*expr, false));

            node.op = Num(size_of(&*expr.ty) as i32);
            node.ty.ty = Ctype::Int;
        }
        Call(name, args) => {
            let mut new_args = vec![];
            for arg in args {
                new_args.push(walk(arg, true));
            }
            node.op = Call(name, new_args);
        }
        Func(name, args, body) => {
            let mut new_args = vec![];
            for arg in args {
                new_args.push(walk(arg, true));
            }
            node.op = Func(name, new_args, Box::new(walk(*body, true)));
        }
        CompStmt(stmts) => {
            let mut new_stmts = vec![];
            for stmt in stmts {
                new_stmts.push(walk(stmt, true));
            }
            node.op = CompStmt(new_stmts);
        }
        ExprStmt(expr) => node.op = ExprStmt(Box::new(walk(*expr, true))),
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
        let mut new = walk(node, true);
        new.stacksize = *STACKSIZE.lock().unwrap();
        new_nodes.push(new);
    }
    new_nodes
}
