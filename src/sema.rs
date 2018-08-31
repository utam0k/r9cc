use parse::{Node, NodeType};

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
    static ref VARS: Mutex<HashMap<String, usize>> = Mutex::new(HashMap::new());
}

fn walk(mut node: Node) -> Node {
    use self::NodeType::*;
    let ty = node.ty.clone();
    match ty {
        Num(_) => return node,
        Ident(ref name) => {
            if let Some(offset) = VARS.lock().unwrap().get(name) {
                node.ty = NodeType::Lvar;
                node.offset = offset.clone();
            } else {
                panic!("undefined variable: {}", name);
            }
        }
        Vardef(name, init_may) => {
            *STACKSIZE.lock().unwrap() += 8;
            VARS.lock().unwrap().insert(
                name.clone(),
                *STACKSIZE.lock().unwrap(),
            );
            node.offset = *STACKSIZE.lock().unwrap();
            if let Some(mut init) = init_may {
                node.ty = Vardef(name, Some(Box::new(walk(*init))));
            }
        }
        If(cond, then, els_may) => {
            let new_cond = Box::new(walk(*cond));
            let new_then = Box::new(walk(*then));
            let mut new_els = None;
            if let Some(mut els) = els_may {
                new_els = Some(Box::new(walk(*els)));
            }
            node.ty = If(new_cond, new_then, new_els);
        }
        For(init, cond, inc, body) => {
            node.ty = For(
                Box::new(walk(*init)),
                Box::new(walk(*cond)),
                Box::new(walk(*inc)),
                Box::new(walk(*body)),
            );
        }
        BinOp(token_type, lhs, rhs) => {
            node.ty = BinOp(token_type, Box::new(walk(*lhs)), Box::new(walk(*rhs)));
        }
        Logand(lhs, rhs) => node.ty = Logand(Box::new(walk(*lhs)), Box::new(walk(*rhs))),
        Logor(lhs, rhs) => node.ty = Logor(Box::new(walk(*lhs)), Box::new(walk(*rhs))),
        Return(expr) => {
            node.ty = Return(Box::new(walk(*expr)));
        }
        Call(name, args) => {
            let mut new_args = vec![];
            for arg in args {
                new_args.push(walk(arg));
            }
            node.ty = Call(name, new_args);
        }
        Func(name, args, body) => {
            let mut new_args = vec![];
            for arg in args {
                new_args.push(walk(arg));
            }
            node.ty = Func(name, new_args, Box::new(walk(*body)));
        }
        CompStmt(stmts) => {
            let mut new_stmts = vec![];
            for stmt in stmts {
                new_stmts.push(walk(stmt));
            }
            node.ty = CompStmt(new_stmts);
        }
        ExprStmt(expr) => node.ty = ExprStmt(Box::new(walk(*expr))),
        _ => panic!("unknown node type"),
    };
    node
}

pub fn sema(nodes: Vec<Node>) -> Vec<Node> {
    let mut new_nodes = vec![];
    for mut node in nodes {
        assert!(matches!(node.ty, NodeType::Func(_, _, _)));

        *VARS.lock().unwrap() = HashMap::new();
        *STACKSIZE.lock().unwrap() = 0;
        let new = walk(node);
        node.stacksize = *STACKSIZE.lock().unwrap();
        new_nodes.push(new);
    }
    new_nodes
}
