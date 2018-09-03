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
    static ref STRLABEL: Mutex<usize> = Mutex::new(0);
    static ref GLOBALS: Mutex<Vec<Var>> = Mutex::new(vec![]);
    static ref STACKSIZE: Mutex<usize> = Mutex::new(0);
}

#[derive(Debug, Clone)]
struct Env {
    vars: HashMap<String, Var>,
    next: Option<Box<Env>>,
}

impl Env {
    pub fn new(next: Option<Box<Env>>) -> Self {
        Env {
            vars: HashMap::new(),
            next: next,
        }
    }

    pub fn find(&self, name: &String) -> Option<&Var> {
        let mut env: &Env = self;
        loop {
            if let Some(var) = env.vars.get(name) {
                return Some(&var);
            }
            if let Some(ref next_env) = env.next {
                env = &next_env;
            } else {
                break;
            }
        }
        return None;
    }
}

fn swap(p: &mut Box<Node>, q: &mut Box<Node>) {
    mem::swap(p, q);
}

#[derive(Debug, Clone)]
pub enum Scope {
    Local(usize), // offset
    Global(String, String, usize), // name, data, len
}

#[derive(Debug, Clone)]
pub struct Var {
    ty: Box<Type>,
    pub scope: Scope,
}

impl Var {
    fn new(ty: Box<Type>, scope: Scope) -> Self {
        Var {
            ty: ty,
            scope: scope,
        }
    }
}

fn addr_of(base: Node, ty: Type) -> Node {
    let ty = Type::new(Ctype::Ptr(Box::new(ty)));
    let mut node = Node::new(NodeType::Addr(Box::new(base.clone())));
    node.ty = Box::new(ty);
    node
}

fn walk(env: &mut Env, mut node: Node, decay: bool) -> Node {
    use self::NodeType::*;
    let op = node.op.clone();
    match op {
        Num(_) => (),
        Str(str) => {
            let name = format!(".L.str{}", *STRLABEL.lock().unwrap());
            *STRLABEL.lock().unwrap() += 1;
            let len = str.len() + 1;
            let var = Var::new(node.ty.clone(), Scope::Global(name.clone(), str, len));
            GLOBALS.lock().unwrap().push(var);

            let mut ret = Node::new(NodeType::Gvar(name));
            ret.ty = node.ty;
            return walk(env, ret, decay);
        }
        Ident(ref name) => {
            if let Some(var) = env.find(name) {
                node.op = NodeType::Lvar;
                if let Scope::Local(offset) = var.scope {
                    node.offset = offset;
                }
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
        Gvar(_) => {
            if decay {
                if let Ctype::Ary(ref ary_of, _) = node.ty.ty.clone() {
                    return addr_of(node, *ary_of.clone());
                }
            }
        }
        Vardef(name, init_may) => {
            *STACKSIZE.lock().unwrap() += size_of(&*node.ty);
            node.offset = *STACKSIZE.lock().unwrap();

            env.vars.insert(
                name.clone(),
                Var::new(node.ty.clone(), Scope::Local(*STACKSIZE.lock().unwrap())),
            );

            if let Some(mut init) = init_may {
                node.op = Vardef(name, Some(Box::new(walk(env, *init, true))));
            }
        }
        If(cond, then, els_may) => {
            let new_cond = Box::new(walk(env, *cond, true));
            let new_then = Box::new(walk(env, *then, true));
            let mut new_els = None;
            if let Some(mut els) = els_may {
                new_els = Some(Box::new(walk(env, *els, true)));
            }
            node.op = If(new_cond, new_then, new_els);
        }
        For(init, cond, inc, body) => {
            node.op = For(
                Box::new(walk(env, *init, true)),
                Box::new(walk(env, *cond, true)),
                Box::new(walk(env, *inc, true)),
                Box::new(walk(env, *body, true)),
            );
        }
        BinOp(token_type, mut lhs, mut rhs) => {
            match token_type {
                TokenType::Plus | TokenType::Minus => {
                    lhs = Box::new(walk(env, *lhs, true));
                    rhs = Box::new(walk(env, *rhs, true));

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
                    lhs = Box::new(walk(env, *lhs, false));
                    if !matches!(lhs.op, NodeType::Lvar) && !matches!(lhs.op, NodeType::Deref(_)) {
                        panic!("not an lvalue: {:?}", node.op);
                    }
                    node.op = BinOp(token_type, lhs.clone(), Box::new(walk(env, *rhs, true)));
                    node.ty = lhs.ty;
                }
                _ => {
                    lhs = Box::new(walk(env, *lhs, true));
                    node.op = BinOp(token_type, lhs.clone(), Box::new(walk(env, *rhs, true)));
                    node.ty = lhs.ty;
                }
            }
        }
        Logand(mut lhs, rhs) => {
            lhs = Box::new(walk(env, *lhs, true));
            node.op = Logand(lhs.clone(), Box::new(walk(env, *rhs, true)));
            node.ty = lhs.ty;
        }
        Logor(mut lhs, rhs) => {
            lhs = Box::new(walk(env, *lhs, true));
            node.op = Logor(lhs.clone(), Box::new(walk(env, *rhs, true)));
            node.ty = lhs.ty;
        }
        Addr(mut expr) => {
            expr = Box::new(walk(env, *expr, true));
            node.ty = Box::new(Type::new(Ctype::Ptr(expr.ty.clone())));
            node.op = Addr(expr);
        }
        Deref(mut expr) => {
            expr = Box::new(walk(env, *expr, true));
            match expr.ty.ty {
                Ctype::Ptr(ref ptr_of) => node.ty = ptr_of.clone(),
                _ => panic!("operand must be a pointer"),
            }
            node.op = Deref(expr);
        }
        Return(expr) => node.op = Return(Box::new(walk(env, *expr, true))),
        Sizeof(mut expr) => {
            expr = Box::new(walk(env, *expr, false));

            node.op = Num(size_of(&*expr.ty) as i32);
            node.ty.ty = Ctype::Int;
        }
        Call(name, args) => {
            let mut new_args = vec![];
            for arg in args {
                new_args.push(walk(env, arg, true));
            }
            node.op = Call(name, new_args);
        }
        Func(name, args, body, stacksize, strings) => {
            let mut new_args = vec![];
            for arg in args {
                new_args.push(walk(env, arg, true));
            }
            node.op = Func(
                name,
                new_args,
                Box::new(walk(env, *body, true)),
                stacksize,
                strings,
            );
        }
        CompStmt(stmts) => {
            let mut new_stmts = vec![];
            let mut new_env = Env::new(Some(Box::new(env.clone())));
            for stmt in stmts {
                new_stmts.push(walk(&mut new_env, stmt, true));
            }
            node.op = CompStmt(new_stmts);
        }
        ExprStmt(expr) => node.op = ExprStmt(Box::new(walk(env, *expr, true))),
        _ => panic!("unknown node type"),
    };
    node
}

pub fn sema(nodes: Vec<Node>) -> Vec<Node> {
    let mut new_nodes = vec![];
    for mut node in nodes {
        assert!(matches!(node.op, NodeType::Func(_, _, _, _, _)));

        *STACKSIZE.lock().unwrap() = 0;
        *GLOBALS.lock().unwrap() = vec![];
        let mut new = walk(&mut Env::new(None), node, true);
        if let NodeType::Func(name, args, body, _, _) = new.op {
            new.op = NodeType::Func(
                name,
                args,
                body,
                *STACKSIZE.lock().unwrap(),
                GLOBALS.lock().unwrap().clone(),
            )
        }
        new_nodes.push(new);
    }
    new_nodes
}
