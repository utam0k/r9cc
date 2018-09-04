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

    fn new_global(ty: Box<Type>, data: String, len: usize) -> Self {
        let name = format!(".L.str{}", *STRLABEL.lock().unwrap());
        *STRLABEL.lock().unwrap() += 1;
        let var = Var::new(ty, Scope::Global(name, data, len));
        return var;
    }
}

fn maybe_decay(base: Node, decay: bool) -> Node {
    if !decay {
        return base;
    }

    if let Ctype::Ary(ary_of, _) = base.ty.ty.clone() {
        let mut node = Node::new(NodeType::Addr(Box::new(base)));
        node.ty = Box::new(Type::new(Ctype::Ptr(ary_of.clone())));
        node
    } else {
        base
    }
}

fn check_lval(node: &Node) {
    let op = &node.op;
    if matches!(op, NodeType::Lvar(_)) || matches!(op, NodeType::Gvar(_, _, _)) ||
        matches!(op, NodeType::Deref(_))
    {
        return;
    }
    panic!("not an lvalue: {:?}", node.op);
}

fn walk(env: &mut Env, mut node: Node, decay: bool) -> Node {
    use self::NodeType::*;
    let op = node.op.clone();
    match op {
        Num(_) => (),
        Str(data, len) => {
            let name = format!(".L.str{}", *STRLABEL.lock().unwrap());
            let var = Var::new_global(node.ty.clone(), data, len);
            GLOBALS.lock().unwrap().push(var);

            let mut ret = Node::new(NodeType::Gvar(name, "".into(), len));
            ret.ty = node.ty;
            return maybe_decay(ret, decay);
        }
        Ident(ref name) => {
            if let Some(var) = env.find(name) {
                match var.scope {
                    Scope::Local(offset) => {
                        let mut ret = Node::new(NodeType::Lvar(Scope::Local(offset)));
                        ret.ty = var.ty.clone();
                        return maybe_decay(ret, decay);
                    }
                    Scope::Global(ref name, ref data, len) => {
                        let mut ret = Node::new(NodeType::Gvar(name.clone(), data.clone(), len));
                        ret.ty = var.ty.clone();
                        return maybe_decay(ret, decay);
                    }
                }
            } else {
                panic!("undefined variable: {}", name);
            }
        }
        Vardef(name, init_may, _) => {
            *STACKSIZE.lock().unwrap() += size_of(&*node.ty);
            let offset = *STACKSIZE.lock().unwrap();

            env.vars.insert(
                name.clone(),
                Var::new(node.ty.clone(), Scope::Local(offset)),
            );

            let mut init = None;
            if let Some(mut init2) = init_may {
                init = Some(Box::new(walk(env, *init2, true)));
            }
            node.op = Vardef(name, init, Scope::Local(offset));
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
                    check_lval(&*lhs);
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
            check_lval(&*expr);
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
        Func(name, args, body, stacksize) => {
            let mut new_args = vec![];
            for arg in args {
                new_args.push(walk(env, arg, true));
            }
            node.op = Func(name, new_args, Box::new(walk(env, *body, true)), stacksize);
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

pub fn sema(nodes: Vec<Node>) -> (Vec<Node>, Vec<Var>) {
    let mut new_nodes = vec![];
    let mut topenv = Env::new(None);

    for mut node in nodes {
        if let NodeType::Vardef(name, _, Scope::Global(_, data, len)) = node.op {
            let var = Var::new_global(node.ty, data, len);
            GLOBALS.lock().unwrap().push(var.clone());
            topenv.vars.insert(name, var);
            continue;
        }

        assert!(matches!(node.op, NodeType::Func(_, _, _, _)));
        let mut new = walk(&mut topenv, node, true);
        if let NodeType::Func(name, args, body, _) = new.op {
            new.op = NodeType::Func(name.clone(), args, body, *STACKSIZE.lock().unwrap());
            *STACKSIZE.lock().unwrap() = 0;
            new_nodes.push(new);
        }
    }
    (new_nodes, GLOBALS.lock().unwrap().clone())
}
