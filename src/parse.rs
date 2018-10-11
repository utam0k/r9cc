use token::{Token, TokenType, bad_token};
use {Ctype, Type, Scope};
use util::roundup;

use std::sync::Mutex;
use std::collections::HashMap;

// Quoted from 9cc
// > This is a recursive-descendent parser which constructs abstract
// > syntax tree from input tokens.
//
// > This parser knows only about BNF of the C grammer and doesn't care
// > about its semantics. Therefore, some invalid expressions, such as
// > `1+2=3`, are accepted by this parser, but that's intentional.
// > Semantic errors are detected in a later pass.

lazy_static!{
    static ref ENV: Mutex<Env> = Mutex::new(Env::new(None));
}

#[derive(Debug, Clone)]
pub enum NodeType {
    Num(i32), // Number literal
    Str(String, usize), // String literal, (data, len)
    Ident(String), // Identifier
    Decl(String), // declaration
    Vardef(String, Option<Box<Node>>, Scope), // Variable definition, name = init
    Lvar(Scope), // Variable reference
    Gvar(String, String, usize), // Variable reference, (name, data, len)
    BinOp(TokenType, Box<Node>, Box<Node>), // left-hand, right-hand
    If(Box<Node>, Box<Node>, Option<Box<Node>>), // "if" ( cond ) then "else" els
    Ternary(Box<Node>, Box<Node>, Box<Node>), // cond ? then : els
    For(Box<Node>, Box<Node>, Box<Node>, Box<Node>), // "for" ( init; cond; inc ) body
    Break,
    DoWhile(Box<Node>, Box<Node>), // do { body } while(cond)
    Addr(Box<Node>), // address-of operator("&"), expr
    Deref(Box<Node>), // pointer dereference ("*"), expr
    Dot(Box<Node>, String, usize), // Struct member accessm, (expr, name, offset)
    Exclamation(Box<Node>), // !, expr
    Neg(Box<Node>), // -
    PostInc(Box<Node>), // post ++
    PostDec(Box<Node>), // post --
    Return(Box<Node>), // "return", stmt
    Sizeof(Box<Node>), // "sizeof", expr
    Alignof(Box<Node>), // "_Alignof", expr
    Call(String, Vec<Node>), // Function call(name, args)
    Func(String, Vec<Node>, Box<Node>, usize), // Function definition(name, args, body, stacksize)
    CompStmt(Vec<Node>), // Compound statement
    VecStmt(Vec<Node>), // For the purpose of assign a value when initializing an array.
    ExprStmt(Box<Node>), // Expression statement
    StmtExpr(Box<Node>), // Statement expression (GNU extn.)
    Null,
}

#[derive(Debug, Clone)]
pub struct Node {
    pub op: NodeType, // Node type
    pub ty: Box<Type>, // C type
}

impl Node {
    pub fn new(op: NodeType) -> Self {
        Self {
            op,
            ty: Box::new(Type::default()),
        }
    }

    pub fn new_int(val: i32) -> Self {
        Node::new(NodeType::Num(val))
    }

    pub fn scale_ptr(node: Box<Node>, ty: &Type) -> Self {
        match ty.ty {
            Ctype::Ptr(ref ptr_to) => {
                Node::new_binop(TokenType::Mul, *node, Node::new_int(ptr_to.size as i32))
            }
            _ => panic!("expect ptr type"),
        }
    }

    pub fn new_binop(ty: TokenType, lhs: Node, rhs: Node) -> Self {
        Node::new(NodeType::BinOp(ty, Box::new(lhs), Box::new(rhs)))
    }

    pub fn new_num(val: i32) -> Self {
        Node::new(NodeType::Num(val))
    }

    pub fn is_null(&self) -> bool {
        match self.op {
            NodeType::Null => true,
            _ => false,
        }
    }
}

macro_rules! new_expr(
    ($i:path, $expr:expr) => (
        Node::new($i(Box::new($expr)))
    )
);

impl Type {
    pub fn new(ty: Ctype, size: usize) -> Self {
        Type {
            ty,
            size,
            align: size,
        }
    }

    pub fn void_ty() -> Self {
        Type::new(Ctype::Void, 0)
    }

    pub fn char_ty() -> Self {
        Type::new(Ctype::Char, 1)
    }

    pub fn int_ty() -> Self {
        Type::new(Ctype::Int, 4)
    }

    pub fn ptr_to(base: Box<Type>) -> Self {
        Type::new(Ctype::Ptr(base), 8)
    }

    pub fn ary_of(base: Box<Type>, len: usize) -> Self {
        let align = base.align;
        let size = base.size * len;
        let mut ty = Type::new(Ctype::Ary(base, len), size);
        ty.align = align;
        ty
    }
}

#[derive(Debug, Clone)]
struct Env {
    tags: HashMap<String, Type>,
    typedefs: HashMap<String, Type>,
    next: Option<Box<Env>>,
}

impl Env {
    pub fn new(next: Option<Box<Env>>) -> Self {
        Env {
            next,
            tags: HashMap::new(),
            typedefs: HashMap::new(),
        }
    }
}

fn find_typedef(name: &String) -> Option<Type> {
    let env = ENV.lock().unwrap().clone();
    let mut next: &Option<Box<Env>> = &Some(Box::new(env));
    loop {
        if let Some(ref e) = next {
            let ty = e.typedefs.get(name);
            if ty.is_some() {
                return ty.cloned();
            }
            next = &e.next;
        } else {
            return None;
        }
    }
}

fn find_tag(name: &String) -> Option<Type> {
    let env = ENV.lock().unwrap().clone();
    let mut next: &Option<Box<Env>> = &Some(Box::new(env));
    loop {
        if let Some(ref e) = next {
            let ty = e.tags.get(name);
            if ty.is_some() {
                return ty.cloned();
            }
            next = &e.next;
        } else {
            return None;
        }
    }
}

fn expect(ty: TokenType, tokens: &Vec<Token>, pos: &mut usize) {
    let t = &tokens[*pos];
    if t.ty != ty {
        bad_token(t, &format!("{:?} expected", ty));
    }
    *pos += 1;
}

fn consume(ty: TokenType, tokens: &Vec<Token>, pos: &mut usize) -> bool {
    let t = &tokens[*pos];
    if t.ty != ty {
        return false;
    }
    *pos += 1;
    return true;
}

fn is_typename(t: &Token) -> bool {
    use self::TokenType::*;
    if let TokenType::Ident(ref name) = t.ty {
        return find_typedef(name).is_some();
    }
    t.ty == Int || t.ty == Char || t.ty == Void || t.ty == Struct
}

fn set_offset(members: &mut Vec<Node>) -> (usize, usize) {
    let mut off = 0;
    let mut align = 0;
    for node in members {
        if let NodeType::Vardef(_, _, Scope::Local(offset)) = &mut node.op {
            let t = &node.ty;
            off = roundup(off, t.align);
            *offset = off;
            off += t.size;

            if align < t.align {
                align = t.align;
            }
        } else {
            panic!();
        }
    }
    return (off, align);
}

fn add_member(ty: &mut Type, mut members: Vec<Node>) {
    let (off, align) = set_offset(&mut members);
    if let Ctype::Struct(ref mut members2) = ty.ty {
        *members2 = members;
    }
    ty.size = roundup(off, align);
}

fn decl_specifiers(tokens: &Vec<Token>, pos: &mut usize) -> Option<Type> {
    let t = &tokens[*pos];
    *pos += 1;
    match t.ty {
        TokenType::Ident(ref name) => {
            if let Some(ty) = find_typedef(name) {
                return Some(ty.clone());
            } else {
                *pos -= 1;
                return None;
            }
        }
        TokenType::Int => Some(Type::int_ty()),
        TokenType::Char => Some(Type::char_ty()),
        TokenType::Void => Some(Type::void_ty()),
        TokenType::Struct => {
            let mut tag_may: Option<String> = None;
            let t = &tokens[*pos];
            if let TokenType::Ident(ref name) = t.ty {
                *pos += 1;
                tag_may = Some(name.clone())
            }

            let mut members = vec![];
            if consume(TokenType::LeftBrace, tokens, pos) {
                while !consume(TokenType::RightBrace, tokens, pos) {
                    members.push(declaration(tokens, pos))
                }
            }

            let mut ty_may: Option<Type> = None;
            if let Some(ref tag) = tag_may {
                if members.is_empty() {
                    ty_may = find_tag(&tag);
                }
            }
            let mut ty = ty_may.unwrap_or(Type::new(Ctype::Struct(vec![]), 10));

            if !members.is_empty() {
                add_member(&mut ty, members);
                if let Some(tag) = tag_may {
                    ENV.lock().unwrap().tags.insert(tag, ty.clone());
                }
            }
            return Some(ty.clone());
        }
        _ => bad_token(&t, "typename expected"),
    }
}

fn ident(tokens: &Vec<Token>, pos: &mut usize) -> String {
    let t = &tokens[*pos];
    if let TokenType::Ident(ref name) = t.ty {
        *pos += 1;
        name.clone()
    } else {
        bad_token(t, "variable name expected");
    }
}

fn primary(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let t = &tokens[*pos];
    *pos += 1;
    match t.ty {
        TokenType::Num(val) => Node::new_num(val),
        TokenType::Str(ref str, len) => {
            let mut node = Node::new(NodeType::Str(str.clone(), len));
            node.ty = Box::new(Type::ary_of(Box::new(Type::char_ty()), len));
            node
        }
        TokenType::Ident(ref name) => {
            if !consume(TokenType::LeftParen, tokens, pos) {
                return Node::new(NodeType::Ident(name.clone()));
            }

            let mut args = vec![];
            if consume(TokenType::RightParen, tokens, pos) {
                return Node::new(NodeType::Call(name.clone(), args));
            }

            args.push(assign(tokens, pos));
            while consume(TokenType::Comma, tokens, pos) {
                args.push(assign(tokens, pos));
            }
            expect(TokenType::RightParen, tokens, pos);
            return Node::new(NodeType::Call(name.clone(), args));
        }
        TokenType::LeftParen => {
            if consume(TokenType::LeftBrace, tokens, pos) {
                let stmt = Box::new(compound_stmt(tokens, pos));
                expect(TokenType::RightParen, tokens, pos);
                return Node::new(NodeType::StmtExpr(stmt));
            }
            let node = expr(tokens, pos);
            expect(TokenType::RightParen, tokens, pos);
            node
        }
        _ => bad_token(t, "number expected"),
    }
}

fn postfix(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let mut lhs = primary(tokens, pos);

    loop {
        if consume(TokenType::Inc, tokens, pos) {
            lhs = new_expr!(NodeType::PostInc, lhs);
            continue;
        }

        if consume(TokenType::Dec, tokens, pos) {
            lhs = new_expr!(NodeType::PostDec, lhs);
            continue;
        }

        if consume(TokenType::Dot, tokens, pos) {
            // TODO: Use new_expr!
            lhs = Node::new(NodeType::Dot(Box::new(lhs), ident(tokens, pos), 0));
            continue;
        }

        if consume(TokenType::Arrow, tokens, pos) {
            lhs = Node::new(NodeType::Dot(
                Box::new(new_expr!(NodeType::Deref, lhs)),
                ident(tokens, pos),
                0,
            ));
            continue;
        }

        if consume(TokenType::LeftBracket, tokens, pos) {
            lhs = new_expr!(
                NodeType::Deref,
                Node::new_binop(TokenType::Plus, lhs, assign(tokens, pos))
            );
            expect(TokenType::RightBracket, tokens, pos);
            continue;
        }
        return lhs;
    }
}

fn unary(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    if consume(TokenType::Minus, tokens, pos) {
        return new_expr!(NodeType::Neg, unary(tokens, pos));
    }
    if consume(TokenType::Mul, tokens, pos) {
        return new_expr!(NodeType::Deref, unary(tokens, pos));
    }
    if consume(TokenType::And, tokens, pos) {
        return new_expr!(NodeType::Addr, unary(tokens, pos));
    }
    if consume(TokenType::Exclamation, tokens, pos) {
        return new_expr!(NodeType::Exclamation, unary(tokens, pos));
    }
    if consume(TokenType::Sizeof, tokens, pos) {
        return new_expr!(NodeType::Sizeof, unary(tokens, pos));
    }
    if consume(TokenType::Alignof, tokens, pos) {
        return new_expr!(NodeType::Alignof, unary(tokens, pos));
    }

    if consume(TokenType::Inc, tokens, pos) {
        return Node::new_binop(TokenType::AddEQ, unary(tokens, pos), Node::new_num(1));
    }
    if consume(TokenType::Dec, tokens, pos) {
        return Node::new_binop(TokenType::SubEQ, unary(tokens, pos), Node::new_num(1));
    }

    postfix(tokens, pos)
}

fn mul(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let mut lhs = unary(&tokens, pos);

    loop {
        if consume(TokenType::Mul, tokens, pos) {
            lhs = Node::new_binop(TokenType::Mul, lhs, unary(&tokens, pos));
        } else if consume(TokenType::Div, tokens, pos) {
            lhs = Node::new_binop(TokenType::Div, lhs, unary(&tokens, pos));
        } else if consume(TokenType::Mod, tokens, pos) {
            lhs = Node::new_binop(TokenType::Mod, lhs, unary(&tokens, pos));
        } else {
            return lhs;
        }
    }
}

fn add(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let mut lhs = mul(&tokens, pos);

    loop {
        if consume(TokenType::Plus, tokens, pos) {
            lhs = Node::new_binop(TokenType::Plus, lhs, mul(&tokens, pos));
        } else if consume(TokenType::Minus, tokens, pos) {
            lhs = Node::new_binop(TokenType::Minus, lhs, mul(&tokens, pos));
        } else {
            return lhs;
        }
    }
}

fn shift(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let mut lhs = add(tokens, pos);
    loop {
        if consume(TokenType::SHL, tokens, pos) {
            lhs = Node::new_binop(TokenType::SHL, lhs, add(tokens, pos));
        } else if consume(TokenType::SHR, tokens, pos) {
            lhs = Node::new_binop(TokenType::SHR, lhs, add(tokens, pos));
        } else {
            return lhs;
        }
    }
}

fn relational(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let mut lhs = shift(tokens, pos);
    loop {
        if consume(TokenType::LeftAngleBracket, tokens, pos) {
            lhs = Node::new_binop(TokenType::LeftAngleBracket, lhs, shift(tokens, pos));
        } else if consume(TokenType::RightAngleBracket, tokens, pos) {
            lhs = Node::new_binop(TokenType::LeftAngleBracket, shift(tokens, pos), lhs);
        } else if consume(TokenType::LE, tokens, pos) {
            lhs = Node::new_binop(TokenType::LE, lhs, shift(tokens, pos))
        } else if consume(TokenType::GE, tokens, pos) {
            lhs = Node::new_binop(TokenType::LE, shift(tokens, pos), lhs);
        } else {
            return lhs;
        }
    }
}

fn equality(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let mut lhs = relational(tokens, pos);
    loop {
        if consume(TokenType::EQ, tokens, pos) {
            lhs = Node::new_binop(TokenType::EQ, lhs, relational(tokens, pos));
        } else if consume(TokenType::NE, tokens, pos) {
            lhs = Node::new_binop(TokenType::NE, lhs, relational(tokens, pos));
        } else {
            return lhs;
        }
    }
}

fn bit_and(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let mut lhs = equality(tokens, pos);
    while consume(TokenType::And, tokens, pos) {
        lhs = Node::new_binop(TokenType::And, lhs, equality(tokens, pos));
    }
    return lhs;
}

fn bit_xor(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let mut lhs = bit_and(tokens, pos);
    while consume(TokenType::Hat, tokens, pos) {
        lhs = Node::new_binop(TokenType::Hat, lhs, bit_and(tokens, pos));
    }
    return lhs;
}

fn bit_or(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let mut lhs = bit_xor(tokens, pos);
    while consume(TokenType::VerticalBar, tokens, pos) {
        lhs = Node::new_binop(TokenType::VerticalBar, lhs, bit_xor(tokens, pos));
    }
    return lhs;
}

fn logand(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let mut lhs = bit_or(tokens, pos);
    while consume(TokenType::Logand, tokens, pos) {
        lhs = Node::new_binop(TokenType::Logand, lhs, logand(tokens, pos));
    }
    return lhs;
}

fn logor(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let mut lhs = logand(tokens, pos);
    while consume(TokenType::Logor, tokens, pos) {
        lhs = Node::new_binop(TokenType::Logor, lhs, logand(tokens, pos));
    }
    return lhs;
}

fn conditional(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let cond = logor(tokens, pos);
    if !consume(TokenType::Question, tokens, pos) {
        return cond;
    }
    let then = expr(tokens, pos);
    expect(TokenType::Colon, tokens, pos);
    let els = conditional(tokens, pos);
    Node::new(NodeType::Ternary(
        Box::new(cond),
        Box::new(then),
        Box::new(els),
    ))
}

fn assign_op(ty: &TokenType) -> Option<&TokenType> {
    use self::TokenType::*;
    match ty {
        Equal | MulEQ | DivEQ | ModEQ | AddEQ | SubEQ | ShlEQ | ShrEQ | BitandEQ | XorEQ |
        BitorEQ => Some(ty),
        _ => None,
    }
}

fn assign(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let lhs = conditional(tokens, pos);
    if let Some(op) = assign_op(&tokens[*pos].ty) {
        *pos += 1;
        Node::new_binop(op.clone(), lhs, assign(tokens, pos))
    } else {
        lhs
    }
}

fn expr(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let lhs = assign(tokens, pos);
    if !consume(TokenType::Comma, tokens, pos) {
        return lhs;
    }
    return Node::new_binop(TokenType::Comma, lhs, expr(tokens, pos));
}

fn ctype(tokens: &Vec<Token>, pos: &mut usize) -> Type {
    let t = &tokens[*pos];
    if let Some(mut ty) = decl_specifiers(tokens, pos) {
        while consume(TokenType::Mul, tokens, pos) {
            ty = Type::ptr_to(Box::new(ty));
        }
        ty
    } else {
        bad_token(t, "typename expected");
    }
}

fn read_array(mut ty: Box<Type>, tokens: &Vec<Token>, pos: &mut usize) -> Type {
    let mut v: Vec<usize> = vec![];
    while consume(TokenType::LeftBracket, tokens, pos) {
        if consume(TokenType::RightBracket, tokens, pos) {
            v.push(0); // temporary value
            continue;
        }

        let len = expr(tokens, pos);
        if let NodeType::Num(n) = len.op {
            v.push(n as usize);
            expect(TokenType::RightBracket, tokens, pos);
        } else {
            panic!("number expected");
        }
    }

    v.reverse();
    for val in v {
        ty = Box::new(Type::ary_of(ty, val));
    }
    *ty
}

fn array_init_rval(tokens: &Vec<Token>, pos: &mut usize, ident: Node) -> Node {
    let mut init = vec![];
    let mut i = 0;
    loop {
        let val = primary(tokens, pos);
        let node = new_expr!(
            NodeType::Deref,
            Node::new_binop(TokenType::Plus, ident.clone(), Node::new(NodeType::Num(i)))
        );
        init.push(Node::new(NodeType::ExprStmt(
            Box::new(Node::new_binop(TokenType::Equal, node, val)),
        )));
        if !consume(TokenType::Comma, tokens, pos) {
            break;
        }
        i += 1;
    }
    expect(TokenType::RightBrace, tokens, pos);
    return Node::new(NodeType::VecStmt(init));
}

fn update_ptr_to(src: &mut Box<Type>, dst: Box<Type>, tokens: &Vec<Token>, pos: &mut usize) {
    match src.ty {
        Ctype::Ptr(ref mut ptr_to) => update_ptr_to(ptr_to, dst, tokens, pos),
        _ => *src = dst,
    }
}

fn direct_decl(ty: Box<Type>, tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let t = &tokens[*pos];
    let mut placeholder = Box::new(Type::default());
    let mut node;

    if let TokenType::Ident(_) = t.ty {
        node = Node::new(NodeType::Vardef(ident(tokens, pos), None, Scope::Local(0)));

    } else if consume(TokenType::LeftParen, tokens, pos) {
        node = declarator(&mut placeholder, tokens, pos);
        expect(TokenType::RightParen, tokens, pos);
    } else {
        bad_token(t, "bad direct-declarator");
    }

    // Read the second half of type name (e.g. `[3][5]`).
    update_ptr_to(
        &mut node.ty,
        Box::new(read_array(ty, tokens, pos)),
        tokens,
        pos,
    );

    // Read an initializer.
    let init: Option<Box<Node>>;
    if consume(TokenType::Equal, tokens, pos) {
        // Assign a value when initializing an array.
        if let TokenType::Ident(ref name) = t.ty {
            if consume(TokenType::LeftBrace, tokens, pos) {
                let mut stmts = vec![];
                let mut ary_declaration =
                    Node::new(NodeType::Vardef(name.clone(), None, Scope::Local(0)));
                ary_declaration.ty = node.ty;
                stmts.push(ary_declaration);
                let init_ary =
                    array_init_rval(tokens, pos, Node::new(NodeType::Ident(name.clone())));
                stmts.push(init_ary);
                return Node::new(NodeType::VecStmt(stmts));
            }
        }

        init = Some(Box::new(assign(tokens, pos)));
        match node.op {
            NodeType::Vardef(_, ref mut init2, _) => *init2 = init,
            _ => unreachable!(),
        }
    }
    return node;
}

fn declarator(ty: &mut Type, tokens: &Vec<Token>, pos: &mut usize) -> Node {
    while consume(TokenType::Mul, tokens, pos) {
        *ty = Type::ptr_to(Box::new(ty.clone()));
    }
    direct_decl(Box::new(ty.clone()), tokens, pos)
}

fn declaration(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let mut ty = decl_specifiers(tokens, pos).unwrap();
    let node = declarator(&mut ty, tokens, pos);
    expect(TokenType::Semicolon, tokens, pos);
    return node;
}

fn param_declaration(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let mut ty = decl_specifiers(tokens, pos).unwrap();
    let mut node = declarator(&mut ty, tokens, pos);
    if let Ctype::Ary(ary_of, _) = node.ty.ty {
        node.ty = Box::new(Type::ptr_to(ary_of));
    }
    node
}

fn expr_stmt(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let expr = expr(tokens, pos);
    let node = new_expr!(NodeType::ExprStmt, expr);
    expect(TokenType::Semicolon, tokens, pos);
    node
}

fn stmt(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let t = &tokens[*pos];
    *pos += 1;

    match t.ty {
        TokenType::Typedef => {
            let node = declaration(tokens, pos);
            if let NodeType::Vardef(name, _, _) = node.op {
                ENV.lock().unwrap().typedefs.insert(name, *node.ty);
                return Node::new(NodeType::Null);
            } else {
                unreachable!();
            }
        }
        TokenType::If => {
            let mut els = None;
            expect(TokenType::LeftParen, tokens, pos);
            let cond = expr(&tokens, pos);
            expect(TokenType::RightParen, tokens, pos);
            let then = stmt(&tokens, pos);
            if consume(TokenType::Else, tokens, pos) {
                els = Some(Box::new(stmt(&tokens, pos)));
            }
            Node::new(NodeType::If(Box::new(cond), Box::new(then), els))
        }
        TokenType::For => {
            expect(TokenType::LeftParen, tokens, pos);

            let init: Box<Node> = if is_typename(&tokens[*pos]) {
                Box::new(declaration(tokens, pos))
            } else if consume(TokenType::Semicolon, tokens, pos) {
                Box::new(Node::new(NodeType::Null))
            } else {
                Box::new(expr_stmt(tokens, pos))
            };

            let cond;
            if !consume(TokenType::Semicolon, tokens, pos) {
                cond = Box::new(expr(&tokens, pos));
                expect(TokenType::Semicolon, tokens, pos);
            } else {
                cond = Box::new(Node::new(NodeType::Null))
            }

            let inc;
            if !consume(TokenType::RightParen, tokens, pos) {
                inc = Box::new(new_expr!(NodeType::ExprStmt, expr(&tokens, pos)));
                expect(TokenType::RightParen, tokens, pos);
            } else {
                inc = Box::new(Node::new(NodeType::Null))
            }

            let body = Box::new(stmt(&tokens, pos));
            Node::new(NodeType::For(init, cond, inc, body))
        }
        TokenType::While => {
            expect(TokenType::LeftParen, tokens, pos);
            let init = Box::new(Node::new(NodeType::Null));
            let inc = Box::new(Node::new(NodeType::Null));
            let cond = Box::new(expr(&tokens, pos));
            expect(TokenType::RightParen, tokens, pos);
            let body = Box::new(stmt(&tokens, pos));
            Node::new(NodeType::For(init, cond, inc, body))
        }
        TokenType::Do => {
            let body = Box::new(stmt(tokens, pos));
            expect(TokenType::While, tokens, pos);
            expect(TokenType::LeftParen, tokens, pos);
            let cond = Box::new(expr(tokens, pos));
            expect(TokenType::RightParen, tokens, pos);
            expect(TokenType::Semicolon, tokens, pos);
            Node::new(NodeType::DoWhile(body, cond))
        }
        TokenType::Break => Node::new(NodeType::Break),
        TokenType::Return => {
            let expr = expr(&tokens, pos);
            expect(TokenType::Semicolon, tokens, pos);
            Node::new(NodeType::Return(Box::new(expr)))
        }
        TokenType::LeftBrace => {
            let mut stmts = vec![];
            while !consume(TokenType::RightBrace, tokens, pos) {
                stmts.push(stmt(&tokens, pos));
            }
            Node::new(NodeType::CompStmt(stmts))
        }
        TokenType::Semicolon => Node::new(NodeType::Null),
        _ => {
            *pos -= 1;
            if is_typename(&tokens[*pos]) {
                return declaration(tokens, pos);
            }
            return expr_stmt(tokens, pos);
        }
    }
}

fn compound_stmt(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let mut stmts = vec![];

    let new_env = Env::new(Some(Box::new(ENV.lock().unwrap().clone())));
    *ENV.lock().unwrap() = new_env;
    while !consume(TokenType::RightBrace, tokens, pos) {
        stmts.push(stmt(tokens, pos));
    }
    let next = ENV.lock().unwrap().next.clone();
    *ENV.lock().unwrap() = *next.unwrap();
    Node::new(NodeType::CompStmt(stmts))
}

fn toplevel(tokens: &Vec<Token>, pos: &mut usize) -> Option<Node> {
    let is_typedef = consume(TokenType::Typedef, &tokens, pos);
    let is_extern = consume(TokenType::Extern, &tokens, pos);

    let mut ty = ctype(tokens, pos);
    let t = &tokens[*pos];
    let name: String;
    if let TokenType::Ident(ref name2) = t.ty {
        name = name2.clone();
    } else {
        bad_token(t, "function or variable name expected");
    }
    *pos += 1;

    // Function
    if consume(TokenType::LeftParen, tokens, pos) {
        let mut args = vec![];
        if !consume(TokenType::RightParen, tokens, pos) {
            args.push(param_declaration(tokens, pos));
            while consume(TokenType::Comma, tokens, pos) {
                args.push(param_declaration(tokens, pos));
            }
            expect(TokenType::RightParen, tokens, pos);
        }

        if consume(TokenType::Semicolon, tokens, pos) {
            let mut node = Node::new(NodeType::Decl(name));
            node.ty = Box::new(Type::new(Ctype::Func(Box::new(ty)), 0));
            return Some(node);
        }

        let t = &tokens[*pos];
        expect(TokenType::LeftBrace, tokens, pos);
        if is_typedef {
            bad_token(t, "typedef {} has function definition");
        }
        let body = compound_stmt(tokens, pos);

        let mut node = Node::new(NodeType::Func(name, args, Box::new(body), 0));
        node.ty = Box::new(Type::new(Ctype::Func(Box::new(ty)), 0));
        return Some(node);
    }

    ty = read_array(Box::new(ty), tokens, pos);
    expect(TokenType::Semicolon, tokens, pos);

    if is_typedef {
        ENV.lock().unwrap().typedefs.insert(
            name.clone(),
            ty.clone(),
        );
        return None;
    }

    // Global variable
    let mut node;
    if is_extern {
        node = Node::new(NodeType::Vardef(
            name,
            None,
            Scope::Global(String::new(), 0, true),
        ));
    } else {
        node = Node::new(NodeType::Vardef(
            name,
            None,
            Scope::Global(String::new(), ty.size, false),
        ));
    }
    node.ty = Box::new(ty);
    Some(node)
}

/* e.g.
 function -> param
+---------+
int main() {     ; +-+                        int   []         2
  int ary[2];    ;   |               +->stmt->declaration->read_array->primary
  ary[0]=1;      ;   | compound_stmt-+->stmt->...                ary
  return ary[0]; ;   |               +->stmt->assign->postfix-+->primary
}                ; +-+                  return        []      +->primary
                                                                 0
*/
pub fn parse(tokens: &Vec<Token>) -> Vec<Node> {
    let mut pos = 0;

    let mut v = vec![];
    while tokens.len() != pos {
        if let Some(node) = toplevel(tokens, &mut pos) {
            v.push(node);
        }
    }
    v
}
