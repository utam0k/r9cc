use token::{Token, TokenType, bad_token};
use sema::Scope;
use util::roundup;

use std::sync::Mutex;
use std::collections::HashMap;

// This is a recursive-descendent parser which constructs abstract
// syntax tree from input tokens.
//
// This parser knows only about BNF of the C grammer and doesn't care
// about its semantics. Therefore, some invalid expressions, such as
// `1+2=3`, are accepted by this parser, but that's intentional.
// Semantic errors are detected in a later pass.

#[derive(Debug, Clone)]
struct Env {
    tags: HashMap<String, Vec<Node>>,
    next: Option<Box<Env>>,
}

impl Env {
    pub fn new(next: Option<Box<Env>>) -> Self {
        Env {
            next: next,
            tags: HashMap::new(),
        }
    }
}

lazy_static!{
    static ref ENV: Mutex<Env> = Mutex::new(Env::new(None));
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
    t.ty == Int || t.ty == Char || t.ty == Struct
}

fn read_type(t: &Token, tokens: &Vec<Token>, pos: &mut usize) -> Option<Type> {
    match t.ty {
        TokenType::Int => {
            *pos += 1;
            Some(Type::int_ty())
        }
        TokenType::Char => {
            *pos += 1;
            Some(Type::char_ty())
        }
        TokenType::Struct => {
            *pos += 1;

            let mut tag_may: Option<String> = None;
            let t = &tokens[*pos];
            if let TokenType::Ident(ref name) = t.ty {
                *pos += 1;
                tag_may = Some(name.clone())
            }

            let mut members = vec![];
            if consume(TokenType::LeftBrace, tokens, pos) {
                while !consume(TokenType::RightBrace, tokens, pos) {
                    members.push(decl(tokens, pos))
                }
            }

            if let Some(tag) = tag_may {
                if members.is_empty() {
                    if let Some(members2) = ENV.lock().unwrap().tags.get(&tag) {
                        members = members2.to_vec();
                        if members.is_empty() {
                            panic!("incomplete type: {}", tag);
                        }
                    }
                } else {
                    ENV.lock().unwrap().tags.insert(tag, members.clone());
                }
            } else {
                if members.is_empty() {
                    panic!("bad struct definition");
                }
            }

            Some(Type::new_struct(members))
        }
        _ => None,
    }
}

#[derive(Debug, Clone)]
pub enum NodeType {
    Num(i32), // Number literal
    Str(String, usize), // String literal, (data, len)
    Ident(String), // Identifier
    Vardef(String, Option<Box<Node>>, Scope), // Variable definition, name = init
    Lvar(Scope), // Variable reference
    Gvar(String, String, usize), // Variable reference, (name, data, len)
    BinOp(TokenType, Box<Node>, Box<Node>), // left-hand, right-hand
    If(Box<Node>, Box<Node>, Option<Box<Node>>), // "if" ( cond ) then "else" els
    For(Box<Node>, Box<Node>, Box<Node>, Box<Node>), // "for" ( init; cond; inc ) body
    DoWhile(Box<Node>, Box<Node>), // do { body } while(cond)
    Addr(Box<Node>), // address-of operator("&"), expr
    Deref(Box<Node>), // pointer dereference ("*"), expr
    Dot(Box<Node>, String, usize), // Struct member accessm, (expr, name, offset)
    Logand(Box<Node>, Box<Node>), // left-hand, right-hand
    Logor(Box<Node>, Box<Node>), // left-hand, right-hand
    Return(Box<Node>), // "return", stmt
    Sizeof(Box<Node>), // "sizeof", expr
    Alignof(Box<Node>), // "_Alignof", expr
    Call(String, Vec<Node>), // Function call(name, args)
    // Function definition(name, args, body, stacksize)
    Func(String, Vec<Node>, Box<Node>, usize),
    CompStmt(Vec<Node>), // Compound statement
    ExprStmt(Box<Node>), // Expression statement
    StmtExpr(Box<Node>), // Statement expression (GNU extn.)
    Null,
}

#[derive(Debug, Clone)]
pub enum Ctype {
    Int,
    Char,
    Ptr(Box<Type>), // ptr of
    Ary(Box<Type>, usize), // ary of, len
    Struct(Vec<Node>), // members
}

impl Default for Ctype {
    fn default() -> Ctype {
        Ctype::Int
    }
}

#[derive(Debug, Clone)]
pub struct Type {
    pub ty: Ctype,
    pub size: usize,
    pub align: usize,
}

impl Default for Type {
    fn default() -> Type {
        Type {
            ty: Ctype::default(),
            size: 4,
            align: 4,
        }
    }
}

impl Type {
    pub fn new(ty: Ctype, size: usize) -> Self {
        Type {
            ty,
            size,
            align: size,
        }
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

    pub fn new_struct(mut members: Vec<Node>) -> Self {
        let (off, align) = Self::set_offset(&mut members);
        let mut ty: Type = Type::new(Ctype::Struct(members), align);
        ty.size = roundup(off, align);
        ty
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

    pub fn int_ty(val: i32) -> Self {
        Node::new(NodeType::Num(val))
    }

    pub fn new_binop(ty: TokenType, lhs: Node, rhs: Node) -> Self {
        Node::new(NodeType::BinOp(ty, Box::new(lhs), Box::new(rhs)))
    }
}

macro_rules! new_expr(
    ($i:path, $expr:expr) => (
        Node::new($i(Box::new($expr)))
    )
);

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
        TokenType::Num(val) => {
            let mut node = Node::new(NodeType::Num(val));
            node.ty = Box::new(Type::int_ty());
            node
        }
        TokenType::Str(ref str, len) => {
            let mut node = Node::new(NodeType::Str(str.clone(), len));
            node.ty = Box::new(Type::ary_of(Box::new(Type::char_ty()), str.len()));
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
            while consume(TokenType::Colon, tokens, pos) {
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
            let node = assign(tokens, pos);
            expect(TokenType::RightParen, tokens, pos);
            node
        }
        _ => bad_token(t, "number expected"),
    }
}

fn postfix(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let mut lhs = primary(tokens, pos);

    loop {
        if consume(TokenType::Dot, tokens, pos) {
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
    if consume(TokenType::Mul, tokens, pos) {
        return new_expr!(NodeType::Deref, mul(tokens, pos));
    }
    if consume(TokenType::And, tokens, pos) {
        return new_expr!(NodeType::Addr, mul(tokens, pos));
    }
    if consume(TokenType::Sizeof, tokens, pos) {
        return new_expr!(NodeType::Sizeof, unary(tokens, pos));
    }
    if consume(TokenType::Alignof, tokens, pos) {
        return new_expr!(NodeType::Alignof, unary(tokens, pos));
    }
    postfix(tokens, pos)
}

fn mul(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let mut lhs = unary(&tokens, pos);

    loop {
        if tokens.len() == *pos {
            return lhs;
        }

        let t = &tokens[*pos];
        if t.ty != TokenType::Mul && t.ty != TokenType::Div {
            return lhs;
        }
        *pos += 1;
        lhs = Node::new_binop(t.ty.clone(), lhs, unary(&tokens, pos));
    }
}

fn add(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let mut lhs = mul(&tokens, pos);

    loop {
        if tokens.len() == *pos {
            return lhs;
        }

        let t = &tokens[*pos];
        if t.ty != TokenType::Plus && t.ty != TokenType::Minus {
            return lhs;
        }
        *pos += 1;
        let rhs = mul(&tokens, pos);
        lhs = Node::new_binop(t.ty.clone(), lhs, rhs);
    }
}

fn rel(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let mut lhs = add(tokens, pos);
    loop {
        let t = &tokens[*pos];
        if t.ty == TokenType::LeftAngleBracket {
            *pos += 1;
            lhs = Node::new_binop(TokenType::LeftAngleBracket, lhs, add(tokens, pos));
            continue;
        }

        if t.ty == TokenType::RightAngleBracket {
            *pos += 1;
            lhs = Node::new_binop(TokenType::LeftAngleBracket, add(tokens, pos), lhs);
            continue;
        }

        return lhs;
    }
}

fn equality(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let mut lhs = rel(tokens, pos);
    loop {
        let t = &tokens[*pos];
        if t.ty == TokenType::EQ {
            *pos += 1;
            lhs = Node::new_binop(TokenType::EQ, lhs, rel(tokens, pos));
        }
        if t.ty == TokenType::NE {
            *pos += 1;
            lhs = Node::new_binop(TokenType::NE, lhs, rel(tokens, pos));
        }
        return lhs;
    }
}


fn logand(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let mut lhs = equality(tokens, pos);
    loop {
        if tokens[*pos].ty != TokenType::Logand {
            return lhs;
        }
        *pos += 1;
        lhs = Node::new(NodeType::Logand(
            Box::new(lhs),
            Box::new(equality(tokens, pos)),
        ));
    }
}

fn logor(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let mut lhs = logand(tokens, pos);
    loop {
        if tokens[*pos].ty != TokenType::Logor {
            return lhs;
        }
        *pos += 1;
        lhs = Node::new(NodeType::Logor(
            Box::new(lhs),
            Box::new(logand(tokens, pos)),
        ));
    }
}

fn assign(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let lhs = logor(tokens, pos);
    if consume(TokenType::Equal, tokens, pos) {
        return Node::new_binop(TokenType::Equal, lhs, logor(tokens, pos));
    }
    return lhs;
}

fn ctype(tokens: &Vec<Token>, pos: &mut usize) -> Type {
    let t = &tokens[*pos];
    if let Some(mut ty) = read_type(t, tokens, pos) {
        while consume(TokenType::Mul, tokens, pos) {
            ty = Type::ptr_to(Box::new(ty));
        }
        ty
    } else {
        bad_token(t, "typename expected");
    }
}

fn read_array(mut ty: Box<Type>, tokens: &Vec<Token>, pos: &mut usize) -> Box<Type> {
    let mut v: Vec<usize> = vec![];
    while consume(TokenType::LeftBracket, tokens, pos) {
        let len = primary(tokens, pos);
        if let NodeType::Num(n) = len.op {
            v.push(n as usize);
            expect(TokenType::RightBracket, tokens, pos);
        } else {
            panic!("number expected");
        }
    }
    for val in v {
        ty = Box::new(Type::ary_of(ty, val));
    }
    ty
}

fn decl(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    // Read the first half of type name (e.g. `int *`).
    let mut ty = Box::new(ctype(tokens, pos));

    // Read an identifier.
    let name = ident(tokens, pos);
    let init: Option<Box<Node>>;

    // Read the second half of type name (e.g. `[3][5]`).
    ty = read_array(ty, tokens, pos);

    // Read an initializer.
    if consume(TokenType::Equal, tokens, pos) {
        init = Some(Box::new(assign(tokens, pos)));
    } else {
        init = None
    }
    expect(TokenType::Semicolon, tokens, pos);
    let mut node = Node::new(NodeType::Vardef(name.clone(), init, Scope::Local(0)));
    node.ty = ty;
    node
}

fn param(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let ty = Box::new(ctype(tokens, pos));
    let name = ident(tokens, pos);
    let mut node = Node::new(NodeType::Vardef(name.clone(), None, Scope::Local(0)));
    node.ty = ty;
    node
}

fn expr_stmt(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let expr = assign(tokens, pos);
    let node = new_expr!(NodeType::ExprStmt, expr);
    expect(TokenType::Semicolon, tokens, pos);
    node
}

fn stmt(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    match tokens[*pos].ty {
        TokenType::Int | TokenType::Char | TokenType::Struct => return decl(tokens, pos),
        TokenType::If => {
            let mut els = None;
            *pos += 1;
            expect(TokenType::LeftParen, tokens, pos);
            let cond = assign(&tokens, pos);
            expect(TokenType::RightParen, tokens, pos);
            let then = stmt(&tokens, pos);
            if consume(TokenType::Else, tokens, pos) {
                els = Some(Box::new(stmt(&tokens, pos)));
            }
            Node::new(NodeType::If(Box::new(cond), Box::new(then), els))
        }
        TokenType::For => {
            *pos += 1;
            expect(TokenType::LeftParen, tokens, pos);
            let init: Box<Node> = if is_typename(&tokens[*pos]) {
                Box::new(decl(tokens, pos))
            } else {
                Box::new(expr_stmt(tokens, pos))
            };
            let cond = Box::new(assign(&tokens, pos));
            expect(TokenType::Semicolon, tokens, pos);
            let inc = Box::new(new_expr!(NodeType::ExprStmt, assign(&tokens, pos)));
            expect(TokenType::RightParen, tokens, pos);
            let body = Box::new(stmt(&tokens, pos));
            Node::new(NodeType::For(init, cond, inc, body))
        }
        TokenType::While => {
            *pos += 1;
            expect(TokenType::LeftParen, tokens, pos);
            let init = Box::new(Node::new(NodeType::Null));
            let inc = Box::new(Node::new(NodeType::Null));
            let cond = Box::new(assign(&tokens, pos));
            expect(TokenType::RightParen, tokens, pos);
            let body = Box::new(stmt(&tokens, pos));
            Node::new(NodeType::For(init, cond, inc, body))
        }
        TokenType::Do => {
            *pos += 1;
            let body = Box::new(stmt(tokens, pos));
            expect(TokenType::While, tokens, pos);
            expect(TokenType::LeftParen, tokens, pos);
            let cond = Box::new(assign(tokens, pos));
            expect(TokenType::RightParen, tokens, pos);
            expect(TokenType::Semicolon, tokens, pos);
            Node::new(NodeType::DoWhile(body, cond))
        }
        TokenType::Return => {
            *pos += 1;
            let expr = assign(&tokens, pos);
            expect(TokenType::Semicolon, tokens, pos);
            Node::new(NodeType::Return(Box::new(expr)))
        }
        TokenType::LeftBrace => {
            *pos += 1;
            let mut stmts = vec![];
            while !consume(TokenType::RightBrace, tokens, pos) {
                stmts.push(stmt(&tokens, pos));
            }
            Node::new(NodeType::CompStmt(stmts))
        }
        TokenType::Semicolon => {
            *pos += 1;
            Node::new(NodeType::Null)
        }
        _ => {
            let expr = assign(&tokens, pos);
            let node = Node::new(NodeType::ExprStmt(Box::new(expr)));
            expect(TokenType::Semicolon, tokens, pos);
            return node;
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

fn toplevel(tokens: &Vec<Token>, pos: &mut usize) -> Node {
    let is_extern = consume(TokenType::Extern, &tokens, pos);
    let ty = ctype(tokens, pos);
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
            args.push(param(tokens, pos));
            while consume(TokenType::Colon, tokens, pos) {
                args.push(param(tokens, pos));
            }
            expect(TokenType::RightParen, tokens, pos);
        }

        expect(TokenType::LeftBrace, tokens, pos);
        let body = compound_stmt(tokens, pos);
        return Node::new(NodeType::Func(name, args, Box::new(body), 0));
    }

    // Global variable
    let ty = read_array(Box::new(ty), tokens, pos);
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
    node.ty = ty;
    expect(TokenType::Semicolon, tokens, pos);
    node
}

/* e.g.
 function -> param
+---------+
int main() {     ; +-+                        int   []         2
  int ary[2];    ;   |               +->stmt->decl->read_array->primary
  ary[0]=1;      ;   | compound_stmt-+->stmt->...                ary
  return ary[0]; ;   |               +->stmt->assign->postfix-+->primary
}                ; +-+                  return        []      +->primary
                                                                 0
*/
pub fn parse(tokens: &Vec<Token>) -> Vec<Node> {
    let mut pos = 0;

    let mut v = vec![];
    while tokens.len() != pos {
        v.push(toplevel(tokens, &mut pos))
    }
    v
}
