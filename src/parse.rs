use token::Token;
use util::roundup;
use {Ctype, Scope, TokenType, Type};

use std::collections::HashMap;

// Quoted from 9cc
// > This is a recursive-descendent parser which constructs abstract
// > syntax tree from input tokens.
//
// > This parser knows only about BNF of the C grammer and doesn't care
// > about its semantics. Therefore, some invalid expressions, such as
// > `1+2=3`, are accepted by this parser, but that's intentional.
// > Semantic errors are detected in a later pass.

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
    let mut parser = Parser::new(tokens);

    let mut v = vec![];
    while tokens.len() != parser.pos {
        if let Some(node) = parser.toplevel() {
            v.push(node);
        }
    }
    v
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

macro_rules! new_expr(
    ($i:path, $expr:expr) => (
        Node::new($i(Box::new($expr)))
    )
);

#[derive(Debug, Clone)]
pub enum NodeType {
    Num(i32),                                        // Number literal
    Str(String, usize),                              // String literal, (data, len)
    Ident(String),                                   // Identifier
    Decl(String),                                    // declaration
    Vardef(String, Option<Box<Node>>, Scope),        // Variable definition, name = init
    Lvar(Scope),                                     // Variable reference
    Gvar(String, String, usize),                     // Variable reference, (name, data, len)
    BinOp(TokenType, Box<Node>, Box<Node>),          // left-hand, right-hand
    If(Box<Node>, Box<Node>, Option<Box<Node>>),     // "if" ( cond ) then "else" els
    Ternary(Box<Node>, Box<Node>, Box<Node>),        // cond ? then : els
    For(Box<Node>, Box<Node>, Box<Node>, Box<Node>), // "for" ( init; cond; inc ) body
    Break,
    DoWhile(Box<Node>, Box<Node>), // do { body } while(cond)
    Addr(Box<Node>),               // address-of operator("&"), expr
    Deref(Box<Node>),              // pointer dereference ("*"), expr
    Dot(Box<Node>, String, usize), // Struct member accessm, (expr, name, offset)
    Exclamation(Box<Node>),        // !, expr
    Neg(Box<Node>),                // -
    PostInc(Box<Node>),            // post ++
    PostDec(Box<Node>),            // post --
    Return(Box<Node>),             // "return", stmt
    Sizeof(Box<Node>),             // "sizeof", expr
    Alignof(Box<Node>),            // "_Alignof", expr
    Call(String, Vec<Node>),       // Function call(name, args)
    Func(String, Vec<Node>, Box<Node>, usize), // Function definition(name, args, body, stacksize)
    CompStmt(Vec<Node>),           // Compound statement
    VecStmt(Vec<Node>),            // For the purpose of assign a value when initializing an array.
    ExprStmt(Box<Node>),           // Expression statement
    StmtExpr(Box<Node>),           // Statement expression (GNU extn.)
    Null,
}

#[derive(Debug, Clone)]
pub struct Node {
    pub op: NodeType,  // Node type
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

pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    pos: usize,
    env: Env,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Parser {
            tokens,
            pos: 0,
            env: Env::new(None),
        }
    }

    fn find_tag(&self, name: &str) -> Option<Type> {
        let mut next: &Option<Box<Env>> = &Some(Box::new(self.env.clone()));
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

    fn find_typedef(&self, name: &str) -> Option<Type> {
        let mut next: &Option<Box<Env>> = &Some(Box::new(self.env.clone()));
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

    fn expect(&mut self, ty: TokenType) {
        let t = &self.tokens[self.pos];
        if t.ty != ty {
            t.bad_token(&format!("{:?} expected", ty));
        }
        self.pos += 1;
    }

    fn consume(&mut self, ty: TokenType) -> bool {
        let t = &self.tokens[self.pos];
        if t.ty != ty {
            return false;
        }
        self.pos += 1;
        true
    }

    fn is_typename(&self, t: &Token) -> bool {
        use self::TokenType::*;
        if let TokenType::Ident(ref name) = t.ty {
            return self.find_typedef(name).is_some();
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
        (off, align)
    }

    fn add_member(ty: &mut Type, mut members: Vec<Node>) {
        let (off, align) = Self::set_offset(&mut members);
        if let Ctype::Struct(ref mut members2) = ty.ty {
            *members2 = members;
        }
        ty.size = roundup(off, align);
    }

    fn decl_specifiers(&mut self) -> Option<Type> {
        let t = &self.tokens[self.pos];
        self.pos += 1;
        match t.ty {
            TokenType::Ident(ref name) => {
                if let Some(ty) = self.find_typedef(name) {
                    return Some(ty.clone());
                } else {
                    self.pos -= 1;
                    return None;
                }
            }
            TokenType::Int => Some(Type::int_ty()),
            TokenType::Char => Some(Type::char_ty()),
            TokenType::Void => Some(Type::void_ty()),
            TokenType::Struct => {
                let mut tag_may: Option<String> = None;
                let t = &self.tokens[self.pos];
                if let TokenType::Ident(ref name) = t.ty {
                    self.pos += 1;
                    tag_may = Some(name.clone())
                }

                let mut members = vec![];
                if self.consume(TokenType::LeftBrace) {
                    while !self.consume(TokenType::RightBrace) {
                        members.push(self.declaration())
                    }
                }

                let mut ty_may: Option<Type> = None;
                if let Some(ref tag) = tag_may {
                    if members.is_empty() {
                        ty_may = self.find_tag(&tag);
                    }
                }
                let mut ty = ty_may.unwrap_or(Type::new(Ctype::Struct(vec![]), 10));

                if !members.is_empty() {
                    Self::add_member(&mut ty, members);
                    if let Some(tag) = tag_may {
                        self.env.tags.insert(tag, ty.clone());
                    }
                }
                Some(ty.clone())
            }
            _ => t.bad_token("typename expected"),
        }
    }

    fn ident(&mut self) -> String {
        let t = &self.tokens[self.pos];
        if let TokenType::Ident(ref name) = t.ty {
            self.pos += 1;
            name.clone()
        } else {
            t.bad_token("variable name expected");
        }
    }

    fn primary(&mut self) -> Node {
        let t = &self.tokens[self.pos];
        self.pos += 1;
        match t.ty {
            TokenType::Num(val) => Node::new_num(val),
            TokenType::Str(ref str, len) => {
                let mut node = Node::new(NodeType::Str(str.clone(), len));
                node.ty = Box::new(Type::ary_of(Box::new(Type::char_ty()), len));
                node
            }
            TokenType::Ident(ref name) => {
                if !self.consume(TokenType::LeftParen) {
                    return Node::new(NodeType::Ident(name.clone()));
                }

                let mut args = vec![];
                if self.consume(TokenType::RightParen) {
                    return Node::new(NodeType::Call(name.clone(), args));
                }

                args.push(self.assign());
                while self.consume(TokenType::Comma) {
                    args.push(self.assign());
                }
                self.expect(TokenType::RightParen);
                Node::new(NodeType::Call(name.clone(), args))
            }
            TokenType::LeftParen => {
                if self.consume(TokenType::LeftBrace) {
                    let stmt = Box::new(self.compound_stmt());
                    self.expect(TokenType::RightParen);
                    return Node::new(NodeType::StmtExpr(stmt));
                }
                let node = self.expr();
                self.expect(TokenType::RightParen);
                node
            }
            _ => t.bad_token("number expected"),
        }
    }

    fn postfix(&mut self) -> Node {
        let mut lhs = self.primary();

        loop {
            if self.consume(TokenType::Inc) {
                lhs = new_expr!(NodeType::PostInc, lhs);
                continue;
            }

            if self.consume(TokenType::Dec) {
                lhs = new_expr!(NodeType::PostDec, lhs);
                continue;
            }

            if self.consume(TokenType::Dot) {
                // TODO: Use new_expr!
                lhs = Node::new(NodeType::Dot(Box::new(lhs), self.ident(), 0));
                continue;
            }

            if self.consume(TokenType::Arrow) {
                lhs = Node::new(NodeType::Dot(
                    Box::new(new_expr!(NodeType::Deref, lhs)),
                    self.ident(),
                    0,
                ));
                continue;
            }

            if self.consume(TokenType::LeftBracket) {
                lhs = new_expr!(
                    NodeType::Deref,
                    Node::new_binop(TokenType::Plus, lhs, self.assign())
                );
                self.expect(TokenType::RightBracket);
                continue;
            }
            return lhs;
        }
    }

    fn unary(&mut self) -> Node {
        if self.consume(TokenType::Minus) {
            return new_expr!(NodeType::Neg, self.unary());
        }
        if self.consume(TokenType::Mul) {
            return new_expr!(NodeType::Deref, self.unary());
        }
        if self.consume(TokenType::And) {
            return new_expr!(NodeType::Addr, self.unary());
        }
        if self.consume(TokenType::Exclamation) {
            return new_expr!(NodeType::Exclamation, self.unary());
        }
        if self.consume(TokenType::Sizeof) {
            return new_expr!(NodeType::Sizeof, self.unary());
        }
        if self.consume(TokenType::Alignof) {
            return new_expr!(NodeType::Alignof, self.unary());
        }

        if self.consume(TokenType::Inc) {
            return Node::new_binop(TokenType::AddEQ, self.unary(), Node::new_num(1));
        }
        if self.consume(TokenType::Dec) {
            return Node::new_binop(TokenType::SubEQ, self.unary(), Node::new_num(1));
        }

        self.postfix()
    }

    fn mul(&mut self) -> Node {
        let mut lhs = self.unary();

        loop {
            if self.consume(TokenType::Mul) {
                lhs = Node::new_binop(TokenType::Mul, lhs, self.unary());
            } else if self.consume(TokenType::Div) {
                lhs = Node::new_binop(TokenType::Div, lhs, self.unary());
            } else if self.consume(TokenType::Mod) {
                lhs = Node::new_binop(TokenType::Mod, lhs, self.unary());
            } else {
                return lhs;
            }
        }
    }

    fn add(&mut self) -> Node {
        let mut lhs = self.mul();

        loop {
            if self.consume(TokenType::Plus) {
                lhs = Node::new_binop(TokenType::Plus, lhs, self.mul());
            } else if self.consume(TokenType::Minus) {
                lhs = Node::new_binop(TokenType::Minus, lhs, self.mul());
            } else {
                return lhs;
            }
        }
    }

    fn shift(&mut self) -> Node {
        let mut lhs = self.add();
        loop {
            if self.consume(TokenType::SHL) {
                lhs = Node::new_binop(TokenType::SHL, lhs, self.add());
            } else if self.consume(TokenType::SHR) {
                lhs = Node::new_binop(TokenType::SHR, lhs, self.add());
            } else {
                return lhs;
            }
        }
    }

    fn relational(&mut self) -> Node {
        let mut lhs = self.shift();
        loop {
            if self.consume(TokenType::LeftAngleBracket) {
                lhs = Node::new_binop(TokenType::LeftAngleBracket, lhs, self.shift());
            } else if self.consume(TokenType::RightAngleBracket) {
                lhs = Node::new_binop(TokenType::LeftAngleBracket, self.shift(), lhs);
            } else if self.consume(TokenType::LE) {
                lhs = Node::new_binop(TokenType::LE, lhs, self.shift())
            } else if self.consume(TokenType::GE) {
                lhs = Node::new_binop(TokenType::LE, self.shift(), lhs);
            } else {
                return lhs;
            }
        }
    }

    fn equality(&mut self) -> Node {
        let mut lhs = self.relational();
        loop {
            if self.consume(TokenType::EQ) {
                lhs = Node::new_binop(TokenType::EQ, lhs, self.relational());
            } else if self.consume(TokenType::NE) {
                lhs = Node::new_binop(TokenType::NE, lhs, self.relational());
            } else {
                return lhs;
            }
        }
    }

    fn bit_and(&mut self) -> Node {
        let mut lhs = self.equality();
        while self.consume(TokenType::And) {
            lhs = Node::new_binop(TokenType::And, lhs, self.equality());
        }
        lhs
    }

    fn bit_xor(&mut self) -> Node {
        let mut lhs = self.bit_and();
        while self.consume(TokenType::Hat) {
            lhs = Node::new_binop(TokenType::Hat, lhs, self.bit_and());
        }
        lhs
    }

    fn bit_or(&mut self) -> Node {
        let mut lhs = self.bit_xor();
        while self.consume(TokenType::VerticalBar) {
            lhs = Node::new_binop(TokenType::VerticalBar, lhs, self.bit_xor());
        }
        lhs
    }

    fn logand(&mut self) -> Node {
        let mut lhs = self.bit_or();
        while self.consume(TokenType::Logand) {
            lhs = Node::new_binop(TokenType::Logand, lhs, self.logand());
        }
        lhs
    }

    fn logor(&mut self) -> Node {
        let mut lhs = self.logand();
        while self.consume(TokenType::Logor) {
            lhs = Node::new_binop(TokenType::Logor, lhs, self.logand());
        }
        lhs
    }

    fn conditional(&mut self) -> Node {
        let cond = self.logor();
        if !self.consume(TokenType::Question) {
            return cond;
        }
        let then = self.expr();
        self.expect(TokenType::Colon);
        let els = self.conditional();
        Node::new(NodeType::Ternary(
            Box::new(cond),
            Box::new(then),
            Box::new(els),
        ))
    }

    fn assign_op(ty: &TokenType) -> Option<&TokenType> {
        use self::TokenType::*;
        match ty {
            Equal | MulEQ | DivEQ | ModEQ | AddEQ | SubEQ | ShlEQ | ShrEQ | BitandEQ | XorEQ
            | BitorEQ => Some(ty),
            _ => None,
        }
    }

    fn assign(&mut self) -> Node {
        let lhs = self.conditional();
        if let Some(op) = Self::assign_op(&self.tokens[self.pos].ty) {
            self.pos += 1;
            Node::new_binop(op.clone(), lhs, self.assign())
        } else {
            lhs
        }
    }

    fn expr(&mut self) -> Node {
        let lhs = self.assign();
        if !self.consume(TokenType::Comma) {
            return lhs;
        }
        Node::new_binop(TokenType::Comma, lhs, self.expr())
    }

    fn ctype(&mut self) -> Type {
        let t = &self.tokens[self.pos];
        if let Some(mut ty) = self.decl_specifiers() {
            while self.consume(TokenType::Mul) {
                ty = Type::ptr_to(Box::new(ty));
            }
            ty
        } else {
            t.bad_token("typename expected");
        }
    }

    fn read_array(&mut self, mut ty: Box<Type>) -> Type {
        let mut v: Vec<usize> = vec![];
        while self.consume(TokenType::LeftBracket) {
            if self.consume(TokenType::RightBracket) {
                v.push(0); // temporary value
                continue;
            }

            let len = self.expr();
            if let NodeType::Num(n) = len.op {
                v.push(n as usize);
                self.expect(TokenType::RightBracket);
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

    fn array_init_rval(&mut self, ident: Node) -> Node {
        let mut init = vec![];
        let mut i = 0;
        loop {
            let val = self.primary();
            let node = new_expr!(
                NodeType::Deref,
                Node::new_binop(TokenType::Plus, ident.clone(), Node::new(NodeType::Num(i)))
            );
            init.push(Node::new(NodeType::ExprStmt(Box::new(Node::new_binop(
                TokenType::Equal,
                node,
                val,
            )))));
            if !self.consume(TokenType::Comma) {
                break;
            }
            i += 1;
        }
        self.expect(TokenType::RightBrace);
        Node::new(NodeType::VecStmt(init))
    }

    fn update_ptr_to(&mut self, src: &mut Box<Type>, dst: Box<Type>) {
        match src.ty {
            Ctype::Ptr(ref mut ptr_to) => self.update_ptr_to(ptr_to, dst),
            _ => *src = dst,
        }
    }

    fn direct_decl(&mut self, ty: Box<Type>) -> Node {
        let t = &self.tokens[self.pos];
        let mut placeholder = Box::new(Type::default());
        let mut node;

        if let TokenType::Ident(_) = t.ty {
            node = Node::new(NodeType::Vardef(self.ident(), None, Scope::Local(0)));
        } else if self.consume(TokenType::LeftParen) {
            node = self.declarator(&mut placeholder);
            self.expect(TokenType::RightParen);
        } else {
            t.bad_token("bad direct-declarator");
        }

        // Read the second half of type name (e.g. `[3][5]`).
        let ty = self.read_array(ty);
        self.update_ptr_to(&mut node.ty, Box::new(ty));

        // Read an initializer.
        let init: Option<Box<Node>>;
        if self.consume(TokenType::Equal) {
            // Assign a value when initializing an array.
            if let TokenType::Ident(ref name) = t.ty {
                if self.consume(TokenType::LeftBrace) {
                    let mut stmts = vec![];
                    let mut ary_declaration =
                        Node::new(NodeType::Vardef(name.clone(), None, Scope::Local(0)));
                    ary_declaration.ty = node.ty;
                    stmts.push(ary_declaration);
                    let init_ary = self.array_init_rval(Node::new(NodeType::Ident(name.clone())));
                    stmts.push(init_ary);
                    return Node::new(NodeType::VecStmt(stmts));
                }
            }

            init = Some(Box::new(self.assign()));
            match node.op {
                NodeType::Vardef(_, ref mut init2, _) => *init2 = init,
                _ => unreachable!(),
            }
        }
        node
    }

    fn declarator(&mut self, ty: &mut Type) -> Node {
        while self.consume(TokenType::Mul) {
            *ty = Type::ptr_to(Box::new(ty.clone()));
        }
        self.direct_decl(Box::new(ty.clone()))
    }

    fn declaration(&mut self) -> Node {
        let mut ty = self.decl_specifiers().unwrap();
        let node = self.declarator(&mut ty);
        self.expect(TokenType::Semicolon);
        node
    }

    fn param_declaration(&mut self) -> Node {
        let mut ty = self.decl_specifiers().unwrap();
        let mut node = self.declarator(&mut ty);
        if let Ctype::Ary(ary_of, _) = node.ty.ty {
            node.ty = Box::new(Type::ptr_to(ary_of));
        }
        node
    }

    fn expr_stmt(&mut self) -> Node {
        let expr = self.expr();
        let node = new_expr!(NodeType::ExprStmt, expr);
        self.expect(TokenType::Semicolon);
        node
    }

    fn stmt(&mut self) -> Node {
        let t = &self.tokens[self.pos];
        self.pos += 1;

        match t.ty {
            TokenType::Typedef => {
                let node = self.declaration();
                if let NodeType::Vardef(name, _, _) = node.op {
                    self.env.typedefs.insert(name, *node.ty);
                    return Node::new(NodeType::Null);
                } else {
                    unreachable!();
                }
            }
            TokenType::If => {
                let mut els = None;
                self.expect(TokenType::LeftParen);
                let cond = self.expr();
                self.expect(TokenType::RightParen);
                let then = self.stmt();
                if self.consume(TokenType::Else) {
                    els = Some(Box::new(self.stmt()));
                }
                Node::new(NodeType::If(Box::new(cond), Box::new(then), els))
            }
            TokenType::For => {
                self.expect(TokenType::LeftParen);

                let init: Box<Node> = if self.is_typename(&self.tokens[self.pos]) {
                    Box::new(self.declaration())
                } else if self.consume(TokenType::Semicolon) {
                    Box::new(Node::new(NodeType::Null))
                } else {
                    Box::new(self.expr_stmt())
                };

                let cond;
                if !self.consume(TokenType::Semicolon) {
                    cond = Box::new(self.expr());
                    self.expect(TokenType::Semicolon);
                } else {
                    cond = Box::new(Node::new(NodeType::Null))
                }

                let inc;
                if !self.consume(TokenType::RightParen) {
                    inc = Box::new(new_expr!(NodeType::ExprStmt, self.expr()));
                    self.expect(TokenType::RightParen);
                } else {
                    inc = Box::new(Node::new(NodeType::Null))
                }

                let body = Box::new(self.stmt());
                Node::new(NodeType::For(init, cond, inc, body))
            }
            TokenType::While => {
                self.expect(TokenType::LeftParen);
                let init = Box::new(Node::new(NodeType::Null));
                let inc = Box::new(Node::new(NodeType::Null));
                let cond = Box::new(self.expr());
                self.expect(TokenType::RightParen);
                let body = Box::new(self.stmt());
                Node::new(NodeType::For(init, cond, inc, body))
            }
            TokenType::Do => {
                let body = Box::new(self.stmt());
                self.expect(TokenType::While);
                self.expect(TokenType::LeftParen);
                let cond = Box::new(self.expr());
                self.expect(TokenType::RightParen);
                self.expect(TokenType::Semicolon);
                Node::new(NodeType::DoWhile(body, cond))
            }
            TokenType::Break => Node::new(NodeType::Break),
            TokenType::Return => {
                let expr = self.expr();
                self.expect(TokenType::Semicolon);
                Node::new(NodeType::Return(Box::new(expr)))
            }
            TokenType::LeftBrace => {
                let mut stmts = vec![];
                while !self.consume(TokenType::RightBrace) {
                    stmts.push(self.stmt());
                }
                Node::new(NodeType::CompStmt(stmts))
            }
            TokenType::Semicolon => Node::new(NodeType::Null),
            _ => {
                self.pos -= 1;
                if self.is_typename(&self.tokens[self.pos]) {
                    return self.declaration();
                }
                self.expr_stmt()
            }
        }
    }

    fn compound_stmt(&mut self) -> Node {
        let mut stmts = vec![];

        let new_env = Env::new(Some(Box::new(self.env.clone())));
        self.env = new_env;
        while !self.consume(TokenType::RightBrace) {
            stmts.push(self.stmt());
        }
        let next = self.env.next.clone();
        self.env = *next.unwrap();
        Node::new(NodeType::CompStmt(stmts))
    }

    fn toplevel(&mut self) -> Option<Node> {
        let is_typedef = self.consume(TokenType::Typedef);
        let is_extern = self.consume(TokenType::Extern);

        let mut ty = self.ctype();
        let t = &self.tokens[self.pos];
        let name: String;
        if let TokenType::Ident(ref name2) = t.ty {
            name = name2.clone();
        } else {
            t.bad_token("function or variable name expected");
        }
        self.pos += 1;

        // Function
        if self.consume(TokenType::LeftParen) {
            let mut args = vec![];
            if !self.consume(TokenType::RightParen) {
                args.push(self.param_declaration());
                while self.consume(TokenType::Comma) {
                    args.push(self.param_declaration());
                }
                self.expect(TokenType::RightParen);
            }

            if self.consume(TokenType::Semicolon) {
                let mut node = Node::new(NodeType::Decl(name));
                node.ty = Box::new(Type::new(Ctype::Func(Box::new(ty)), 0));
                return Some(node);
            }

            let t = &self.tokens[self.pos];
            self.expect(TokenType::LeftBrace);
            if is_typedef {
                t.bad_token("typedef {} has function definition");
            }
            let body = self.compound_stmt();

            let mut node = Node::new(NodeType::Func(name, args, Box::new(body), 0));
            node.ty = Box::new(Type::new(Ctype::Func(Box::new(ty)), 0));
            return Some(node);
        }

        ty = self.read_array(Box::new(ty));
        self.expect(TokenType::Semicolon);

        if is_typedef {
            self.env.typedefs.insert(name.clone(), ty.clone());
            return None;
        }

        // Global variable
        let mut node = Node::new(NodeType::Vardef(
            name,
            None,
            if is_extern {
                Scope::Global(String::new(), 0, true)
            } else {
                Scope::Global(String::new(), ty.size, false)
            },
        ));
        node.ty = Box::new(ty);
        Some(node)
    }
}
