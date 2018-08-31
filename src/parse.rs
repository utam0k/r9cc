use token::{Token, TokenType};

fn expect(ty: TokenType, t: &Token, pos: &mut usize) {
    if t.ty != ty {
        panic!(
            "{:?} ({:?}) expected, but got {:?} ({:?})",
            ty,
            ty,
            t.ty,
            t.ty
        );
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
    t.ty == TokenType::Int
}

#[derive(Debug, Clone)]
pub enum NodeType {
    Num(i32), // Number literal
    Ident(String), // Identifier
    Vardef(String, Option<Box<Node>>), // Variable definition, name = init
    Lvar, // Variable reference
    BinOp(TokenType, Box<Node>, Box<Node>), // left-hand, right-hand
    If(Box<Node>, Box<Node>, Option<Box<Node>>), // "if" ( cond ) then "else" els
    For(Box<Node>, Box<Node>, Box<Node>, Box<Node>), // "for" ( init; cond; inc ) body
    Logand(Box<Node>, Box<Node>), // left-hand, right-hand
    Logor(Box<Node>, Box<Node>), // left-hand, right-hand
    Return(Box<Node>), // stmt
    Call(String, Vec<Node>), // Function call(name, args)
    Func(String, Vec<Node>, Box<Node>), // Function definition(name, args, body)
    ExprStmt(Box<Node>), // expresson stmt
    CompStmt(Vec<Node>), // Compound statement
}

#[derive(Debug, Clone)]
pub struct Node {
    pub ty: NodeType, // Node type

    // Function definition
    pub stacksize: usize,

    // Local variable
    pub offset: usize,
}

impl Node {
    fn new(op: NodeType) -> Self {
        Self {
            ty: op,
            stacksize: 0,
            offset: 0,
        }
    }

    fn term(tokens: &Vec<Token>, pos: &mut usize) -> Self {
        let t = &tokens[*pos];
        *pos += 1;
        match t.ty {
            TokenType::Num(val) => return Self::new(NodeType::Num(val)),
            TokenType::Ident(ref name) => {
                if !consume(TokenType::LeftParen, tokens, pos) {
                    return Self::new(NodeType::Ident(name.clone()));
                }

                let mut args = vec![];
                if consume(TokenType::RightParen, tokens, pos) {
                    return Self::new(NodeType::Call(name.clone(), args));
                }

                args.push(Self::assign(tokens, pos));
                while consume(TokenType::Colon, tokens, pos) {
                    args.push(Self::assign(tokens, pos));
                }
                expect(TokenType::RightParen, &tokens[*pos], pos);
                return Self::new(NodeType::Call(name.clone(), args));
            }
            TokenType::LeftParen => {
                let node = Self::assign(tokens, pos);
                expect(TokenType::RightParen, &tokens[*pos], pos);
                node
            }
            _ => panic!("number expected, but got {}", t.input),
        }
    }

    fn mul(tokens: &Vec<Token>, pos: &mut usize) -> Self {
        let mut lhs = Self::term(&tokens, pos);

        loop {
            if tokens.len() == *pos {
                return lhs;
            }

            let t = &tokens[*pos];
            if t.ty != TokenType::Mul && t.ty != TokenType::Div {
                return lhs;
            }
            *pos += 1;
            lhs = Self::new(NodeType::BinOp(
                t.ty.clone(),
                Box::new(lhs),
                Box::new(Self::term(&tokens, pos)),
            ));
        }
    }

    fn add(tokens: &Vec<Token>, pos: &mut usize) -> Self {
        let mut lhs = Self::mul(&tokens, pos);

        loop {
            if tokens.len() == *pos {
                return lhs;
            }

            let t = &tokens[*pos];
            if t.ty != TokenType::Plus && t.ty != TokenType::Minus {
                return lhs;
            }
            *pos += 1;
            let rhs = Self::mul(&tokens, pos);
            lhs = Self::new(NodeType::BinOp(t.ty.clone(), Box::new(lhs), Box::new(rhs)));
        }
    }

    fn rel(tokens: &Vec<Token>, pos: &mut usize) -> Self {
        let mut lhs = Self::add(tokens, pos);
        loop {
            let t = &tokens[*pos];
            if t.ty == TokenType::LeftAngleBracket {
                *pos += 1;
                lhs = Self::new(NodeType::BinOp(
                    TokenType::LeftAngleBracket,
                    Box::new(lhs),
                    Box::new(Self::add(tokens, pos)),
                ));
                continue;
            }

            if t.ty == TokenType::RightAngleBracket {
                *pos += 1;
                lhs = Self::new(NodeType::BinOp(
                    TokenType::LeftAngleBracket,
                    Box::new(Self::add(tokens, pos)),
                    Box::new(lhs),
                ));
                continue;
            }

            return lhs;
        }
    }

    fn logand(tokens: &Vec<Token>, pos: &mut usize) -> Self {
        let mut lhs = Self::rel(tokens, pos);
        loop {
            if tokens[*pos].ty != TokenType::Logand {
                return lhs;
            }
            *pos += 1;
            lhs = Self::new(NodeType::Logand(
                Box::new(lhs),
                Box::new(Self::rel(tokens, pos)),
            ));
        }
    }

    fn logor(tokens: &Vec<Token>, pos: &mut usize) -> Self {
        let mut lhs = Self::logand(tokens, pos);
        loop {
            if tokens[*pos].ty != TokenType::Logor {
                return lhs;
            }
            *pos += 1;
            lhs = Self::new(NodeType::Logor(
                Box::new(lhs),
                Box::new(Self::logand(tokens, pos)),
            ));
        }
    }

    fn assign(tokens: &Vec<Token>, pos: &mut usize) -> Self {
        let lhs = Self::logor(tokens, pos);
        if consume(TokenType::Equal, tokens, pos) {
            return Self::new(NodeType::BinOp(
                TokenType::Equal,
                Box::new(lhs),
                Box::new(Self::logor(tokens, pos)),
            ));
        }
        return lhs;
    }

    fn decl(tokens: &Vec<Token>, pos: &mut usize) -> Self {
        *pos += 1;

        let t = &tokens[*pos];
        if let TokenType::Ident(ref name) = t.ty {
            *pos += 1;

            let init: Option<Box<Node>>;
            if consume(TokenType::Equal, tokens, pos) {
                init = Some(Box::new(Self::assign(tokens, pos)));
            } else {
                init = None
            }
            expect(TokenType::Semicolon, &tokens[*pos], pos);
            return Node::new(NodeType::Vardef(name.clone(), init));
        } else {
            panic!("variable name expected, but got {}", t.input);
        }
    }

    fn param(tokens: &Vec<Token>, pos: &mut usize) -> Self {
        *pos += 1;

        let t = &tokens[*pos];
        if let TokenType::Ident(ref name) = t.ty {
            *pos += 1;
            return Node::new(NodeType::Vardef(name.clone(), None));
        } else {
            panic!("parameter name expected, but got {}", t.input);
        }
    }

    fn expr_stmt(tokens: &Vec<Token>, pos: &mut usize) -> Self {
        let expr = Self::assign(tokens, pos);
        expect(TokenType::Semicolon, &tokens[*pos], pos);
        Node::new(NodeType::ExprStmt(Box::new(expr)))
    }

    fn stmt(tokens: &Vec<Token>, pos: &mut usize) -> Self {
        match tokens[*pos].ty {
            TokenType::Int => return Self::decl(tokens, pos),
            TokenType::If => {
                let mut els = None;
                *pos += 1;
                expect(TokenType::LeftParen, &tokens[*pos], pos);
                let cond = Self::assign(&tokens, pos);
                expect(TokenType::RightParen, &tokens[*pos], pos);
                let then = Self::stmt(&tokens, pos);
                if consume(TokenType::Else, tokens, pos) {
                    els = Some(Box::new(Self::stmt(&tokens, pos)));
                }
                Self::new(NodeType::If(Box::new(cond), Box::new(then), els))
            }
            TokenType::For => {
                *pos += 1;
                expect(TokenType::LeftParen, &tokens[*pos], pos);
                let init: Box<Node>;
                if is_typename(&tokens[*pos]) {
                    init = Box::new(Self::decl(tokens, pos));
                } else {
                    init = Box::new(Self::expr_stmt(tokens, pos));
                }
                let cond = Box::new(Self::assign(&tokens, pos));
                expect(TokenType::Semicolon, &tokens[*pos], pos);
                let inc = Box::new(Self::assign(&tokens, pos));
                expect(TokenType::RightParen, &tokens[*pos], pos);
                let body = Box::new(Self::stmt(&tokens, pos));
                Self::new(NodeType::For(init, cond, inc, body))
            }
            TokenType::Return => {
                *pos += 1;
                let expr = Self::assign(&tokens, pos);
                expect(TokenType::Semicolon, &tokens[*pos], pos);
                Self::new(NodeType::Return(Box::new(expr)))
            }
            TokenType::LeftBrace => {
                *pos += 1;
                let mut stmts = vec![];
                while !consume(TokenType::RightBrace, tokens, pos) {
                    stmts.push(Self::stmt(&tokens, pos));
                }
                Self::new(NodeType::CompStmt(stmts))
            }
            _ => {
                let expr = Self::assign(&tokens, pos);
                let node = Self::new(NodeType::ExprStmt(Box::new(expr)));
                expect(TokenType::Semicolon, &tokens[*pos], pos);
                return node;
            }
        }
    }

    fn compound_stmt(tokens: &Vec<Token>, pos: &mut usize) -> Self {
        let mut stmts = vec![];

        while !consume(TokenType::RightBrace, tokens, pos) {
            stmts.push(Self::stmt(tokens, pos));
        }
        Self::new(NodeType::CompStmt(stmts))
    }

    fn function(tokens: &Vec<Token>, pos: &mut usize) -> Self {
        match tokens[*pos].ty {
            TokenType::Int => {
                *pos += 1;
                let t = &tokens[*pos];
                match t.ty {
                    TokenType::Ident(ref name) => {
                        *pos += 1;

                        let mut args = vec![];
                        expect(TokenType::LeftParen, &tokens[*pos], pos);
                        if !consume(TokenType::RightParen, tokens, pos) {
                            args.push(Self::param(tokens, pos));
                            while consume(TokenType::Colon, tokens, pos) {
                                args.push(Self::param(tokens, pos));
                            }
                            expect(TokenType::RightParen, &tokens[*pos], pos);
                        }

                        expect(TokenType::LeftBrace, &tokens[*pos], pos);
                        let body = Self::compound_stmt(tokens, pos);
                        Node::new(NodeType::Func(name.clone(), args, Box::new(body)))
                    }
                    _ => panic!("function name expected, but got {}", t.input),
                }
            }
            _ => {
                panic!(
                    "function return type expected, but got {}",
                    tokens[*pos].input
                )
            }
        }
    }

    pub fn parse(tokens: &Vec<Token>) -> Vec<Self> {
        let mut pos = 0;

        let mut v = vec![];
        while tokens.len() != pos {
            v.push(Self::function(tokens, &mut pos))
        }
        v
    }
}
