use token::{Token, TokenType};

fn expect(t: &Token, ty: TokenType, pos: &mut usize) {
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

fn consume(tokens: &Vec<Token>, ty: TokenType, pos: &mut usize) -> bool {
    let t = &tokens[*pos];
    if t.ty != ty {
        return false;
    }
    *pos += 1;
    return true;
}

#[derive(Debug, Clone)]
pub enum NodeType {
    Num(i32), // Number literal
    Ident(String), // Identifier
    BinOp(TokenType, Box<Node>, Box<Node>), // left-hand, right-hand
    If(Box<Node>, Box<Node>, Option<Box<Node>>), // cond, then, els
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
}

impl Node {
    fn new(op: NodeType) -> Self {
        Self { ty: op }
    }

    fn term(tokens: &Vec<Token>, pos: &mut usize) -> Self {
        let t = &tokens[*pos];
        *pos += 1;
        match t.ty {
            TokenType::Num(val) => return Self::new(NodeType::Num(val)),
            TokenType::Ident(ref name) => {
                if !consume(tokens, TokenType::LeftParen, pos) {
                    return Self::new(NodeType::Ident(name.clone()));
                }

                let mut args = vec![];
                if consume(tokens, TokenType::RightParen, pos) {
                    return Self::new(NodeType::Call(name.clone(), args));
                }

                args.push(Self::assign(tokens, pos));
                while consume(tokens, TokenType::Colon, pos) {
                    args.push(Self::assign(tokens, pos));
                }
                expect(&tokens[*pos], TokenType::RightParen, pos);
                return Self::new(NodeType::Call(name.clone(), args));
            }
            TokenType::LeftParen => {
                let node = Self::assign(tokens, pos);
                expect(&tokens[*pos], TokenType::RightParen, pos);
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

    fn logand(tokens: &Vec<Token>, pos: &mut usize) -> Self {
        let mut lhs = Self::add(tokens, pos);
        loop {
            if tokens[*pos].ty != TokenType::Logand {
                return lhs;
            }
            *pos += 1;
            lhs = Self::new(NodeType::Logand(
                Box::new(lhs),
                Box::new(Self::add(tokens, pos)),
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
        if consume(tokens, TokenType::Equal, pos) {
            return Self::new(NodeType::BinOp(
                TokenType::Equal,
                Box::new(lhs),
                Box::new(Self::logor(tokens, pos)),
            ));
        }
        return lhs;
    }

    fn stmt(tokens: &Vec<Token>, pos: &mut usize) -> Self {
        match tokens[*pos].ty {
            TokenType::If => {
                let mut els = None;
                *pos += 1;
                expect(&tokens[*pos], TokenType::LeftParen, pos);
                let cond = Self::assign(&tokens, pos);
                expect(&tokens[*pos], TokenType::RightParen, pos);
                let then = Self::stmt(&tokens, pos);
                if consume(tokens, TokenType::Else, pos) {
                    els = Some(Box::new(Self::stmt(&tokens, pos)));
                }
                Self::new(NodeType::If(Box::new(cond), Box::new(then), els))
            }
            TokenType::Return => {
                *pos += 1;
                let expr = Self::assign(&tokens, pos);
                expect(&tokens[*pos], TokenType::Semicolon, pos);
                Self::new(NodeType::Return(Box::new(expr)))
            }
            _ => {
                let expr = Self::assign(&tokens, pos);
                let node = Self::new(NodeType::ExprStmt(Box::new(expr)));
                expect(&tokens[*pos], TokenType::Semicolon, pos);
                return node;
            }
        }
    }

    fn compound_stmt(tokens: &Vec<Token>, pos: &mut usize) -> Self {
        let mut stmts = vec![];

        while !consume(tokens, TokenType::RightBrace, pos) {
            stmts.push(Self::stmt(tokens, pos));
        }
        Self::new(NodeType::CompStmt(stmts))
    }

    fn function(tokens: &Vec<Token>, pos: &mut usize) -> Self {
        let t = tokens[*pos].clone();
        match t.ty {
            TokenType::Ident(name) => {
                *pos += 1;

                let mut args = vec![];
                expect(&tokens[*pos], TokenType::LeftParen, pos);
                if !consume(tokens, TokenType::RightParen, pos) {
                    args.push(Self::term(tokens, pos));
                    while consume(tokens, TokenType::Colon, pos) {
                        args.push(Self::term(tokens, pos));
                    }
                    expect(&tokens[*pos], TokenType::RightParen, pos);
                }

                expect(&tokens[*pos], TokenType::LeftBrace, pos);
                let body = Self::compound_stmt(tokens, pos);
                Node::new(NodeType::Func(name, args, Box::new(body)))
            }
            _ => panic!("function name expected, but got {}", t.input),
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
