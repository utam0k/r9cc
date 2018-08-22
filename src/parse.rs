use token::{Token, TokenType};

#[derive(Debug, Clone, PartialEq)]
pub enum NodeType {
    Num, // Number literal
    Plus,
    Minus,
    Mul,
    Div,
}

impl From<TokenType> for NodeType {
    fn from(token_type: TokenType) -> Self {
        match token_type {
            TokenType::Num => NodeType::Num,
            TokenType::Plus => NodeType::Plus,
            TokenType::Minus => NodeType::Minus,
            TokenType::Mul => NodeType::Mul,
            TokenType::Div => NodeType::Div,
        }
    }
}

impl Default for NodeType {
    fn default() -> Self {
        NodeType::Num
    }
}

#[derive(Default, Debug, Clone)]
pub struct Node {
    pub ty: NodeType, // Node type
    pub lhs: Option<Box<Node>>, // left-hand side. If None, Node is number etc.
    pub rhs: Option<Box<Node>>, // right-hand side
    pub val: i32, // Number literal
}

impl Node {
    fn new(op: NodeType, lhs: Box<Node>, rhs: Box<Node>) -> Self {
        Self {
            ty: op,
            lhs: Some(lhs),
            rhs: Some(rhs),
            ..Default::default()
        }
    }

    fn new_num(val: i32) -> Self {
        Self {
            ty: NodeType::Num,
            val: val,
            ..Default::default()
        }
    }

    fn number(tokens: &Vec<Token>, pos: usize) -> Self {
        let t = &tokens[pos];
        if t.ty != TokenType::Num {
            panic!("number expected, but got {}", t.input);
        }
        return Self::new_num(t.val);
    }

    fn mul(tokens: &Vec<Token>, mut pos: usize) -> (Self, usize) {
        let mut lhs = Self::number(&tokens, pos);
        pos += 1;

        loop {
            if tokens.len() == pos {
                return (lhs, pos);
            }

            let op = tokens[pos].ty.clone();
            if op != TokenType::Mul && op != TokenType::Div {
                return (lhs, pos);
            }
            pos += 1;
            lhs = Self::new(
                NodeType::from(op),
                Box::new(lhs),
                Box::new(Self::number(&tokens, pos)),
            );
            pos += 1;
        }
    }

    fn expr(tokens: &Vec<Token>, pos: usize) -> (Self, usize) {
        let (mut lhs, mut pos) = Self::mul(&tokens, pos);

        loop {
            if tokens.len() == pos {
                return (lhs, pos);
            }

            let op = tokens[pos].ty.clone();
            if op != TokenType::Plus && op != TokenType::Minus {
                return (lhs, pos);
            }
            pos += 1;
            let (rhs, new_pos) = Self::mul(&tokens, pos);
            pos = new_pos;
            lhs = Self::new(NodeType::from(op), Box::new(lhs), Box::new(rhs));
        }
    }

    pub fn parse(tokens: &Vec<Token>) -> Self {
        let (node, pos) = Self::expr(tokens, 0);

        if tokens.len() != pos {
            panic!("stray token: {}", tokens[pos].input);
        }
        return node;
    }
}
