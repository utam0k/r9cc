// Tokenizer
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Num(i32), // Number literal
    Ident(String), // Identifier
    Int, // "int"
    Plus, // +
    Minus, // -
    Mul, // *
    Div, // /
    If, // "if"
    Else, // "else"
    For, // "for"
    Semicolon, // ;
    LeftParen, // (
    RightParen, // )
    LeftBracket, // [
    RightBracket, // ]
    LeftBrace, // {
    RightBrace, // {
    LeftAngleBracket, // <
    RightAngleBracket, // >
    Equal, // =
    Logor, // ||
    Logand, // &&
    Return, // "return"
    Colon, // ,
}

// impl From<char> for TokenType {
impl TokenType {
    fn new_single_letter(c: char) -> Option<Self> {
        use self::TokenType::*;
        match c {
            '+' => Some(Plus),
            '-' => Some(Minus),
            '*' => Some(Mul),
            '/' => Some(Div),
            ';' => Some(Semicolon),
            '=' => Some(Equal),
            '(' => Some(LeftParen),
            ')' => Some(RightParen),
            '[' => Some(LeftBracket),
            ']' => Some(RightBracket),
            '{' => Some(LeftBrace),
            '}' => Some(RightBrace),
            '<' => Some(LeftAngleBracket),
            '>' => Some(RightAngleBracket),
            ',' => Some(Colon),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct Symbol {
    pub name: &'static str,
    pub ty: TokenType,
}

impl Symbol {
    fn new(name: &'static str, ty: TokenType) -> Self {
        Symbol { name: name, ty: ty }
    }
}

lazy_static! {
    static ref SYMBOLS: [Symbol; 7] = [
        Symbol::new("else" , TokenType::Else),
        Symbol::new("for" , TokenType::For),
        Symbol::new("if" , TokenType::If),
        Symbol::new("int" , TokenType::Int),
        Symbol::new("return" , TokenType::Return),
        Symbol::new("&&", TokenType::Logand),
        Symbol::new("||", TokenType::Logor),
    ];
}

// Token type
#[derive(Debug, Clone)]
pub struct Token {
    pub ty: TokenType, // Token type
    pub input: String, // Token string (for error reporting)
}

pub fn tokenize(mut p: String) -> Vec<Token> {
    // Tokenized input is stored to this vec.
    let mut tokens: Vec<Token> = vec![];

    'outer: while let Some(c) = p.chars().nth(0) {
        // Skip whitespce
        if c.is_whitespace() {
            p = p.split_off(1); // p++
            continue;
        }

        // Single-letter token
        if let Some(ty) = TokenType::new_single_letter(c) {
            let token = Token {
                ty: ty,
                input: p.clone(),
            };
            p = p.split_off(1); // p++
            tokens.push(token);
            continue;
        }

        // Multi-letter symbol or keyword
        for symbol in SYMBOLS.iter() {
            let name = symbol.name;
            let len = name.len();
            if len > p.len() {
                continue;
            }

            let p_c = p.clone();
            let (first, _last) = p_c.split_at(len);
            if name != first {
                continue;
            }

            tokens.push(Token {
                ty: symbol.ty.clone(),
                input: p.clone(),
            });
            p = p.split_off(len); // p += len
            continue 'outer;
        }

        // Identifier
        if c.is_alphabetic() || c == '_' {
            let mut len = 1;
            while let Some(c2) = p.chars().nth(len) {
                if c2.is_alphabetic() || c2.is_ascii_digit() || c2 == '_' {
                    len += 1;
                    continue;
                }
                break;
            }

            let p_c = p.clone();
            let (name, _last) = p_c.split_at(len);
            p = p.split_off(len); // p += len
            let token = Token {
                ty: TokenType::Ident(name.to_string()),
                input: p.clone(),
            };
            tokens.push(token);
            continue;
        }

        // Number
        if c.is_ascii_digit() {
            let mut val: i32 = 0;
            for c in p.clone().chars() {
                if !c.is_ascii_digit() {
                    break;
                }
                val = val * 10 + c.to_digit(10).unwrap() as i32;
                p = p.split_off(1); // p++
            }

            let token = Token {
                ty: TokenType::Num(val as i32),
                input: p.clone(),
            };
            tokens.push(token);
            continue;
        }

        panic!("cannot tokenize: {}\n", p);
    }
    tokens
}
