// Tokenizer
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Num(i32), // Number literal
    Ident(String), // Identifier
    Plus, // +
    Minus, // -
    Mul, // *
    Div, // /
    If, // "if"
    Else, // "else"
    Return, // "return"
    Semicolon, // ;
    LeftParen, // (
    RightParen, // )
    LeftBrace, // {
    RightBrace, // {
    Equal, // =
    Colon, // ,
}

impl From<char> for TokenType {
    fn from(c: char) -> Self {
        use self::TokenType::*;
        match c {
            '+' => Plus,
            '-' => Minus,
            '*' => Mul,
            '/' => Div,
            ';' => Semicolon,
            '=' => Equal,
            '(' => LeftParen,
            ')' => RightParen,
            '{' => LeftBrace,
            '}' => RightBrace,
            ',' => Colon,
            e => panic!("unknow Token type: {}", e),
        }
    }
}

impl From<String> for TokenType {
    fn from(s: String) -> Self {
        match &*s {
            "return" => TokenType::Return,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            name => TokenType::Ident(name.to_string()),
        }
    }
}

// Token type
#[derive(Debug, Clone)]
pub struct Token {
    pub ty: TokenType, // Token type
    pub input: String, // Token string (for error reporting)
}

pub fn scan(mut p: String) -> Vec<Token> {
    // Tokenized input is stored to this vec.
    let mut tokens: Vec<Token> = vec![];

    while let Some(c) = p.chars().nth(0) {
        // Skip whitespce
        if c.is_whitespace() {
            p = p.split_off(1); // p++
            continue;
        }

        // Single-letter token
        match c {
            '+' | '-' | '*' | '/' | ';' | '=' | '(' | ')' | ',' | '{' | '}' => {
                let token = Token {
                    ty: TokenType::from(c),
                    input: p.clone(),
                };
                p = p.split_off(1); // p++
                tokens.push(token);
                continue;
            }
            _ => (),
        }

        // Identifier
        if c.is_alphabetic() || c == '_' {
            let mut name = String::new();
            while let Some(c2) = p.chars().nth(0) {
                if c2.is_alphabetic() || c2.is_ascii_digit() || c2 == '_' {
                    p = p.split_off(1); // p++
                    name.push(c2);
                    continue;
                }
                break;
            }
            let token = Token {
                ty: TokenType::from(name),
                input: p.clone(),
            };
            tokens.push(token);
            continue;
        }

        // Number
        if c.is_ascii_digit() {
            let n = strtol(&mut p);
            let token = Token {
                ty: TokenType::Num(n.unwrap() as i32),
                input: p.clone(),
            };
            tokens.push(token);
            continue;
        }

        panic!("cannot tokenize: {}\n", p);
    }
    tokens
}

pub fn tokenize(p: String) -> Vec<Token> {
    scan(p)
}

fn strtol(s: &mut String) -> Option<i64> {
    if s.is_empty() {
        return None;
    }

    let mut pos = 0;
    for c in s.chars() {
        if !c.is_ascii_digit() {
            break;
        }
        pos += 1;
    }

    let t: String = s.drain(..pos).collect();
    Some(t.parse::<i64>().unwrap())
}
