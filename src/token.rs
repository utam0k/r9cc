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
    For, // "for"
    Semicolon, // ;
    LeftParen, // (
    RightParen, // )
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
            '<' => LeftAngleBracket,
            '>' => RightAngleBracket,
            ',' => Colon,
            e => panic!("unknow Token type: {}", e),
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
    static ref SYMBOLS: [Symbol; 6] = [
        Symbol::new("else" , TokenType::Else),
        Symbol::new("for" , TokenType::For),
        Symbol::new("if" , TokenType::If),
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

pub fn scan(mut p: String) -> Vec<Token> {
    // Tokenized input is stored to this vec.
    let mut tokens: Vec<Token> = vec![];

    'outer: while let Some(c) = p.chars().nth(0) {
        // Skip whitespce
        if c.is_whitespace() {
            p = p.split_off(1); // p++
            continue;
        }

        // Single-letter token
        match c {
            '+' | '-' | '*' | '/' | ';' | '=' | '(' | ')' | ',' | '{' | '}' | '<' | '>' => {
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

        // Multi-letter token
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
