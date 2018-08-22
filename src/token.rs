// Tokenizer
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Num, // Number literal
    Plus, // +
    Minus, // -
    Mul, // *
    Div, // /
    Return, // Return
    Semicolon, // ;
}

impl From<char> for TokenType {
    fn from(c: char) -> Self {
        match c {
            '+' => TokenType::Plus,
            '-' => TokenType::Minus,
            '*' => TokenType::Mul,
            '/' => TokenType::Div,
            ';' => TokenType::Semicolon,
            e => panic!("unknow Token type: {}", e),
        }
    }
}

impl From<String> for TokenType {
    fn from(s: String) -> Self {
        match &*s {
            "return" => TokenType::Return,
            name => panic!("unknown identifier: {}", name),
        }
    }
}

impl Default for TokenType {
    fn default() -> Self {
        TokenType::Num
    }
}

// Token type
#[derive(Default, Debug, Clone)]
pub struct Token {
    pub ty: TokenType, // Token type
    pub val: i32, // Number literal
    pub input: String, // Token string (for error reporting)
}

pub fn scan(mut p: String) -> Vec<Token> {
    // Tokenized input is stored to this vec.
    let mut tokens: Vec<Token> = vec![];

    let org = p.clone();
    while let Some(c) = p.chars().nth(0) {
        // Skip whitespce
        if c.is_whitespace() {
            p = p.split_off(1); // p++
            continue;
        }

        // Single-letter token
        match c {
            '+' | '-' | '*' | '/' | ';' => {
                let token = Token {
                    ty: TokenType::from(c),
                    input: org.clone(),
                    ..Default::default()
                };
                p = p.split_off(1); // p++
                tokens.push(token);
                continue;
            }
            _ => (),
        }

        // Keyword
        if c.is_alphabetic() || c == '_' {
            let mut name = String::new();
            while let Some(c2) = p.chars().nth(0) {
                p = p.split_off(1); // p++
                if c2.is_alphabetic() || c2.is_ascii_digit() || c2 == '_' {
                    name.push(c2);
                    continue;
                }
                break;
            }
            let token = Token {
                ty: TokenType::from(name),
                input: org.clone(),
                ..Default::default()
            };
            tokens.push(token);
            continue;
        }

        // Number
        if c.is_ascii_digit() {
            let n = strtol(&mut p);
            let token = Token {
                ty: TokenType::Num,
                input: org.clone(),
                val: n.unwrap() as i32,
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
