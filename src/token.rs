// Tokenizer
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Num(i32), // Number literal
    Str(String, usize), // String literal. (str, len)
    CharLiteral(String), // Char literal.
    Ident(String), // Identifier
    Extern, // "int"
    Int, // "int"
    Char, // "char"
    Plus, // +
    Minus, // -
    Mul, // *
    And, // &
    Div, // /
    If, // "if"
    Else, // "else"
    For, // "for"
    Do, // "do"
    While, // "while"
    EQ, // ==
    NE, // !=
    Semicolon, // ;
    LeftParen, // (
    RightParen, // )
    LeftBracket, // [
    RightBracket, // ]
    LeftBrace, // {
    RightBrace, // }
    LeftAngleBracket, // <
    RightAngleBracket, // >
    Equal, // =
    Logor, // ||
    Logand, // &&
    Return, // "return"
    Sizeof, // "sizeof"
    Colon, // ,
}

impl TokenType {
    fn new_single_letter(c: char) -> Option<Self> {
        use self::TokenType::*;
        match c {
            '+' => Some(Plus),
            '-' => Some(Minus),
            '*' => Some(Mul),
            '/' => Some(Div),
            '&' => Some(And),
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

#[derive(Debug, Clone)]
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
    static ref SYMBOLS: Vec<Symbol> = [
        Symbol::new("char" , TokenType::Char),
        Symbol::new("do" , TokenType::Do),
        Symbol::new("else" , TokenType::Else),
        Symbol::new("extern" , TokenType::Extern),
        Symbol::new("for" , TokenType::For),
        Symbol::new("if" , TokenType::If),
        Symbol::new("int" , TokenType::Int),
        Symbol::new("return" , TokenType::Return),
        Symbol::new("while" , TokenType::While),
        Symbol::new("sizeof" , TokenType::Sizeof),
        Symbol::new("&&", TokenType::Logand),
        Symbol::new("||", TokenType::Logor),
        Symbol::new("==", TokenType::EQ),
        Symbol::new("!=", TokenType::NE),
    ].to_vec();
}

// Token type
#[derive(Debug, Clone)]
pub struct Token {
    pub ty: TokenType, // Token type
    pub input: String, // Token string (for error reporting)
}

fn escaped(c: char) -> Option<char> {
    // Issue: https://github.com/rust-lang/rfcs/issues/751
    match c {
        // 'a' => Some("\a"),
        // 'b' => Some("\b"),
        // 'f' => Some("\f"),
        'n' => Some('\n'),
        'r' => Some('\r'),
        't' => Some('\t'),
        // 'v' => Some("\v"),
        _ => None,
    }
}

fn read_char(p: &mut String) -> char {
    let result: char;
    if let Some(c) = p.chars().nth(0) {
        if c != '\\' {
            result = c;
            p.drain(..1); // p ++
        } else {
            p.drain(..1); // p ++
            let c2 = p.chars().nth(0).unwrap();
            result = if let Some(esc) = escaped(c2) { esc } else { c2 };
            p.drain(..1); // p ++
        }

        if p.chars().nth(0) != Some('\'') {
            panic!("unclosed character literal");
        }

        p.drain(..1); // p ++
        return result;
    } else {
        panic!("PREMATURE end of input");
    }
}

fn read_string(mut p: String) -> (String, usize) {
    let mut sb = String::new();
    let mut len = 0;
    loop {
        if let Some(mut c2) = p.chars().nth(0) {
            if c2 == '"' {
                return (sb, len + 1);
            }

            if c2 != '\\' {
                p = p.split_off(1); // p ++
                len += 1;
                sb.push(c2);
                continue;
            }

            p = p.split_off(1); // p ++
            len += 1;
            c2 = p.chars().nth(0).unwrap();
            if let Some(esc) = escaped(c2) {
                sb.push(esc);
            } else {
                sb.push(c2);
            }
            p = p.split_off(1); // p ++
            len += 1;
        } else {
            panic!("PREMATURE end of input");
        }
    }
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

        // Character literal
        if c == '\'' {
            p = p.split_off(1); // p++
            let val = read_char(&mut p);
            tokens.push(Token {
                ty: TokenType::Num(val as u8 as i32),
                input: p.clone(),
            });
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

        // String literal
        if c == '"' {
            p = p.split_off(1); // p ++

            let (sb, len) = read_string(p.clone());
            p = p.split_off(len); // p += len - 1
            let token = Token {
                ty: TokenType::Str(sb, len),
                input: p.clone(),
            };
            tokens.push(token);
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
