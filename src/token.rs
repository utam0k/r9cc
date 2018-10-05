use std::sync::Mutex;
use std::collections::HashMap;

use FILE_NAME;

// Tokenizer
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Num(i32), // Number literal
    Str(String, usize), // String literal. (str, len)
    CharLiteral(String), // Char literal.
    Ident(String), // Identifier
    Arrow, // ->
    Extern, // "extern"
    Typedef, // "typedef"
    Int, // "int"
    Char, // "char"
    Void, // "void"
    Struct, // "struct"
    Plus, // +
    Minus, // -
    Mul, // *
    Div, // /
    And, // &
    Dot, // .
    Comma, // ,
    Exclamation, // !
    Question, // ?
    VerticalBar, // |
    Hat, // ^
    Colon, // :
    If, // "if"
    Else, // "else"
    For, // "for"
    Do, // "do"
    While, // "while"
    Break, // "break"
    EQ, // ==
    NE, // !=
    LE, // <=
    GE, // >=
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
    SHL, // <<
    Inc, // ++
    Dec, // --
    MulEQ, // *=
    DivEQ, // /=
    ModEQ, // %=
    AddEQ, // +=
    SubEQ, // -=
    ShlEQ, // <<=
    ShrEQ, // >>=
    BitandEQ, // &=
    XorEQ, // ^=
    BitorEQ, // |=
    SHR, // >>
    Mod, // %
    Return, // "return"
    Sizeof, // "sizeof"
    Alignof, // "_Alignof"
}

impl TokenType {
    fn new_single_letter(c: &char) -> Option<Self> {
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
            ',' => Some(Comma),
            '.' => Some(Dot),
            '!' => Some(Exclamation),
            '?' => Some(Question),
            '|' => Some(VerticalBar),
            '^' => Some(Hat),
            '%' => Some(Mod),
            ':' => Some(Colon),
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
        Symbol { name, ty }
    }
}

lazy_static! {
    static ref SYMBOLS: Vec<Symbol> = [
        Symbol::new("<<=", TokenType::ShlEQ),
        Symbol::new(">>=", TokenType::ShrEQ),
        Symbol::new("!=", TokenType::NE),
        Symbol::new("&&", TokenType::Logand),
        Symbol::new("++", TokenType::Inc),
        Symbol::new("--", TokenType::Dec),
        Symbol::new("->", TokenType::Arrow),
        Symbol::new("<<", TokenType::SHL),
        Symbol::new("<=", TokenType::LE),
        Symbol::new("==", TokenType::EQ),
        Symbol::new(">=", TokenType::GE),
        Symbol::new(">>", TokenType::SHR),
        Symbol::new("||", TokenType::Logor),
        Symbol::new("*=", TokenType::MulEQ),
        Symbol::new("/=", TokenType::DivEQ),
        Symbol::new("%=", TokenType::ModEQ),
        Symbol::new("+=", TokenType::AddEQ),
        Symbol::new("-=", TokenType::SubEQ),
        Symbol::new("&=", TokenType::BitandEQ),
        Symbol::new("^=", TokenType::XorEQ),
        Symbol::new("|=", TokenType::BitorEQ),
    ].to_vec();
}

// Token type
#[derive(Debug, Clone)]
pub struct Token {
    pub ty: TokenType, // Token type
    pub start: usize, // For error reporting
}

impl Token {
    pub fn new(ty: TokenType, start: usize) -> Self {
        Token { ty, start }
    }
}

// Error reporting
lazy_static!{
    static ref INPUT_FILE: Mutex<Vec<char>> = Mutex::new(vec![]);
}

fn print_line(t: &Token) {
    let mut pos = 0;
    let mut start = 0;
    let mut line = 0;
    let mut col = 0;
    let input_file = INPUT_FILE.lock().unwrap();
    for p in input_file.iter() {
        if p == &'\n' {
            start = pos + 1;
            line += 1;
            col = 0;
            pos += 1;
            continue;
        }

        if pos != t.start {
            col += 1;
            pos += 1;
            continue;
        }

        print!(
            "error at {}:{}:{}\n\n",
            FILE_NAME.lock().unwrap(),
            line + 1,
            col
        );
        break;
    }

    for p in input_file[start..].iter() {
        if p == &'\n' {
            break;
        }
        print!("{}", p);
    }
    print!("\n");
    for _ in 0..col - 1 {
        print!(" ");
    }
    print!("^\n\n");
}

pub fn bad_token(t: &Token, msg: &str) -> ! {
    print_line(t);
    panic!("{}", msg);
}

fn escaped(c: &char) -> Option<char> {
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

fn read_char(p: &Vec<char>, pos: &mut usize) -> char {
    let result: char;
    if let Some(c) = p.get(*pos) {
        if c != &'\\' {
            result = c.clone();
            *pos += 1;
        } else {
            *pos += 1;
            let c2 = p.get(*pos).unwrap();
            result = if let Some(esc) = escaped(c2) {
                esc
            } else {
                c2.clone()
            };
            *pos += 1;
        }

        if p.get(*pos) != Some(&'\'') {
            panic!("unclosed character literal");
        }

        *pos += 1;
        return result;
    } else {
        panic!("premature end of input");
    }
}

fn read_string(p: &Vec<char>, pos: usize) -> (String, usize) {
    let mut sb = String::new();
    let mut len = 0;
    loop {
        if let Some(mut c2) = p.get(pos + len) {
            if c2 == &'"' {
                return (sb, len + 1);
            }

            if c2 != &'\\' {
                len += 1;
                sb.push(c2.clone());
                continue;
            }

            len += 1;
            c2 = p.get(pos + len).unwrap();
            if let Some(esc) = escaped(c2) {
                sb.push(esc);
            } else {
                sb.push(c2.clone());
            }
            len += 1;
        } else {
            panic!("PREMATURE end of input");
        }
    }
}

fn keyword_map() -> HashMap<String, TokenType> {
    let mut map = HashMap::new();
    map.insert("_Alignof".into(), TokenType::Alignof);
    map.insert("break".into(), TokenType::Break);
    map.insert("char".into(), TokenType::Char);
    map.insert("void".into(), TokenType::Void);
    map.insert("do".into(), TokenType::Do);
    map.insert("else".into(), TokenType::Else);
    map.insert("extern".into(), TokenType::Extern);
    map.insert("for".into(), TokenType::For);
    map.insert("if".into(), TokenType::If);
    map.insert("int".into(), TokenType::Int);
    map.insert("return".into(), TokenType::Return);
    map.insert("sizeof".into(), TokenType::Sizeof);
    map.insert("struct".into(), TokenType::Struct);
    map.insert("typedef".into(), TokenType::Typedef);
    map.insert("while".into(), TokenType::While);
    map
}

// Tokenized input is stored to this vec.
pub fn tokenize(p: Vec<char>) -> Vec<Token> {
    *INPUT_FILE.lock().unwrap() = p.clone();
    let mut tokens: Vec<Token> = vec![];
    let keywords = keyword_map();

    let mut pos = 0;

    'outer: while let Some(c) = p.get(pos) {
        // Skip whitespce
        if c.is_whitespace() {
            pos += 1;
            continue;
        }

        // Line comment
        if p.get(pos..pos + 2) == Some(&['/', '/']) {
            while p.get(pos) != Some(&'\n') {
                pos += 1;
            }
            continue;
        }

        // Block comment
        if p.get(pos..pos + 2) == Some(&['/', '*']) {
            pos += 2;
            loop {
                if let Some(two_char) = p.get(pos..pos + 2) {
                    pos += 1;
                    if two_char == &['*', '/'] {
                        pos += 1;
                        continue 'outer;
                    }
                } else {
                    panic!("unclosed comment");
                }
            }
        }

        // Character literal
        if c == &'\'' {
            pos += 1;
            let val = read_char(&p, &mut pos);
            tokens.push(Token::new(
                TokenType::Num(val.clone() as u8 as i32),
                pos - 1,
            ));
            continue;
        }


        // String literal
        if c == &'"' {
            pos += 1;

            let (sb, len) = read_string(&p, pos);
            pos += len;
            let token = Token::new(TokenType::Str(sb, len), pos - len - 1);
            tokens.push(token);
            continue;
        }

        // Multi-letter symbol
        for symbol in SYMBOLS.iter() {
            let name = symbol.name;
            let len = name.len();
            if pos + len > p.len() {
                continue;
            }

            let first = &p[pos..pos + len];
            if name.to_string() != first.into_iter().collect::<String>() {
                continue;
            }

            tokens.push(Token::new(symbol.ty.clone(), pos));
            pos += len;
            continue 'outer;
        }

        // Single-letter symbol
        if let Some(ty) = TokenType::new_single_letter(c) {
            let token = Token::new(ty, pos);
            pos += 1;
            tokens.push(token);
            continue;
        };

        // Keyword or identifier
        if c.is_alphabetic() || c == &'_' {
            let mut len = 1;
            while let Some(c2) = p.get(pos + len) {
                if c2.is_alphabetic() || c2.is_ascii_digit() || c2 == &'_' {
                    len += 1;
                    continue;
                }
                break;
            }

            let name: String = p[pos..pos + len].into_iter().collect();
            pos += len;
            let token;
            if let Some(keyword) = keywords.get(&name) {
                token = Token::new(keyword.clone(), pos - len);
            } else {
                token = Token::new(TokenType::Ident(name.clone()), pos - len);
            }
            tokens.push(token);
            continue;
        }

        // Number
        if c.is_ascii_digit() {
            let mut val: i32 = 0;
            let mut len = 0;
            for c in p[pos..].iter() {
                if !c.is_ascii_digit() {
                    break;
                }
                val = val * 10 + c.to_digit(10).unwrap() as i32;
                len += 1;
            }
            pos += len;
            let token = Token::new(TokenType::Num(val as i32), pos - len);
            tokens.push(token);
            continue;
        }

        panic!(
            "cannot tokenize: {:?}\n",
            p[pos..].into_iter().collect::<String>()
        );
    }
    tokens
}
