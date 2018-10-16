use preprocess;

use std::sync::{Mutex, Arc};
use std::collections::HashMap;
use std::fs::File;
use std::io;
use std::io::prelude::*;

lazy_static!  {
    static ref FILE_NAME: Mutex<String> = Mutex::new(String::new());
    static ref BUF: Mutex<Arc<Vec<char>>> = Mutex::new(Arc::new(vec![]));
}


// Tokenizer
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Num(i32), // Number literal
    Str(String, usize), // String literal. (str, len)
    CharLiteral(String), // Char literal.
    Ident(String), // Identifier
    Param(usize), // Function-like macro parameter
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
    HashMark, // #
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
    NewLine, // preprocessor-only token
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
            '#' => Some(HashMark),
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

    // For preprocessor
    pub stringize: bool,

    // For error reporting
    pub buf: Arc<Vec<char>>,
    pub filename: String,
    pub start: usize,
    pub end: usize,
}

impl Default for Token {
    fn default() -> Token {
        Token {
            ty: TokenType::Int,
            buf: Arc::new(vec![]),
            filename: "".to_string(),
            start: 0,
            end: 0,
            stringize: false,
        }
    }
}

impl Token {
    pub fn new(ty: TokenType, start: usize) -> Self {
        Token {
            ty,
            buf: BUF.lock().unwrap().clone(),
            filename: FILE_NAME.lock().unwrap().clone(),
            start,
            ..Default::default()
        }
    }

    pub fn tokstr(&self) -> String {
        self.buf[self.start..self.end].into_iter().collect()
    }

    pub fn line(&self) -> usize {
        self.buf[..self.end].iter().filter(|c| *c == &'\n').count()
    }
}

// Error reporting
lazy_static!{
    static ref INPUT_FILE: Mutex<Vec<char>> = Mutex::new(vec![]);
}

// Finds a line pointed by a given pointer from the input file
// to print it out.
fn print_line(buf: &Vec<char>, path: &String, pos: usize) {
    let mut p = 0;
    let mut start = 0;
    let mut line = 0;
    let mut col = 0;
    for c in buf.iter() {
        if c == &'\n' {
            start = pos + 1;
            line += 1;
            col = 0;
            p += 1;
            continue;
        }

        if p != pos {
            col += 1;
            p += 1;
            continue;
        }

        print!("error at {}:{}:{}\n\n", path, line + 1, col);
        break;
    }

    for p in buf[start..].iter() {
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
    print_line(&t.buf, &t.filename, t.start);
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

fn read_file(filename: String) -> String {
    let mut input = String::new();
    let mut fp = io::stdin();
    if filename != "-".to_string() {
        let mut fp = File::open(filename).expect("file not found");
        fp.read_to_string(&mut input).expect(
            "something went wrong reading the file",
        );
        return input;
    }
    fp.read_to_string(&mut input).expect(
        "something went wrong reading the file",
    );
    return input;
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

fn block_comment(p: &Vec<char>, pos: &mut usize) {
    *pos += 2;
    loop {
        let two_char = p.get(*pos..*pos + 2).expect("unclosed comment");
        *pos += 1;
        if two_char == &['*', '/'] {
            *pos += 1;
            return;
        }
    }
}

fn char_literal(p: &Vec<char>, pos: &mut usize, tokens: &mut Vec<Token>) -> char {
    *pos += 1;
    let result: char;
    let c = p.get(*pos).expect("premature end of input");
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
    let mut t = Token::new(TokenType::Num(result as u8 as i32), *pos - 1);
    t.end = *pos + 1;
    tokens.push(t);
    return result;
}

fn string_literal(p: &Vec<char>, pos: &mut usize, tokens: &mut Vec<Token>) {
    *pos += 1;
    let mut sb = String::new();
    let mut len = 0;
    loop {
        let mut c2 = p.get(*pos + len).expect("PREMATURE end of input");
        if c2 == &'"' {
            len += 1;
            *pos += len;
            let mut t = Token::new(TokenType::Str(sb, len), *pos - len - 1);
            t.end = *pos + 1;
            tokens.push(t);
            return;
        }

        if c2 != &'\\' {
            len += 1;
            sb.push(c2.clone());
            continue;
        }

        len += 1;
        c2 = p.get(*pos + len).unwrap();
        if let Some(esc) = escaped(c2) {
            sb.push(esc);
        } else {
            sb.push(c2.clone());
        }
        len += 1;
    }
}

fn ident(
    p: &Vec<char>,
    keywords: &HashMap<String, TokenType>,
    pos: &mut usize,
    tokens: &mut Vec<Token>,
) {
    let mut len = 1;
    while let Some(c2) = p.get(*pos + len) {
        if c2.is_alphabetic() || c2.is_ascii_digit() || c2 == &'_' {
            len += 1;
            continue;
        }
        break;
    }

    let name: String = p[*pos..*pos + len].into_iter().collect();
    *pos += len;
    let mut t;
    if let Some(keyword) = keywords.get(&name) {
        t = Token::new(keyword.clone(), *pos - len);
    } else {
        t = Token::new(TokenType::Ident(name.clone()), *pos - len);
    }
    t.end = *pos;
    tokens.push(t);
}

fn parse_number(p: &Vec<char>, pos: &mut usize, tokens: &mut Vec<Token>, base: u32) {
    let mut sum: i32 = 0;
    let mut len = 0;
    for c in p[*pos..].iter() {
        if let Some(val) = c.to_digit(base) {
            sum = sum * base as i32 + val as i32;
            *pos += 1;
            len += 1;
        } else {
            break;
        }
    }
    let mut t = Token::new(TokenType::Num(sum as i32), *pos - len);
    t.end = *pos;
    tokens.push(t);
}

fn number(p: &Vec<char>, pos: &mut usize, tokens: &mut Vec<Token>) {
    match p.get(*pos..*pos + 2) {
        Some(&['0', 'x']) |
        Some(&['0', 'X']) => {
            *pos += 2;
            parse_number(p, pos, tokens, 16);
        }
        Some(&['0', _]) => {
            parse_number(p, pos, tokens, 8);
        }
        _ => parse_number(p, pos, tokens, 10),
    }
}

// Tokenized input is stored to this vec.
fn scan(p: &Vec<char>, keywords: &HashMap<String, TokenType>) -> Vec<Token> {
    *BUF.lock().unwrap() = Arc::new(p.clone());

    let mut tokens: Vec<Token> = vec![];

    let mut pos = 0;

    'outer: while let Some(c) = p.get(pos) {
        // New line (preprocessor-only token)
        if c == &'\n' {
            let mut t = Token::new(TokenType::NewLine, pos);
            pos += 1;
            t.end = pos;
            tokens.push(t);
            continue;
        }

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
            block_comment(&p, &mut pos);
            continue;
        }

        // Character literal
        if c == &'\'' {
            char_literal(&p, &mut pos, &mut tokens);
            continue;
        }


        // String literal
        if c == &'"' {
            string_literal(&p, &mut pos, &mut tokens);
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

            let mut t = Token::new(symbol.ty.clone(), pos);
            pos += len;
            t.end = pos;
            tokens.push(t);
            continue 'outer;
        }

        // Single-letter symbol
        if let Some(ty) = TokenType::new_single_letter(c) {
            let mut t = Token::new(ty, pos);
            pos += 1;
            t.end = pos;
            tokens.push(t);
            continue;
        };

        // Keyword or identifier
        if c.is_alphabetic() || c == &'_' {
            ident(&p, &keywords, &mut pos, &mut tokens);
            continue;
        }

        // Number
        if c.is_ascii_digit() {
            number(&p, &mut pos, &mut tokens);
            continue;
        }

        panic!(
            "cannot tokenize: {:?}\n",
            p[pos..].into_iter().collect::<String>()
        );
    }
    tokens
}

fn canonicalize_newline(p: &mut Vec<char>) {
    let mut pos = 0;
    while pos < p.len() {
        if p[pos] == '\r' && p[pos + 1] == '\n' {
            p.remove(pos);
            p.remove(pos);
        }
        pos += 1;
    }
}

fn remove_backslash_newline(p: &mut Vec<char>) {
    let mut pos = 0;
    while pos < p.len() {
        if p[pos] == '\\' && p[pos + 1] == '\n' {
            p.remove(pos);
            p.remove(pos);
        }
        pos += 1;
    }
}

fn append(x_str: &String, y_str: &String, start: usize) -> Token {
    let concated = format!("{}{}", x_str, y_str);
    let l = concated.len() + 1; // Because `+1` has `\0`.
    return Token::new(TokenType::Str(concated, l), start);
}

fn strip_newlines(tokens: Vec<Token>) -> Vec<Token> {
    let mut v: Vec<Token> = vec![];

    for t in tokens.into_iter() {
        if t.ty != TokenType::NewLine {
            v.push(t);
        }
    }
    v
}

fn join_string_literals(tokens: Vec<Token>) -> Vec<Token> {
    let mut v = vec![];
    let mut last_may: Option<Token> = None;

    for mut t in tokens.into_iter() {
        if let Some(ref last) = last_may {
            match (&last.ty, &t.ty) {
                (TokenType::Str(ref last_str, _), TokenType::Str(ref t_str, _)) => {
                    let new = append(last_str, t_str, last.start);
                    v.pop();
                    v.push(new);
                    continue;
                }
                _ => (),
            }
        }

        last_may = Some(t.clone());
        v.push(t);
    }
    v
}

pub fn tokenize(path: String, ctx: &mut preprocess::Context) -> Vec<Token> {
    *FILE_NAME.lock().unwrap() = path.clone();
    let mut buf = read_file(path).chars().collect();

    canonicalize_newline(&mut buf);
    remove_backslash_newline(&mut buf);

    let mut tokens = scan(&buf, &keyword_map());

    tokens = preprocess::preprocess(tokens, ctx);
    tokens = strip_newlines(tokens);
    join_string_literals(tokens)
}
