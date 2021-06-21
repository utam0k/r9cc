use crate::preprocess;
use crate::CharacterType;
use crate::TokenType;

use std::collections::HashMap;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::rc::Rc;

pub fn tokenize(path: String, ctx: &mut preprocess::Preprocessor) -> Vec<Token> {
    let mut tokenizer = Tokenizer::new(Rc::new(path));
    tokenizer.canonicalize_newline();
    tokenizer.remove_backslash_newline();
    tokenizer.scan(&keyword_map());

    tokenizer.tokens = preprocess::preprocess(tokenizer.tokens, ctx);
    tokenizer.strip_newlines_tokens();
    tokenizer.join_string_literals();
    tokenizer.tokens
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

#[derive(Debug, Clone)]
pub struct Token {
    pub ty: TokenType, // Token type

    // For preprocessor
    pub stringize: bool,

    // For error reporting
    pub buf: Rc<Vec<char>>,
    pub filename: Rc<String>,
    pub start: usize,
    pub end: usize,
}

impl Default for Token {
    fn default() -> Token {
        Token {
            ty: TokenType::Int,
            buf: Rc::new(vec![]),
            filename: Rc::new("".to_string()),
            start: 0,
            end: 0,
            stringize: false,
        }
    }
}

impl Token {
    pub fn new(ty: TokenType, start: usize, filename: Rc<String>, buf: Rc<Vec<char>>) -> Self {
        Token {
            ty,
            buf,
            filename,
            start,
            ..Default::default()
        }
    }

    pub fn bad_token(&self, msg: &str) -> ! {
        print_line(&*self.buf, &*self.filename, self.start);
        panic!("{}", msg);
    }

    pub fn tokstr(&self) -> String {
        self.buf[self.start..self.end].iter().collect()
    }

    pub fn get_line_number(&self) -> usize {
        self.buf[..self.end].iter().filter(|c| *c == &'\n').count()
    }

    pub fn is_ident(&self, s: &str) -> bool {
        match self.ty {
            TokenType::Ident(ref name) => name == s,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
struct Symbol {
    name: &'static str,
    ty: TokenType,
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
    ]
    .to_vec();
}

// Tokenizer
struct Tokenizer {
    p: Rc<Vec<char>>,
    pos: usize,
    tokens: Vec<Token>,

    // Error reporting
    filename: Rc<String>,
}

impl Tokenizer {
    fn new(filename: Rc<String>) -> Self {
        Tokenizer {
            p: Rc::new(Self::read_file(&filename).chars().collect()),
            filename,
            pos: 0,
            tokens: vec![],
        }
    }

    fn read_file(filename: &str) -> String {
        let mut input = String::new();
        let mut fp = io::stdin();
        if filename != &"-".to_string() {
            let mut fp = File::open(filename).expect("file not found");
            fp.read_to_string(&mut input)
                .expect("something went wrong reading the file");
            return input;
        }
        fp.read_to_string(&mut input)
            .expect("something went wrong reading the file");
        input
    }

    fn new_token(&self, ty: TokenType) -> Token {
        Token::new(ty, self.pos, self.filename.clone(), self.p.clone())
    }

    // This does not support non-ASCII characters.
    fn get_character(&self, advance_from_pos: usize) -> Option<CharacterType> {
        self.p.get(self.pos + advance_from_pos).map(|ch| {
            if ch == &'\n' {
                CharacterType::NewLine
            } else if ch == &' ' || ch == &'\t' {
                CharacterType::Whitespace
            } else if ch.is_alphabetic() || ch == &'_' {
                CharacterType::Alphabetic
            } else if ch.is_ascii_digit() {
                CharacterType::Digit
            } else {
                CharacterType::NonAlphabetic(*ch)
            }
        })
    }

    fn scan(&mut self, keywords: &HashMap<String, TokenType>) -> Vec<Token> {
        'outer: while let Some(head_char) = self.get_character(0) {
            match head_char {
                CharacterType::NewLine => {
                    let mut t = self.new_token(TokenType::NewLine);
                    self.pos += 1;
                    t.end = self.pos;
                    self.tokens.push(t);
                }
                CharacterType::Whitespace => self.pos += 1,
                CharacterType::Alphabetic => self.ident(&keywords),
                CharacterType::Digit => self.number(),

                CharacterType::NonAlphabetic('\'') => self.char_literal(),
                CharacterType::NonAlphabetic('\"') => self.string_literal(),
                CharacterType::NonAlphabetic('/') => match self.p.get(self.pos + 1) {
                    Some('/') => self.line_comment(),
                    Some('*') => self.block_comment(),
                    Some('=') => {
                        let mut t = self.new_token(TokenType::DivEQ);
                        self.pos += 2;
                        t.end = self.pos;
                        self.tokens.push(t);
                    }
                    // This is Dividing operator
                    _ => {
                        let mut t = self.new_token(TokenType::Div);
                        self.pos += 1;
                        t.end = self.pos;
                        self.tokens.push(t);
                    }
                },
                CharacterType::NonAlphabetic(c) => {
                    // Multi-letter symbol
                    for symbol in SYMBOLS.iter() {
                        let name = symbol.name;
                        let len = name.len();
                        if self.pos + len > self.p.len() {
                            continue;
                        }

                        let first = &self.p[self.pos..self.pos + len];
                        if name != first.iter().collect::<String>() {
                            continue;
                        }

                        let mut t = self.new_token(symbol.ty.clone());
                        self.pos += len;
                        t.end = self.pos;
                        self.tokens.push(t);
                        continue 'outer;
                    }

                    // Single-letter symbol
                    if let Some(ty) = TokenType::new_single_letter(c) {
                        let mut t = self.new_token(ty);
                        self.pos += 1;
                        t.end = self.pos;
                        self.tokens.push(t);
                        continue 'outer;
                    }
                    self.bad_position("Unknown symbol.");
                }
                CharacterType::Unknown(_) => self.bad_position("Unknwon character type."),
            }
        }

        self.tokens.clone()
    }

    fn line_comment(&mut self) {
        while self.p.get(self.pos) != Some(&'\n') {
            self.pos += 1;
        }
    }

    fn block_comment(&mut self) {
        self.pos += 2;
        loop {
            if let Some(two_char) = self.p.get(self.pos..self.pos + 2) {
                self.pos += 1;
                if two_char == ['*', '/'] {
                    self.pos += 1;
                    return;
                }
            } else {
                self.bad_position("unclosed comment");
            }
        }
    }

    fn escaped(c: char) -> Option<char> {
        // Issue: https://github.com/rust-lang/rfcs/issues/751
        match c {
            // 'a' => Some('\a'),
            // 'b' => Some('\b'),
            // 'f' => Some('\f'),
            'n' => Some('\n'),
            'r' => Some('\r'),
            't' => Some('\t'),
            // 'v' => Some('\v'),
            _ => None,
        }
    }

    fn char_literal(&mut self) {
        self.pos += 1;
        let result: char;
        let c = self.p.get(self.pos).expect("premature end of input");
        if c != &'\\' {
            result = *c;
            self.pos += 1;
        } else {
            self.pos += 1;
            let c2 = self.p.get(self.pos).unwrap();
            result = if let Some(esc) = Self::escaped(*c2) {
                esc
            } else {
                *c2
            };
            self.pos += 1;
        }

        if self.p.get(self.pos) != Some(&'\'') {
            panic!("unclosed character literal");
        }

        let mut t = self.new_token(TokenType::Num(result as u8 as i32));
        self.pos += 1;
        t.end = self.pos + 1;
        self.tokens.push(t);
    }

    fn string_literal(&mut self) {
        self.pos += 1;
        let mut sb = String::new();
        let mut len = 0;
        loop {
            let mut c2 = self.p.get(self.pos + len).expect("PREMATURE end of input");
            if c2 == &'"' {
                len += 1;
                self.pos += len;
                let mut t = self.new_token(TokenType::Str(sb, len));
                t.start = self.pos - len - 1;
                t.end = self.pos + 1;
                self.tokens.push(t);
                return;
            }

            if c2 != &'\\' {
                len += 1;
                sb.push(c2.clone());
                continue;
            }

            len += 1;
            c2 = self.p.get(self.pos + len).unwrap();
            if let Some(esc) = Self::escaped(*c2) {
                sb.push(esc);
            } else {
                sb.push(c2.clone());
            }
            len += 1;
        }
    }

    fn ident(&mut self, keywords: &HashMap<String, TokenType>) {
        let mut len = 1;
        while let Some(c2) = self.p.get(self.pos + len) {
            if c2.is_alphabetic() || c2.is_ascii_digit() || c2 == &'_' {
                len += 1;
                continue;
            }
            break;
        }

        let name: String = self.p[self.pos..self.pos + len].iter().collect();
        let mut t;
        if let Some(keyword) = keywords.get(&name) {
            t = self.new_token(keyword.clone());
        } else {
            t = self.new_token(TokenType::Ident(name.clone()));
        }
        self.pos += len;
        t.end = self.pos;
        self.tokens.push(t);
    }

    fn number(&mut self) {
        match self.p.get(self.pos..self.pos + 2) {
            Some(&['0', 'x']) | Some(&['0', 'X']) => {
                self.pos += 2;
                self.parse_number(16);
            }
            Some(&['0', _]) => {
                self.parse_number(8);
            }
            _ => self.parse_number(10),
        }
    }

    fn parse_number(&mut self, base: u32) {
        let mut sum: i32 = 0;
        let mut len = 0;
        for c in self.p[self.pos..].iter() {
            if let Some(val) = c.to_digit(base) {
                sum = sum * base as i32 + val as i32;
                len += 1;
            } else {
                break;
            }
        }
        let mut t = self.new_token(TokenType::Num(sum as i32));
        self.pos += len;
        t.end = self.pos;
        self.tokens.push(t);
    }

    fn canonicalize_newline(&mut self) {
        let mut pos = 0;
        while pos < self.p.len() {
            if self.p[pos] == '\r' && self.p[pos + 1] == '\n' {
                Rc::get_mut(&mut self.p).unwrap().remove(pos);
                Rc::get_mut(&mut self.p).unwrap().remove(pos);
            }
            pos += 1;
        }
    }

    // Quoted from 9cc
    // > Concatenates continuation lines. We keep the total number of
    // > newline characters the same to keep the line counter sane.
    fn remove_backslash_newline(&mut self) {
        let mut pos = 0;
        let mut cnt = 0;
        while pos < self.p.len() {
            if self.p[pos] == '\\' && self.p[pos + 1] == '\n' {
                cnt += 1;
                Rc::get_mut(&mut self.p).unwrap().remove(pos);
                Rc::get_mut(&mut self.p).unwrap().remove(pos);
                pos += 1;
            } else if self.p[pos] == '\n' {
                for _ in 0..cnt {
                    Rc::get_mut(&mut self.p).unwrap().insert(pos, '\n');
                    pos += 1;
                }
                pos += 1;
                cnt = 0;
            } else {
                pos += 1;
            }
        }
    }

    fn append(&mut self, x_str: &str, y_str: &str, start: usize) -> Token {
        let concated = format!("{}{}", x_str, y_str);
        let l = concated.len() + 1; // Because `+1` has `\0`.
        Token::new(
            TokenType::Str(concated, l),
            start,
            self.filename.clone(),
            self.p.clone(),
        )
    }

    fn join_string_literals(&mut self) {
        let mut v = vec![];
        let mut last_may: Option<Token> = None;

        for t in self.tokens.clone().into_iter() {
            if let Some(ref last) = last_may {
                if let (TokenType::Str(ref last_str, _), TokenType::Str(ref t_str, _)) =
                    (&last.ty, &t.ty)
                {
                    let new = self.append(last_str, t_str, last.start);
                    v.pop();
                    v.push(new);
                    continue;
                }
            }

            last_may = Some(t.clone());
            v.push(t);
        }
        self.tokens = v;
    }

    fn strip_newlines_tokens(&mut self) {
        self.tokens = self
            .tokens
            .clone()
            .into_iter()
            .filter(|t| t.ty != TokenType::NewLine)
            .collect()
    }

    fn bad_position(&self, msg: &'static str) {
        print_line(&self.p, &self.filename, self.pos);
        panic!("{}", msg);
    }
}

// Finds a line pointed by a given pointer from the input file
// to print it out.
fn print_line(buf: &[char], path: &str, pos: usize) {
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
    println!();
    for _ in 0..col - 1 {
        print!(" ");
    }
    print!("^\n\n");
}
