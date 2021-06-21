// C preprocessor

use crate::token::{tokenize, Token};
use crate::TokenType;

use std::collections::HashMap;
use std::mem;
use std::rc::Rc;

pub fn preprocess(tokens: Vec<Token>, ctx: &mut Preprocessor) -> Vec<Token> {
    ctx.preprocess_impl(tokens)
}

#[derive(Clone)]
pub struct Env {
    input: Vec<Token>,
    output: Vec<Token>,
    pos: usize,
    next: Option<Box<Env>>,
}

impl Default for Env {
    fn default() -> Env {
        Env {
            input: vec![],
            output: vec![],
            pos: 0,
            next: None,
        }
    }
}

impl Env {
    pub fn new(input: Vec<Token>, next: Option<Box<Env>>) -> Self {
        Env {
            input,
            next,
            ..Default::default()
        }
    }
}

#[derive(Debug, Clone)]
enum MacroType {
    Objlike,
    Funclike(Vec<String>),
}

#[derive(Debug, Clone)]
struct Macro {
    ty: MacroType,
    pub tokens: Vec<Token>,
}

impl Macro {
    fn new(ty: MacroType) -> Self {
        Macro { ty, tokens: vec![] }
    }

    fn replace_params(mut self) -> Self {
        match self.ty {
            MacroType::Funclike(ref params) => {
                let mut map = HashMap::new();
                for (i, item) in params.iter().enumerate() {
                    let name = item.clone();
                    map.insert(name, i);
                }

                for i in 0..self.tokens.len() {
                    let t = &self.tokens[i].clone();
                    match t.ty {
                        TokenType::Ident(ref name) => {
                            if let Some(n) = map.get(name) {
                                if let Some(elem) = self.tokens.get_mut(i) {
                                    *elem = Token::new(
                                        TokenType::Param(*n),
                                        0,
                                        t.filename.clone(),
                                        t.buf.clone(),
                                    );
                                }
                            } else {
                                continue;
                            }
                        }
                        _ => continue,
                    }
                }

                // Process '#' followed by a macro parameter.
                self.tokens = self
                    .tokens
                    .into_iter()
                    .scan(false, |is_prev_hashmark, mut t| {
                        if *is_prev_hashmark {
                            match t.ty {
                                TokenType::Param(_) => t.stringize = true,
                                _ => *is_prev_hashmark = false,
                            }
                        } else {
                            *is_prev_hashmark = t.ty == TokenType::HashMark;
                        }
                        Some(t)
                    })
                    .collect::<Vec<_>>();

                let mut is_prev_stringize = false;
                self.tokens.reverse();
                self.tokens = self
                    .tokens
                    .into_iter()
                    .filter_map(|t| {
                        if is_prev_stringize && t.ty == TokenType::HashMark {
                            is_prev_stringize = t.stringize;
                            None
                        } else {
                            is_prev_stringize = t.stringize;
                            Some(t)
                        }
                    })
                    .collect::<Vec<_>>();
                self.tokens.reverse();
            }
            _ => unreachable!(),
        }
        self
    }
}

pub struct Preprocessor {
    macros: HashMap<String, Macro>,
    pub env: Box<Env>,
}

impl Preprocessor {
    pub fn new() -> Self {
        Preprocessor {
            macros: HashMap::new(),
            env: Box::new(Env::new(vec![], None)),
        }
    }

    fn next(&mut self) -> Option<Token> {
        if self.eof() {
            return None;
        }
        let pos = self.env.pos;
        let t = Some(mem::replace(&mut self.env.input[pos], Token::default()));
        self.env.pos += 1;
        t
    }

    fn eof(&self) -> bool {
        self.env.pos == self.env.input.len()
    }

    fn get(&mut self, ty: TokenType, msg: &str) -> Token {
        let t = self.next().expect(msg);
        if t.ty != ty {
            t.bad_token(msg);
        }
        t
    }

    fn ident(&mut self, msg: &str) -> String {
        let t = self.next().expect(msg);
        match t.ty {
            TokenType::Ident(s) | TokenType::Str(s, _) => s,
            _ => t.bad_token(msg),
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.env.input.get(self.env.pos)
    }

    fn consume(&mut self, ty: TokenType) -> bool {
        if let Some(t) = self.peek() {
            if t.ty != ty {
                return false;
            }
        } else {
            return false;
        }
        self.env.pos += 1;
        true
    }

    fn read_until_eol(&mut self) -> Vec<Token> {
        let mut v = vec![];
        while let Some(t) = self.next() {
            if t.ty == TokenType::NewLine {
                break;
            }
            v.push(t);
        }
        v
    }

    fn read_one_arg(&mut self) -> Vec<Token> {
        let mut v = vec![];
        let msg = "unclosed macro argument";
        let start = self.peek().expect(msg).clone();
        let mut level = 0;

        while !self.eof() {
            let t = self.peek().expect(msg).clone();
            if level == 0 {
                if t.ty == TokenType::RightParen || t.ty == TokenType::Comma {
                    return v;
                }
            }

            self.next();
            if t.ty == TokenType::LeftParen {
                level += 1;
            } else if t.ty == TokenType::RightParen {
                level -= 1;
            }
            v.push(t);
        }
        start.bad_token(msg);
    }

    fn read_args(&mut self) -> Vec<Vec<Token>> {
        let mut v = vec![];
        if self.consume(TokenType::RightParen) {
            return v;
        }
        v.push(self.read_one_arg());
        while !self.consume(TokenType::RightParen) {
            self.get(TokenType::Comma, "comma expected");
            v.push(self.read_one_arg());
        }
        v
    }

    fn stringize(tokens: &[Token], filename: Rc<String>, buf: Rc<Vec<char>>) -> Token {
        let mut sb = String::new();
        for (i, t) in tokens.iter().enumerate() {
            if i != 0 {
                sb.push(' ');
            }
            sb.push_str(&t.tokstr());
        }

        let len = sb.len();
        Token::new(TokenType::Str(sb, len), 0, filename, buf)
    }

    fn add_special_macro(&mut self, t: &Token) -> bool {
        if t.is_ident("__LINE__") {
            self.env.output.push(Token::new(
                TokenType::Num(t.get_line_number() as i32),
                0,
                t.filename.clone(),
                t.buf.clone(),
            ));
            true
        } else {
            false
        }
    }

    fn apply_objlike(&mut self, tokens: Vec<Token>) {
        for t in tokens {
            if self.add_special_macro(&t) {
                continue;
            } else {
                self.env.output.push(t);
            }
        }
    }

    fn apply_funclike(&mut self, tokens: Vec<Token>, params: &[String], start: &Token) {
        self.get(TokenType::LeftParen, "comma expected");
        let args = self.read_args();
        if params.len() != args.len() {
            start.bad_token("number of parameter does not match");
        }

        for t in tokens {
            if self.add_special_macro(&t) {
                continue;
            }

            match t.ty {
                TokenType::Param(val) => {
                    if t.stringize {
                        self.env
                            .output
                            .push(Self::stringize(&args[val], t.filename, t.buf));
                    } else {
                        self.env.output.append(&mut args[val].clone());
                    }
                }
                _ => self.env.output.push(t),
            }
        }
    }

    fn apply(&mut self, m: Macro, start: &Token) {
        match m.ty {
            MacroType::Objlike => self.apply_objlike(m.tokens),
            MacroType::Funclike(ref params) => self.apply_funclike(m.tokens, params, start),
        }
    }

    fn funclike_macro(&mut self, name: String) {
        let mut params = vec![];
        params.push(self.ident("parameter name expected"));
        while !self.consume(TokenType::RightParen) {
            self.get(TokenType::Comma, "comma expected");
            params.push(self.ident("parameter name expected"));
        }

        let mut m = Macro::new(MacroType::Funclike(params));
        m.tokens = self.read_until_eol();
        m = m.replace_params();
        self.macros.insert(name, m);
    }

    fn objlike_macro(&mut self, name: String) {
        let mut m = Macro::new(MacroType::Objlike);
        m.tokens = self.read_until_eol();
        self.macros.insert(name, m);
    }

    fn define(&mut self) {
        let name = self.ident("macro name expected");
        if self.consume(TokenType::LeftParen) {
            return self.funclike_macro(name);
        }
        self.objlike_macro(name);
    }

    fn include(&mut self) {
        let path = self.ident("string expected");
        let t = self.next().expect("newline expected");
        if t.ty != TokenType::NewLine {
            t.bad_token("newline expected");
        }
        let mut v = tokenize(path, self);
        self.env.output.append(&mut v);
    }

    fn preprocess_impl(&mut self, tokens: Vec<Token>) -> Vec<Token> {
        self.env = Box::new(Env::new(tokens, Some(self.env.clone())));

        while !self.eof() {
            let t = self.next().unwrap();
            let macro_name;
            if let TokenType::Ident(ref name) = t.ty {
                macro_name = Some(name.clone());
            } else {
                macro_name = None;
            }
            if let Some(name) = macro_name {
                if let Some(m) = self.macros.get(&name).cloned() {
                    self.apply(m, &t);
                } else {
                    self.env.output.push(t);
                }
                continue;
            }

            if t.ty != TokenType::HashMark {
                self.env.output.push(t);
                continue;
            }

            let ident = self.ident("identifier expected");
            if &*ident == "define" {
                self.define();
            } else if &*ident == "include" {
                self.include();
            } else {
                t.bad_token("unknown directive");
            }
        }

        let mut output = vec![];
        mem::swap(&mut self.env.output, &mut output);
        self.env = self.env.next.take().unwrap();
        output
    }
}
