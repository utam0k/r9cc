// C preprocessor

use token::{Token, tokenize};
use TokenType;

use std::collections::HashMap;
use std::rc::Rc;
use std::mem;

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

    pub fn new_env(&mut self, input: Vec<Token>) {
        self.env = Box::new(Env::new(
            input,
            Some(mem::replace(&mut self.env, Box::new(Env::default()))),
        ));
    }

    pub fn next(&mut self) -> Option<Token> {
        if self.eof() {
            return None;
        }
        let pos = self.env.pos;
        let t = Some(mem::replace(&mut self.env.input[pos], Token::default()));
        self.env.pos += 1;
        t
    }

    pub fn eof(&self) -> bool {
        self.env.pos == self.env.input.len()
    }

    pub fn get(&mut self, ty: TokenType, msg: &str) -> Token {
        let t = self.next().expect(msg);
        if t.ty != ty {
            t.bad_token(msg);
        }
        t
    }

    fn ident(&mut self, msg: &str) -> String {
        let t = self.next().expect(msg);
        match t.ty {
            TokenType::Ident(s) |
            TokenType::Str(s, _) => s,
            _ => t.bad_token(msg),
        }
    }

    pub fn peek(&self) -> Option<&Token> {
        self.env.input.get(self.env.pos)
    }

    pub fn consume(&mut self, ty: TokenType) -> bool {
        if let Some(t) = self.peek() {
            if t.ty != ty {
                return false;
            }
        } else {
            return false;
        }
        self.env.pos += 1;
        return true;
    }

    pub fn read_until_eol(&mut self) -> Vec<Token> {
        let mut v = vec![];
        while let Some(t) = self.next() {
            if t.ty == TokenType::NewLine {
                break;
            }
            v.push(t);
        }
        v
    }
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
    pub fn new(ty: MacroType) -> Self {
        Macro { ty, tokens: vec![] }
    }
}

fn is_ident(t: &Token, s: &str) -> bool {
    match t.ty {
        TokenType::Ident(ref name) => name == s,
        _ => false,
    }
}

fn replace_params(m: &mut Macro) {
    match m.ty {
        MacroType::Funclike(ref params) => {
            let mut map = HashMap::new();
            for i in 0..params.len() {
                let name = params[i].clone();
                map.insert(name, i);
            }

            for i in 0..m.tokens.len() {
                let t = &m.tokens[i].clone();
                match t.ty {
                    TokenType::Ident(ref name) => {
                        if let Some(n) = map.get(name) {
                            if let Some(elem) = m.tokens.get_mut(i) {
                                *elem = Token::new(
                                    TokenType::Param(n.clone()),
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
            let mut v = vec![];
            let mut i = 0;
            while i < m.tokens.len() {
                let t1 = m.tokens[i].clone();
                if i != m.tokens.len() - 1 && t1.ty == TokenType::HashMark {
                    if let Some(elem) = m.tokens.get_mut(i + 1) {
                        elem.stringize = true;
                        v.push(elem.clone());
                        i += 1;
                    } else {
                        v.push(t1)
                    }
                } else {
                    v.push(t1)
                }
                i += 1;
            }
            m.tokens = v;
        }
        _ => unreachable!(),
    }
}

fn read_one_arg(ctx: &mut Preprocessor) -> Vec<Token> {
    let mut v = vec![];
    let msg = "unclosed macro argument";
    let start = ctx.peek().expect(msg).clone();
    let mut level = 0;

    while !ctx.eof() {
        let t = ctx.peek().expect(msg).clone();
        if level == 0 {
            if t.ty == TokenType::RightParen || t.ty == TokenType::Comma {
                return v;
            }
        }

        ctx.next();
        if t.ty == TokenType::LeftParen {
            level += 1;
        } else if t.ty == TokenType::RightParen {
            level -= 1;
        }
        v.push(t);
    }
    start.bad_token(msg);
}

fn read_args(ctx: &mut Preprocessor) -> Vec<Vec<Token>> {
    let mut v = vec![];
    if ctx.consume(TokenType::RightParen) {
        return v;
    }
    v.push(read_one_arg(ctx));
    while !ctx.consume(TokenType::RightParen) {
        ctx.get(TokenType::Comma, "comma expected");
        v.push(read_one_arg(ctx));
    }
    v
}

fn stringize(tokens: &Vec<Token>, filename: Rc<String>, buf: Rc<Vec<char>>) -> Token {
    let mut sb = String::new();
    for i in 0..tokens.len() {
        let t = &tokens[i];
        if i != 0 {
            sb.push(' ');
        }
        sb.push_str(&t.tokstr());
    }

    let len = sb.len();
    Token::new(TokenType::Str(sb, len), 0, filename, buf)
}

fn add_special_macro(t: &Token, ctx: &mut Preprocessor) -> bool {
    if is_ident(&t, "__LINE__") {
        ctx.env.output.push(Token::new(
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

fn apply_objlike(tokens: Vec<Token>, ctx: &mut Preprocessor) {
    for t in tokens {
        if add_special_macro(&t, ctx) {
            continue;
        } else {
            ctx.env.output.push(t);
        }
    }
}

fn apply_funclike(tokens: Vec<Token>, params: &Vec<String>, start: &Token, ctx: &mut Preprocessor) {
    ctx.get(TokenType::LeftParen, "comma expected");
    let mut args = read_args(ctx);
    if params.len() != args.len() {
        start.bad_token("number of parameter does not match");
    }

    for t in tokens {
        if add_special_macro(&t, ctx) {
            continue;
        }

        match t.ty {
            TokenType::Param(val) => {
                if t.stringize {
                    ctx.env.output.push(
                        stringize(&args[val], t.filename, t.buf),
                    );
                } else {
                    ctx.env.output.append(&mut args[val].clone());
                }
            }
            _ => ctx.env.output.push(t),
        }
    }
}

fn apply(m: Macro, start: &Token, ctx: &mut Preprocessor) {
    match m.ty {
        MacroType::Objlike => apply_objlike(m.tokens, ctx),
        MacroType::Funclike(ref params) => apply_funclike(m.tokens, params, start, ctx),
    }
}

fn funclike_macro(name: String, ctx: &mut Preprocessor) {
    let mut params = vec![];
    params.push(ctx.ident("parameter name expected"));
    while !ctx.consume(TokenType::RightParen) {
        ctx.get(TokenType::Comma, "comma expected");
        params.push(ctx.ident("parameter name expected"));
    }

    let mut m = Macro::new(MacroType::Funclike(params));
    m.tokens = ctx.read_until_eol();
    replace_params(&mut m);
    ctx.macros.insert(name, m);
}

fn objlike_macro(name: String, ctx: &mut Preprocessor) {
    let mut m = Macro::new(MacroType::Objlike);
    m.tokens = ctx.read_until_eol();
    ctx.macros.insert(name, m);
}

fn define(ctx: &mut Preprocessor) {
    let name = ctx.ident("macro name expected");
    if ctx.consume(TokenType::LeftParen) {
        return funclike_macro(name, ctx);
    }
    objlike_macro(name, ctx);
}

fn include(ctx: &mut Preprocessor) {
    let path = ctx.ident("string expected");
    let t = ctx.next().expect("newline expected");
    if t.ty != TokenType::NewLine {
        t.bad_token("newline expected");
    }
    let mut v = tokenize(path, ctx);
    ctx.env.output.append(&mut v);
}

pub fn preprocess(tokens: Vec<Token>, ctx: &mut Preprocessor) -> Vec<Token> {
    ctx.env = Box::new(Env::new(tokens, Some(ctx.env.clone())));

    while !ctx.eof() {
        let t = ctx.next().unwrap();
        let macro_name;
        if let TokenType::Ident(ref name) = t.ty {
            macro_name = Some(name.clone());
        } else {
            macro_name = None;
        }
        if let Some(name) = macro_name {
            if let Some(mut m) = ctx.macros.get(&name).cloned() {
                apply(m, &t, ctx);
            } else {
                ctx.env.output.push(t);
            }
            continue;
        }


        if t.ty != TokenType::HashMark {
            ctx.env.output.push(t);
            continue;
        }

        let ident = ctx.ident("identifier expected");
        if &*ident == "define" {
            define(ctx);
        } else if &*ident == "include" {
            include(ctx);
        } else {
            t.bad_token("unknown directive");
        }
    }

    let mut output = vec![];
    mem::swap(&mut ctx.env.output, &mut output);
    ctx.env = ctx.env.next.take().unwrap();
    output
}
