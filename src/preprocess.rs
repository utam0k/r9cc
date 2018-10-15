// C preprocessor

use token::{Token, TokenType, bad_token, tokenize};

use std::collections::HashMap;
use std::mem;

pub struct Context {
    macros: HashMap<String, Macro>,
    pub preprocess: Box<Preprocess>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            macros: HashMap::new(),
            preprocess: Box::new(Preprocess::new(vec![], None)),
        }
    }

    pub fn new_preprocess(&mut self, input: Vec<Token>) {
        self.preprocess = Box::new(Preprocess::new(
            input,
            Some(mem::replace(
                &mut self.preprocess,
                Box::new(Preprocess::default()),
            )),
        ));
    }

    pub fn next(&mut self) -> Option<Token> {
        if self.eof() {
            return None;
        }
        let pos = self.preprocess.pos;
        let t = Some(mem::replace(
            &mut self.preprocess.input[pos],
            Token::default(),
        ));
        self.preprocess.pos += 1;
        t
    }

    pub fn eof(&self) -> bool {
        self.preprocess.pos == self.preprocess.input.len()
    }

    pub fn get(&mut self, ty: TokenType, msg: &str) -> Token {
        let t = self.next().expect(msg);
        if t.ty != ty {
            bad_token(&t, msg);
        }
        t
    }

    fn ident(&mut self, msg: &str) -> String {
        let t = self.next().expect(msg);
        match t.ty {
            TokenType::Ident(s) |
            TokenType::Str(s, _) => s,
            _ => bad_token(&t, msg),
        }
    }

    pub fn peek(&self) -> Option<&Token> {
        self.preprocess.input.get(self.preprocess.pos)
    }

    pub fn consume(&mut self, ty: TokenType) -> bool {
        if let Some(t) = self.peek() {
            if t.ty != ty {
                return false;
            }
        } else {
            return false;
        }
        self.preprocess.pos += 1;
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
pub struct Preprocess {
    input: Vec<Token>,
    pos: usize,
    next: Option<Box<Preprocess>>,
}

impl Default for Preprocess {
    fn default() -> Preprocess {
        Preprocess {
            input: vec![],
            pos: 0,
            next: None,
        }
    }
}

impl Preprocess {
    pub fn new(input: Vec<Token>, next: Option<Box<Preprocess>>) -> Self {
        Preprocess {
            input,
            pos: 0,
            next,
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
                                *elem = Token::new(TokenType::Param(n.clone()), 0);
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

fn read_one_arg(ctx: &mut Context) -> Vec<Token> {
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

            ctx.next();
            if t.ty == TokenType::LeftParen {
                level += 1;
            } else if t.ty == TokenType::RightParen {
                level -= 1;
            }
            v.push(t);
        }
    }
    bad_token(&start, msg);
}

fn read_args(ctx: &mut Context) -> Vec<Vec<Token>> {
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

fn stringize(tokens: &Vec<Token>) -> Token {
    let mut sb = String::new();
    for i in 0..tokens.len() {
        let t = &tokens[i];
        if i != 0 {
            sb.push(' ');
        }
        sb.push_str(&t.tokstr());
    }

    let len = sb.len();
    Token::new(TokenType::Str(sb, len), 0)
}

fn apply(m: Macro, ctx: &mut Context) -> Vec<Token> {
    match m.ty {
        MacroType::Objlike => m.tokens,
        MacroType::Funclike(ref params) => {
            let mut v = vec![];
            ctx.get(TokenType::LeftParen, "comma expected");
            let mut args = read_args(ctx);
            if params.len() != args.len() {
                bad_token(&ctx.peek().unwrap(), "number of parameter does not match");
            }

            for t in m.tokens {
                match t.ty {
                    TokenType::Param(val) => {
                        if t.stringize {
                            v.push(stringize(&args[val]));
                        } else {
                            v.append(&mut args[val].clone());
                        }
                    }
                    _ => v.push(t),
                }
            }
            v
        }
    }
}

fn funclike_macro(name: String, ctx: &mut Context) {
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

fn objlike_macro(name: String, ctx: &mut Context) {
    let mut m = Macro::new(MacroType::Objlike);
    m.tokens = ctx.read_until_eol();
    ctx.macros.insert(name, m);
}

fn define(ctx: &mut Context) {
    let name = ctx.ident("macro name expected");
    if ctx.consume(TokenType::LeftParen) {
        return funclike_macro(name, ctx);
    }
    objlike_macro(name, ctx);
}

fn include(ctx: &mut Context) -> Vec<Token> {
    let path = ctx.ident("string expected");
    let t = ctx.next().expect("newline expected");
    if t.ty != TokenType::NewLine {
        bad_token(&t, "newline expected");
    }
    tokenize(path, ctx)
}

pub fn preprocess(tokens: Vec<Token>, ctx: &mut Context) -> Vec<Token> {
    ctx.preprocess = Box::new(Preprocess::new(tokens, Some(ctx.preprocess.clone())));
    let mut v: Vec<Token> = vec![];

    while !ctx.eof() {
        let t = ctx.next().unwrap();
        let macro_name;
        if let TokenType::Ident(ref name) = t.ty {
            macro_name = Some(name.clone());
        } else {
            macro_name = None;
        }
        if let Some(name) = macro_name {
            if let Some(m) = ctx.macros.get(&name).cloned() {
                v.append(&mut apply(m, ctx));
            } else {
                v.push(t);
            }
            continue;
        }


        if t.ty != TokenType::HashMark {
            v.push(t);
            continue;
        }

        let ident = ctx.ident("identifier expected");
        if &*ident == "define" {
            define(ctx);
        } else if &*ident == "include" {
            v.append(&mut include(ctx));
        } else {
            bad_token(&t, "unknown directive");
        }
    }

    ctx.preprocess = ctx.preprocess.next.take().unwrap();
    v
}
