// C preprocessor

use token::{Token, TokenType, bad_token, tokenize};

use std::collections::HashMap;
use std::vec::IntoIter;
use std::mem;

pub struct Context {
    pub macros: HashMap<String, Vec<Token>>,
    pub preprocess: Box<Preprocess>,
}

impl Context {
    pub fn new(input: IntoIter<Token>) -> Self {
        Context {
            macros: HashMap::new(),
            preprocess: Box::new(Preprocess::new(input, None)),
        }
    }

    pub fn new_preprocess(&mut self, input: IntoIter<Token>) {
        self.preprocess = Box::new(Preprocess::new(
            input,
            Some(mem::replace(
                &mut self.preprocess,
                Box::new(Preprocess::default()),
            )),
        ));
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.preprocess.input.next()
    }

    pub fn eof(&self) -> bool {
        self.preprocess.input.len() == 0
    }
}

#[derive(Clone)]
pub struct Preprocess {
    input: IntoIter<Token>,
    next: Option<Box<Preprocess>>,
}

impl Default for Preprocess {
    fn default() -> Preprocess {
        Preprocess {
            input: vec![].into_iter(),
            next: None,
        }
    }
}

impl Preprocess {
    pub fn new(input: IntoIter<Token>, next: Option<Box<Preprocess>>) -> Self {
        Preprocess { input, next }
    }
}

fn get(msg: &str, ctx: &mut Context) -> String {
    let t = ctx.next_token().unwrap();
    match t.ty {
        TokenType::Ident(s) |
        TokenType::Str(s, _) => s,
        _ => bad_token(&t, msg),
    }
}

fn define(ctx: &mut Context) {
    let name = get("macro name expected", ctx);
    let mut v2: Vec<Token> = vec![];
    while let Some(t) = ctx.next_token() {
        if t.ty == TokenType::NewLine {
            break;
        }
        v2.push(t);
    }
    ctx.macros.insert(name, v2);
}

fn include(ctx: &mut Context) -> Vec<Token> {
    let path = get("string expected", ctx);
    let t = ctx.next_token().unwrap();
    if t.ty != TokenType::NewLine {
        bad_token(&t, "newline expected");
    }
    tokenize(path, ctx)
}

pub fn preprocess(tokens: Vec<Token>, ctx: &mut Context) -> Vec<Token> {
    ctx.preprocess = Box::new(Preprocess::new(
        tokens.into_iter(),
        Some(ctx.preprocess.clone()),
    ));
    let mut v: Vec<Token> = vec![];

    while !ctx.eof() {
        let t = ctx.next_token().unwrap();
        let macro_name;
        if let TokenType::Ident(ref name) = t.ty {
            macro_name = Some(name.clone());
        } else {
            macro_name = None;
        }
        if let Some(name) = macro_name {
            if let Some(m) = ctx.macros.get(&name) {
                v.append(&mut m.clone());
            } else {
                v.push(t);
            }
            continue;
        }


        if t.ty != TokenType::HashMark {
            v.push(t);
            continue;
        }

        let ident = get("identifier expected", ctx);
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
