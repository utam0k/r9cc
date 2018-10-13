use token::{Token, TokenType, bad_token, tokenize};

use std::collections::HashMap;
use std::sync::Mutex;

lazy_static! {
    static ref DEFINED: Mutex<HashMap<String, Vec<Token>>> = Mutex::new(HashMap::new());
}

pub fn preprocess(tokens: Vec<Token>) -> Vec<Token> {
    use self::TokenType::*;

    let mut v: Vec<Token> = vec![];
    let mut tokens = tokens.into_iter();

    while tokens.len() != 0 {
        let mut t = tokens.next().unwrap();
        let macro_name;
        if let Ident(ref name) = t.ty {
            macro_name = Some(name.clone());
        } else {
            macro_name = None;
        }
        if let Some(name) = macro_name {
            if let Some(macro_) = DEFINED.lock().unwrap().get(&name) {
                v.append(&mut macro_.clone());
            } else {
                v.push(t);
            }
            continue;
        }


        if t.ty != HashMark {
            v.push(t);
            continue;
        }

        if let Ident(ident) = tokens.next().unwrap().ty {
            if &*ident == "define" {
                t = tokens.next().unwrap();
                if let TokenType::Ident(name) = t.ty {
                    let mut v2: Vec<Token> = vec![];
                    while let Some(t) = tokens.next() {
                        if t.ty == TokenType::NewLine {
                            break;
                        }
                        v2.push(t);
                    }
                    DEFINED.lock().unwrap().insert(name, v2);
                } else {
                    bad_token(&t, "macro name expected");
                }
                continue;
            }

            if &*ident == "include" {
                t = tokens.next().unwrap();
                let path;
                if let Str(ref s, _) = t.ty {
                    path = s.clone();
                } else {
                    bad_token(&t, "string expected");
                }

                t = tokens.next().unwrap();
                if t.ty != NewLine {
                    bad_token(&t, "newline expected");
                }

                let mut nv = tokenize(path);
                v.append(&mut nv);
            }
        } else {
            bad_token(&t, "identifier expected");
        }
    }
    v
}
