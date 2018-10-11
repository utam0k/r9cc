use token::{Token, TokenType, bad_token, tokenize};

pub fn preprocess(tokens: Vec<Token>) -> Vec<Token> {
    use self::TokenType::*;

    let mut v: Vec<Token> = vec![];
    let mut tokens = tokens.into_iter();

    while tokens.len() != 0 {
        let mut t = tokens.next().unwrap();
        if t.ty != HashMark {
            v.push(t);
            continue;
        }

        t = tokens.next().unwrap();
        if t.ty != Ident("include".into()) {
            bad_token(&t, "'include' expected");
        }

        t = tokens.next().unwrap();
        let path;
        if let Str(ref s, _) = t.ty {
            path = s.clone();
        } else {
            bad_token(&t, "'include' expected");
        }

        t = tokens.next().unwrap();
        if t.ty != NewLine {
            bad_token(&t, "newline expected");
        }

        let mut nv = tokenize(path);
        v.append(&mut nv);
    }
    v
}
