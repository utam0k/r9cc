extern crate r9cc;
use r9cc::strtol;

use std::env;
use std::process::exit;

const REGS: [&str; 8] = ["rdi", "rsi", "r10", "r11", "r12", "r13", "r14", "r15"];
static mut cur: usize = 0;

// Tokenizer

enum TokenType {
    Num, // Number literal
}

// Token type
#[derive(Default, Debug)]
struct Token {
    ty: i32, // Token type
    val: i32, // Number literal
    input: String, // Token string (for error reporting)
}

fn tokenize(mut p: String) -> Vec<Token> {
    // Tokenized input is stored to this vec.
    let mut tokens: Vec<Token> = vec![];

    let org = p.clone();
    while let Some(c) = p.chars().nth(0) {
        // Skip whitespce
        if c.is_whitespace() {
            p = p.split_off(1); // p++
            continue;
        }

        // + or -
        if c == '+' || c == '-' {
            let token = Token {
                ty: c as i32,
                input: org.clone(),
                ..Default::default()
            };
            p = p.split_off(1); // p++
            tokens.push(token);
            continue;
        }

        // Number
        if c.is_ascii_digit() {
            let (n, mut remaining) = strtol(&p);
            p = remaining;
            let token = Token {
                ty: TokenType::Num as i32,
                input: org.clone(),
                val: n.unwrap() as i32,
            };
            tokens.push(token);
            continue;
        }

        eprint!("cannot tokenize: {}\n", p);
        exit(1);
    }
    return tokens;
}

// Recursive-descendent parser

enum NodeType {
    Num,
}

#[derive(Default, Debug, Clone)]
struct Node {
    ty: i32, // Node type
    lhs: Option<Box<Node>>, // left-hand side
    rhs: Option<Box<Node>>, // right-hand side
    val: i32, // Number literal
}

impl Node {
    fn new(op: i32, lhs: Box<Node>, rhs: Box<Node>) -> Self {
        Self {
            ty: op,
            lhs: Some(lhs),
            rhs: Some(rhs),
            ..Default::default()
        }
    }

    fn new_num(val: i32) -> Self {
        Self {
            ty: NodeType::Num as i32,
            val: val,
            ..Default::default()
        }
    }

    fn number(tokens: &Vec<Token>, pos: usize) -> Self {
        if tokens[pos].ty == TokenType::Num as i32 {
            let val = tokens[pos].val;
            return Self::new_num(val);
        }
        panic!("number expected, but got {}", tokens[pos].input);
    }

    pub fn expr(tokens: Vec<Token>) -> Self {
        let mut pos = 0;
        let mut lhs = Self::number(&tokens, pos);
        pos += 1;
        if tokens.len() == pos {
            return lhs;
        }

        loop {
            if tokens.len() == pos {
                break;
            }

            let op = tokens[pos].ty;
            if op != '+' as i32 && op != '-' as i32 {
                println!("Break op: {}", op);
                break;
            }
            pos += 1;
            lhs = Self::new(op, Box::new(lhs), Box::new(Self::number(&tokens, pos)));
            pos += 1;
        }

        if tokens.len() != pos {
            panic!("stray token: {}", tokens[pos].input);
        }
        return lhs;
    }

    // Code generator
    fn gen(self) -> String {
        if self.ty == NodeType::Num as i32 {
            let reg: &str;
            unsafe {
                if cur > REGS.len() {
                    panic!("register exhausted");
                }
                reg = REGS[cur];
                cur += 1;
            }
            print!("  mov {}, {}\n", reg, self.val);
            return reg.into();
        }

        let dst = self.lhs.unwrap().gen();
        let src = self.rhs.unwrap().gen();
        match self.ty as u8 as char {
            '+' => {
                print!("  add {}, {}\n", dst, src);
                return dst;
            }
            '-' => {
                print!("  sub {}, {}\n", dst, src);
                return dst;
            }
            _ => panic!("unknown operator"),
        }
    }
}


fn main() {
    let mut args = env::args();
    if args.len() != 2 {
        eprint!("Usage: 9cc <code>\n");
        return;
    }

    let tokens = tokenize(args.nth(1).unwrap());
    let node = Node::expr(tokens);

    // Print the prologue
    print!(".intel_syntax noprefix\n");
    print!(".global main\n");
    print!("main:\n");

    // Generate code while descending the parse tree.
    print!("  mov rax, {}\n", node.gen());
    print!("  ret\n");
}
