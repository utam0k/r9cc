extern crate r9cc;
use r9cc::strtol;

#[macro_use]
extern crate lazy_static;

use std::env;
use std::process::exit;
use std::sync::Mutex;

// Tokenizer

#[derive(Debug, PartialEq, Clone)]
enum TokenType {
    Num, // Number literal
    Plus,
    Minus,
}

impl From<char> for TokenType {
    fn from(c: char) -> Self {
        match c {
            '+' => TokenType::Plus,
            '-' => TokenType::Minus,
            e => panic!("unknow Token type: {}", e),
        }
    }
}

impl Default for TokenType {
    fn default() -> Self {
        TokenType::Num
    }
}

// Token type
#[derive(Default, Debug)]
struct Token {
    ty: TokenType, // Token type
    val: i32,      // Number literal
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
                ty: TokenType::from(c),
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
                ty: TokenType::Num,
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

#[derive(Debug, Clone, PartialEq)]
enum NodeType {
    Num, // Number literal
    Plus,
    Minus,
}

impl From<TokenType> for NodeType {
    fn from(token_type: TokenType) -> Self {
        match token_type {
            TokenType::Num => NodeType::Num,
            TokenType::Plus => NodeType::Plus,
            TokenType::Minus => NodeType::Minus,
        }
    }
}

impl Default for NodeType {
    fn default() -> Self {
        NodeType::Num
    }
}

#[derive(Default, Debug, Clone)]
struct Node {
    ty: NodeType,           // Node type
    lhs: Option<Box<Node>>, // left-hand side. If None, Node is number etc.
    rhs: Option<Box<Node>>, // right-hand side
    val: i32,               // Number literal
}

impl Node {
    fn new(op: NodeType, lhs: Box<Node>, rhs: Box<Node>) -> Self {
        Self {
            ty: op,
            lhs: Some(lhs),
            rhs: Some(rhs),
            ..Default::default()
        }
    }

    fn new_num(val: i32) -> Self {
        Self {
            ty: NodeType::Num,
            val: val,
            ..Default::default()
        }
    }

    fn number(tokens: &Vec<Token>, pos: usize) -> Self {
        let t = &tokens[pos];
        if t.ty != TokenType::Num {
            panic!("number expected, but got {}", t.input);
        }
        return Self::new_num(t.val);
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

            let op = tokens[pos].ty.clone();
            if op != TokenType::Plus && op != TokenType::Minus {
                break;
            }
            pos += 1;
            lhs = Self::new(
                NodeType::from(op),
                Box::new(lhs),
                Box::new(Self::number(&tokens, pos)),
            );
            pos += 1;
        }

        if tokens.len() != pos {
            panic!("stray token: {}", tokens[pos].input);
        }
        return lhs;
    }
}

// Intermediate representation

#[derive(Debug, Clone)]
enum IRType {
    IMM,
    MOV,
    RETURN,
    KILL,
    NOP,
    ADD,
    SUB,
}

impl From<NodeType> for IRType {
    fn from(node_type: NodeType) -> Self {
        match node_type {
            NodeType::Plus => IRType::ADD,
            NodeType::Minus => IRType::SUB,
            e => panic!("cannot convert: {:?}", e),
        }
    }
}

#[derive(Debug, Clone)]
struct IR {
    op: IRType,
    lhs: usize,
    rhs: usize,
}

impl IR {
    fn new(op: IRType, lhs: usize, rhs: usize) -> Self {
        Self {
            op: op,
            lhs: lhs,
            rhs: rhs,
        }
    }
}

fn gen_ir_sub(mut v: Vec<IR>, node: Node) -> (usize, Vec<IR>) {
    if node.ty == NodeType::Num {
        let r = v.len();
        v.push(IR::new(IRType::IMM, r, node.val as usize));
        return (r, v);
    }

    assert!(node.ty == NodeType::Plus || node.ty == NodeType::Minus);

    let (lhs, ins) = gen_ir_sub(v, *node.lhs.unwrap());
    let (rhs, mut ins) = gen_ir_sub(ins, *node.rhs.unwrap());

    ins.push(IR::new(IRType::from(node.ty), lhs, rhs));
    ins.push(IR::new(IRType::KILL, rhs, 0));
    return (lhs, ins);
}

fn gen_ir(node: Node) -> Vec<IR> {
    let (r, mut ins) = gen_ir_sub(vec![], node);
    ins.push(IR::new(IRType::RETURN, r, 0));
    return ins;
}

// Register allocator

const REGS_N: usize = 8;
const REGS: [&str; REGS_N] = ["rdi", "rsi", "r10", "r11", "r12", "r13", "r14", "r15"];

lazy_static! {
    static ref USED: Mutex<[bool; REGS_N]> = Mutex::new([false; REGS_N]);
    static ref REG_MAP: Mutex<Vec<Option<usize>>> = Mutex::new(vec![None]);
}

fn used_get(i: usize) -> bool {
    USED.lock().unwrap()[i]
}

fn used_set(i: usize, val: bool) {
    USED.lock().unwrap()[i] = val;
}

fn reg_map_get(i: usize) -> Option<usize> {
    REG_MAP.lock().unwrap().get(i).cloned().unwrap()
}

fn reg_map_set(i: usize, val: usize) {
    REG_MAP.lock().unwrap()[i] = Some(val);
}

fn alloc(ir_reg: usize) -> usize {
    if let Some(r) = reg_map_get(ir_reg) {
        assert!(used_get(r));
        return r;
    }

    for i in 0..REGS.len() {
        if used_get(i) {
            continue;
        }
        used_set(i, true);
        reg_map_set(ir_reg, i);
        return i;
    }
    panic!("register exhauseted");
}

fn kill(r: usize) {
    assert!(used_get(r));
    used_set(r, false);
}

fn alloc_regs(irv: Vec<IR>) -> Vec<IR> {
    use IRType::*;
    let mut new: Vec<IR> = vec![];
    let irv_len = irv.len();

    unsafe {
        REG_MAP.lock().unwrap().set_len(irv_len);
    }

    for i in 0..irv_len {
        let mut ir = irv[i].clone();
        match ir.op {
            IMM => ir.lhs = alloc(ir.lhs),
            RETURN => kill(reg_map_get(ir.lhs).unwrap()),
            KILL => {
                kill(reg_map_get(ir.lhs).unwrap());
                ir.op = IRType::NOP;
            }
            ADD | SUB | MOV => {
                ir.lhs = alloc(ir.lhs);
                ir.rhs = alloc(ir.rhs);
            }
            op => panic!("unknow operator: {:?}", op),
        }
        new.push(ir);
    }
    return new;
}

// Code generator

fn gen_x86(irv: Vec<IR>) {
    use IRType::*;
    for ir in irv.clone() {
        match ir.op {
            IMM => print!("  mov {}, {}\n", REGS[ir.lhs], ir.rhs),
            MOV => print!("  mov {}, {}\n", REGS[ir.lhs], REGS[ir.rhs]),
            RETURN => {
                print!("  mov rax, {}\n", REGS[ir.lhs]);
                print!("  ret\n");
            }
            ADD => print!("  add {}, {}\n", REGS[ir.lhs], REGS[ir.rhs]),
            SUB => print!("  sub {}, {}\n", REGS[ir.lhs], REGS[ir.rhs]),
            NOP | KILL => (),
        }
    }
}

fn main() {
    let mut args = env::args();
    if args.len() != 2 {
        eprint!("Usage: 9cc <code>\n");
        return;
    }

    // Tokenize and parse.
    let tokens = tokenize(args.nth(1).unwrap());
    let node = Node::expr(tokens);
    let irv = gen_ir(node);
    let irv_alloced = alloc_regs(irv);

    // Print the prologue.
    print!(".intel_syntax noprefix\n");
    print!(".global main\n");
    print!("main:\n");

    gen_x86(irv_alloced);
}
