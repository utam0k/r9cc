extern crate r9cc;
use r9cc::strtol;

#[macro_use]
extern crate lazy_static;
extern crate num;

use num::traits::FromPrimitive;

use std::env;
use std::process::exit;
use std::sync::Mutex;

// Tokenizer

enum TokenType {
    Num, // Number literal
}

// Token type
#[derive(Default, Debug)]
struct Token {
    ty: i32,       // Token type
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
    Num, // Number literal
}

#[derive(Default, Debug, Clone)]
struct Node {
    ty: i32,                // Node type
    lhs: Option<Box<Node>>, // left-hand side. If None, Node is number etc.
    rhs: Option<Box<Node>>, // right-hand side
    val: i32,               // Number literal
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
}

// Intermediate representation

enum IRType {
    IMM,
    MOV,
    RETURN,
    KILL,
    NOP,
}

impl FromPrimitive for IRType {
    fn from_i64(n: i64) -> Option<Self> {
        use IRType::*;
        match n {
            0 => Some(IMM),
            1 => Some(MOV),
            2 => Some(RETURN),
            3 => Some(KILL),
            4 => Some(NOP),
            _ => None,
        }
    }

    fn from_u64(n: u64) -> Option<Self> {
        use IRType::*;
        match n {
            0 => Some(IMM),
            1 => Some(MOV),
            2 => Some(RETURN),
            3 => Some(KILL),
            4 => Some(NOP),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
struct IR {
    op: i32,
    lhs: i32,
    rhs: i32,
}

impl IR {
    fn new(op: i32, lhs: i32, rhs: i32) -> Self {
        Self {
            op: op,
            lhs: lhs,
            rhs: rhs,
        }
    }
}

lazy_static! {
    static ref INS: Mutex<Vec<IR>> = Mutex::new(vec![]);
}

static mut REGNO: usize = 0;

fn gen_ir_sub(node: Node) -> i32 {
    if node.ty == NodeType::Num as i32 {
        let r: i32;
        unsafe {
            r = REGNO as i32;
            REGNO += 1;
        };
        INS.lock()
            .unwrap()
            .push(IR::new(IRType::IMM as i32, r, node.val));
        return r;
    }

    assert!(node.ty == '+' as i32 || node.ty == '-' as i32);

    let lhs = gen_ir_sub(*node.lhs.unwrap());
    let rhs = gen_ir_sub(*node.rhs.unwrap());

    INS.lock().unwrap().push(IR::new(node.ty, lhs, rhs));
    INS.lock()
        .unwrap()
        .push(IR::new(IRType::KILL as i32, rhs, 0));
    return lhs;
}

fn gen_ir(node: Node) {
    let r = gen_ir_sub(node);
    INS.lock()
        .unwrap()
        .push(IR::new(IRType::RETURN as i32, r, 0));
}

// Register allocator
const REGS: [&str; 8] = ["rdi", "rsi", "r10", "r11", "r12", "r13", "r14", "r15"];

lazy_static! {
    static ref USED: Mutex<[bool; 8]> = Mutex::new([false; 8]);
    static ref REG_MAP: Mutex<[i32; 1000]> = Mutex::new([-1; 1000]);
}

fn used_get(i: usize) -> bool {
    USED.lock().unwrap()[i]
}

fn used_set(i: usize, val: bool) {
    USED.lock().unwrap()[i] = val;
}

fn reg_map_get(i: usize) -> i32 {
    REG_MAP.lock().unwrap()[i]
}

fn reg_map_set(i: usize, val: i32) {
    REG_MAP.lock().unwrap()[i] = val;
}

fn alloc(ir_reg: usize) -> i32 {
    if reg_map_get(ir_reg as usize) != -1 {
        let r = reg_map_get(ir_reg);
        assert!(used_get(r as usize));
        return r;
    }

    for i in 0..REGS.len() {
        if used_get(i) {
            continue;
        }
        used_set(i, true);
        reg_map_set(ir_reg, i as i32);
        return i as i32;
    }
    panic!("register exhauseted");
}

fn kill(r: usize) {
    assert!(used_get(r));
    used_set(r, false);
}

fn alloc_regs() {
    use IRType::*;
    let mut ins_alloced: Vec<IR> = vec![];

    for mut ir in INS.lock().unwrap().clone() {
        match IRType::from_i32(ir.op) {
            Some(IMM) => ir.lhs = alloc(ir.lhs as usize),
            Some(MOV) => {
                ir.lhs = alloc(ir.lhs as usize);
                ir.rhs = alloc(ir.rhs as usize);
            }
            Some(RETURN) => kill(reg_map_get(ir.lhs as usize) as usize),
            Some(KILL) => {
                kill(reg_map_get(ir.lhs as usize) as usize);
                ir.op = IRType::NOP as i32;
            }
            None => match ir.op as u8 as char {
                '+' | '-' => {
                    ir.lhs = alloc(ir.lhs as usize);
                    ir.rhs = alloc(ir.rhs as usize);
                }
                _ => panic!("unknow operator"),
            },
            _ => panic!("unknow operator"),
        }
        ins_alloced.push(ir);
    }

    for i in 0..ins_alloced.len() {
        INS.lock().unwrap()[i] = ins_alloced[i].clone();
    }
}

// Code generator

fn gen_x86() {
    use IRType::*;
    for ir in INS.lock().unwrap().clone() {
        match IRType::from_i32(ir.op) {
            Some(IMM) => print!("  mov {}, {}\n", REGS[ir.lhs as usize], ir.rhs),
            Some(MOV) => print!(
                "  mov {}, {}\n",
                REGS[ir.lhs as usize], REGS[ir.rhs as usize]
            ),
            Some(RETURN) => {
                print!("  mov rax, {}\n", REGS[ir.lhs as usize]);
                print!("  ret\n");
            }
            Some(NOP) | Some(KILL) => (),
            None => match ir.op as u8 as char {
                '+' => print!(
                    "  add {}, {}\n",
                    REGS[ir.lhs as usize], REGS[ir.rhs as usize]
                ),
                '-' => print!(
                    "  sub {}, {}\n",
                    REGS[ir.lhs as usize], REGS[ir.rhs as usize]
                ),
                _ => panic!("unknow operator"),
            },
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
    gen_ir(node.clone());
    alloc_regs();

    // Print the prologue.
    print!(".intel_syntax noprefix\n");
    print!(".global main\n");
    print!("main:\n");

    gen_x86();
}
