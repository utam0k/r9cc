pub mod gen_ir;
pub mod gen_x86;
pub mod irdump;
pub mod parse;
pub mod preprocess;
pub mod regalloc;
pub mod sema;
pub mod token;
mod util;

#[macro_use]
extern crate lazy_static;

const REGS_N: usize = 7;

#[macro_export]
macro_rules! matches(
    ($e:expr, $p:pat) => (
        match $e {
            $p => true,
            _ => false
        }
    )
);

// Token type
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Num(i32),            // Number literal
    Str(String, usize),  // String literal. (str, len)
    CharLiteral(String), // Char literal.
    Ident(String),       // Identifier
    Param(usize),        // Function-like macro parameter
    Arrow,               // ->
    Extern,              // "extern"
    Typedef,             // "typedef"
    Int,                 // "int"
    Char,                // "char"
    Void,                // "void"
    Struct,              // "struct"
    Plus,                // +
    Minus,               // -
    Mul,                 // *
    Div,                 // /
    And,                 // &
    Dot,                 // .
    Comma,               // ,
    Exclamation,         // !
    Question,            // ?
    VerticalBar,         // |
    Hat,                 // ^
    Colon,               // :
    HashMark,            // #
    If,                  // "if"
    Else,                // "else"
    For,                 // "for"
    Do,                  // "do"
    While,               // "while"
    Break,               // "break"
    EQ,                  // ==
    NE,                  // !=
    LE,                  // <=
    GE,                  // >=
    Semicolon,           // ;
    LeftParen,           // (
    RightParen,          // )
    LeftBracket,         // [
    RightBracket,        // ]
    LeftBrace,           // {
    RightBrace,          // }
    LeftAngleBracket,    // <
    RightAngleBracket,   // >
    Equal,               // =
    Logor,               // ||
    Logand,              // &&
    SHL,                 // <<
    Inc,                 // ++
    Dec,                 // --
    MulEQ,               // *=
    DivEQ,               // /=
    ModEQ,               // %=
    AddEQ,               // +=
    SubEQ,               // -=
    ShlEQ,               // <<=
    ShrEQ,               // >>=
    BitandEQ,            // &=
    XorEQ,               // ^=
    BitorEQ,             // |=
    SHR,                 // >>
    Mod,                 // %
    Return,              // "return"
    Sizeof,              // "sizeof"
    Alignof,             // "_Alignof"
    NewLine,             // preprocessor-only token
}

// Character Kind
#[derive(Debug, PartialEq)]
pub enum CharacterType {
    Whitespace, // ' '
    NewLine,    // ' \n'
    Alphabetic,
    Digit,
    NonAlphabetic(char),
    Unknown(char),
}

impl TokenType {
    fn new_single_letter(c: char) -> Option<Self> {
        use self::TokenType::*;
        match c {
            '+' => Some(Plus),
            '-' => Some(Minus),
            '*' => Some(Mul),
            '/' => Some(Div),
            '&' => Some(And),
            ';' => Some(Semicolon),
            '=' => Some(Equal),
            '(' => Some(LeftParen),
            ')' => Some(RightParen),
            '[' => Some(LeftBracket),
            ']' => Some(RightBracket),
            '{' => Some(LeftBrace),
            '}' => Some(RightBrace),
            '<' => Some(LeftAngleBracket),
            '>' => Some(RightAngleBracket),
            ',' => Some(Comma),
            '.' => Some(Dot),
            '!' => Some(Exclamation),
            '?' => Some(Question),
            '|' => Some(VerticalBar),
            '^' => Some(Hat),
            '%' => Some(Mod),
            ':' => Some(Colon),
            '#' => Some(HashMark),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Ctype {
    Int,
    Char,
    Void,
    Ptr(Box<Type>),           // ptr of
    Ary(Box<Type>, usize),    // ary of, len
    Struct(Vec<parse::Node>), // members
    Func(Box<Type>),
}

impl Default for Ctype {
    fn default() -> Ctype {
        Ctype::Int
    }
}

#[derive(Debug, Clone)]
pub struct Type {
    pub ty: Ctype,
    pub size: usize,  // sizeof
    pub align: usize, // alignof
}

impl Default for Type {
    fn default() -> Type {
        Type {
            ty: Ctype::default(),
            size: 4,
            align: 4,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Scope {
    Local(usize),                // offset
    Global(String, usize, bool), // data, len, is_extern
}

#[derive(Debug, Clone)]
pub struct Var {
    ty: Box<Type>,
    pub name: String,
    pub scope: Scope,
}

impl Var {
    fn new(ty: Box<Type>, name: String, scope: Scope) -> Self {
        Var { ty, name, scope }
    }

    fn new_global(ty: Box<Type>, name: String, data: String, len: usize, is_extern: bool) -> Self {
        Var::new(ty, name.clone(), Scope::Global(data, len, is_extern))
    }
}
