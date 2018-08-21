use parse::{Node, NodeType};

#[derive(Debug, Clone)]
pub enum IRType {
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
pub struct IR {
    pub op: IRType,
    pub lhs: usize,
    pub rhs: usize,
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

pub fn gen_ir(node: Node) -> Vec<IR> {
    let (r, mut ins) = gen_ir_sub(vec![], node);
    ins.push(IR::new(IRType::RETURN, r, 0));
    return ins;
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
