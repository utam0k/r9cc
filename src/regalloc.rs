use ir::{IRType, IR};
use REGS_N;

use std::sync::Mutex;

// Need reserving initialized memory area in order to use set_len().
const INIT_ARRAY_SIZE: usize = 10;

lazy_static! {
    static ref USED: Mutex<[bool; REGS_N]> = Mutex::new([false; REGS_N]);
    static ref REG_MAP: Mutex<Vec<Option<usize>>> = Mutex::new(vec![None; INIT_ARRAY_SIZE]);
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

    for i in 0..REGS_N {
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

pub fn alloc_regs(irv: &mut Vec<IR>) {
    use self::IRType::*;
    let irv_len = irv.len();

    if irv_len > INIT_ARRAY_SIZE {
        unsafe {
            REG_MAP.lock().unwrap().set_len(irv_len);
        }
    }

    for i in 0..irv_len {
        let mut ir = irv[i].clone();
        match ir.op {
            IMM | RETURN | ALLOCA => ir.lhs = Some(alloc(ir.lhs.unwrap())),
            MOV | LOAD | STORE | SUB | MUL | DIV => {
                ir.lhs = Some(alloc(ir.lhs.unwrap()));
                ir.rhs = Some(alloc(ir.rhs.unwrap()));
            }
            ADD(imm) => {
                ir.lhs = Some(alloc(ir.lhs.unwrap()));
                if imm.is_none() {
                    ir.rhs = Some(alloc(ir.rhs.unwrap()));
                }
            }
            KILL => {
                kill(reg_map_get(ir.lhs.unwrap()).unwrap());
                ir.op = NOP;
            }
            op => panic!("unknow operator: {:?}", op),
        }
        irv[i] = ir;
    }
}
