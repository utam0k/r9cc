use parse::{Type, Ctype};

pub fn size_of(ty: Box<&Type>) -> usize {
    use self::Ctype::*;
    match ty.ty {
        Char => 1,
        Int => 4,
        Ptr(_) => 8,
        Ary(ref ary_of, len) => size_of(Box::new(ary_of)) * len,
    }
}

pub fn align_of(ty: Box<&Type>) -> usize {
    use self::Ctype::*;
    match ty.ty {
        Char => 1,
        Int => 4,
        Ptr(_) => 8,
        Ary(ref ary_of, _) => align_of(Box::new(ary_of)),
    }
}

pub fn roundup(x: usize, align: usize) -> usize {
    (x + align - 1) & !(align - 1)
}
