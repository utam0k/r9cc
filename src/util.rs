use parse::{Type, Ctype};

pub fn size_of(ty: &Type) -> usize {
    use self::Ctype::*;
    match ty.ty {
        Int => 4,
        Ptr(_) => 8,
        Ary(ref ary_of, len) => size_of(&*ary_of) * len,
    }
}
