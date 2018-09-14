pub fn roundup(x: usize, align: usize) -> usize {
    (x + align - 1) & !(align - 1)
}
