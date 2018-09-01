#[macro_use]
pub mod util {
    #[macro_export]
    macro_rules! matches(
        ($e:expr, $p:pat) => (
            match $e {
                $p => true,
                _ => false
            }
        )
    );
}
