#[macro_export]
macro_rules! set {
    ($($x:expr),*) => {{
        #[allow(unused_mut)]
        let mut temp_set = std::collections::BTreeSet::new();
        $(temp_set.insert($x);)*
        temp_set
    }}
}
