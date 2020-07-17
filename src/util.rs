pub mod interval {
    use std::u32;
    use interval_map::Interval;

    pub fn empty() -> Interval<u32> {
        Interval::new(u32::MIN, u32::MIN)
    }

    pub fn singleton(symbol: u32) -> Interval<u32> {
        Interval::new(symbol, symbol + 1)
    }

    pub fn less_than(symbol: u32) -> Interval<u32> {
        Interval::new(u32::MIN, symbol)
    }

    pub fn greater_than(symbol: u32) -> Interval<u32> {
        Interval::new(symbol + 1, u32::MAX)
    }

    pub fn closed(lower: u32, upper: u32) -> Interval<u32> {
        Interval::new(lower, upper + 1)
    }

    pub fn open(lower: u32, upper: u32) -> Interval<u32> {
        Interval::new(lower + 1, upper)
    }

    pub fn all() -> Interval<u32> {
        Interval::new(u32::MIN, u32::MAX)
    }
}

#[macro_export]
macro_rules! set {
    ($($x:expr),*) => {{
        #[allow(unused_mut)]
        let mut temp_set = std::collections::BTreeSet::new();
        $(temp_set.insert($x);)*
        temp_set
    }}
}
