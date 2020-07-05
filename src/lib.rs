use std::ops::AddAssign;

use num::{Bounded, One};

#[macro_use]
pub mod util;
pub mod re;

pub use crate::re::RE;

pub trait StateGenerator {
    type State;

    fn next_initial(&mut self) -> Self::State;
    fn next_sink(&mut self) -> Self::State;
    fn next_final(&mut self) -> Self::State;
    fn disable_final(&mut self) -> &mut Self;
    fn enable_final(&mut self) -> &mut Self;
}

pub struct SimpleStateGenerator<S> {
    state: S,
}

impl<S: Bounded> SimpleStateGenerator<S> {
    pub fn new() -> SimpleStateGenerator<S> {
        SimpleStateGenerator { state: S::min_value() }
    }
}

impl<S: AddAssign + Copy + One> StateGenerator for SimpleStateGenerator<S> {
    type State = S;

    fn next_initial(&mut self) -> S {
        let state = self.state;
        self.state += S::one();
        state
    }

    fn next_sink(&mut self) -> S {
        let state = self.state;
        self.state += S::one();
        state
    }

    fn next_final(&mut self) -> S {
        let state = self.state;
        self.state += S::one();
        state
    }

    fn disable_final(&mut self) -> &mut SimpleStateGenerator<S> {
        self
    }

    fn enable_final(&mut self) -> &mut SimpleStateGenerator<S> {
        self
    }
}

