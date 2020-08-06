use finite_automata::Enfa;
use re_bootstrap::{
    Re as ReBootstrap,
    StateGenerator,
};

pub struct Re {
    re: ReBootstrap,
}

impl Re {
    pub fn new(_expression: &str) -> Re {
        panic!("Not implemented")
    }

    pub fn compile(&mut self) {
        self.re.compile()
    }

    pub fn is_match(&self, text: &str) -> bool {
        self.re.is_match(text)
    }

    pub fn as_enfa<S: Clone + Ord, G: StateGenerator<State = S>>(&self, states: &mut G) -> Enfa<S, u32> {
        self.re.as_enfa(states)
    }
}
