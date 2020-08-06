use std::{
    collections::BTreeSet as Set,
    u32,
};
use ::interval_map::{
    Interval,
    interval_map,
};
use finite_automata::{
    Enfa,
    Nfa,
    Dfa,
    Subsume,
    states_contains_from,
    states_contains_all_from,
};
use crate::{
    StateGenerator,
    SimpleStateGenerator,
};

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Expression {
    SymbolSet { intervals: Vec<Interval<u32>> },
    NegatedSymbolSet { intervals: Vec<Interval<u32>> },
    Alternation { res: Vec<Re> },
    Concatenation { res: Vec<Re> },
    Repetition { re: Box<Re>, min: Option<u32>, max: Option<u32> },
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Re {
    expression: Expression,
    dfa: Option<Dfa<Set<u32>, u32>>,
}

impl Re {
    pub fn new(expression: Expression) -> Re {
        Re { expression, dfa: None }
    }

    pub fn compile(&mut self) {
        if self.dfa.is_none() {
            self.dfa = Some(Dfa::from(&self.as_enfa(&mut SimpleStateGenerator::new())));
        }
    }

    pub fn is_match(&self, text: &str) -> bool {
        if let Some(dfa) = self.dfa.as_ref() {
            let mut source_index = dfa.initial_index();
            for character in text.chars() {
                if let Some(transition_index) = dfa.transitions_contains_outgoing((source_index, &character.into())) {
                    let (_, _, target_index) = dfa.transitions_index(transition_index);
                    source_index = target_index;
                } else { return false; }
            }
            dfa.is_final(source_index)
        } else {
            panic!("regular expression must be compiled")
        }
    }

    pub fn as_enfa<S: Clone + Ord, G: StateGenerator<State = S>>(&self, states: &mut G) -> Enfa<S, u32> {
        match &self.expression {
            Expression::SymbolSet { intervals } => {
                let mut sym = Enfa::new(states.next_initial());
                let sym_final_index = sym.states_insert(states.next_final());
                if intervals.len() > 0 {
                    for interval in intervals {
                        sym.transitions_insert((sym.initial_index(), interval.clone(), sym_final_index));
                    }
                } else {
                    sym.transitions_insert((sym.initial_index(), Interval::empty(), sym_final_index));
                }
                sym.set_final(sym_final_index);
                sym
            },
            Expression::NegatedSymbolSet { intervals } => {
                let mut neg = Enfa::new(states.next_initial());
                let neg_final_index = neg.states_insert(states.next_final());
                let mut negated_intervals = interval_map![Interval::all() => true];
                for interval in intervals {
                    negated_intervals.update(&interval, |_| Some(false));
                }
                for (negated_interval, is_negated) in negated_intervals {
                    if is_negated {
                        neg.transitions_insert((neg.initial_index(), negated_interval, neg_final_index));
                    }
                }
                neg.set_final(neg_final_index);
                neg
            },
            Expression::Alternation { res } => {
                let mut alt = Enfa::new(states.next_initial());
                let alt_final_index = alt.states_insert(states.next_final());
                for re in res {
                    let fa = re.as_enfa(states.disable_final());
                    alt.subsume(&fa);
                    let fa_initial_index = states_contains_from(&alt, &fa, fa.initial_index()).expect("state does not exist");
                    alt.transitions_insert((alt.initial_index(), Interval::empty(), fa_initial_index));
                    for fa_final_index in fa.final_indices() {
                        let fa_final_index = states_contains_from(&alt, &fa, fa_final_index).expect("state does not exist");
                        alt.transitions_insert((fa_final_index, Interval::empty(), alt_final_index));
                    }
                }
                alt.set_final(alt_final_index);
                alt
            },
            Expression::Concatenation { res } => {
                let mut con = Enfa::new(states.next_initial());
                let con_final_index = con.states_insert(states.next_final());
                let mut prev_fa_final_indices = set![con.initial_index()];
                for re in res {
                    let fa = re.as_enfa(states.disable_final());
                    con.subsume(&fa);
                    let fa_initial_index = states_contains_from(&con, &fa, fa.initial_index()).expect("state does not exist");
                    for prev_fa_final_index in prev_fa_final_indices {
                        con.transitions_insert((prev_fa_final_index, Interval::empty(), fa_initial_index));
                    }
                    prev_fa_final_indices = states_contains_all_from(&con, &fa, fa.final_indices()).expect("not all states exist").collect();
                }
                for prev_fa_final_index in prev_fa_final_indices {
                    con.transitions_insert((prev_fa_final_index, Interval::empty(), con_final_index));
                }
                con.set_final(con_final_index);
                con
            },
            Expression::Repetition { re, min, max } => {
                let mut rep = Enfa::new(states.next_initial());
                let rep_final_index = rep.states_insert(states.next_final());
                let mut prev_fa_final_indices = set![rep.initial_index()];
                let mut count = 0;
                while if let Some(min) = min { count < *min } else { false } {
                    let fa = re.as_enfa(states.disable_final());
                    rep.subsume(&fa);
                    let fa_initial_index = states_contains_from(&rep, &fa, fa.initial_index()).expect("state does not exist");
                    for prev_fa_final_index in prev_fa_final_indices {
                        rep.transitions_insert((prev_fa_final_index, Interval::empty(), fa_initial_index));
                    }
                    prev_fa_final_indices = states_contains_all_from(&rep, &fa, fa.final_indices()).expect("not all states exist").collect();
                    count += 1;
                }
                if let Some(max) = max {
                    while count < *max {
                        let fa = re.as_enfa(states.disable_final());
                        rep.subsume(&fa);
                        let fa_initial_index = states_contains_from(&rep, &fa, fa.initial_index()).expect("state does not exist");
                        for prev_fa_final_index in prev_fa_final_indices {
                            rep.transitions_insert((prev_fa_final_index, Interval::empty(), fa_initial_index));
                            rep.transitions_insert((prev_fa_final_index, Interval::empty(), rep_final_index));
                        }
                        prev_fa_final_indices = states_contains_all_from(&rep, &fa, fa.final_indices()).expect("not all states exist").collect();
                        count += 1;
                    }
                    for prev_fa_final_index in prev_fa_final_indices {
                        rep.transitions_insert((prev_fa_final_index, Interval::empty(), rep_final_index));
                    }
                } else {
                    let fa = re.as_enfa(states.disable_final());
                    rep.subsume(&fa);
                    let fa_initial_index = states_contains_from(&rep, &fa, fa.initial_index()).expect("state does not exist");
                    for prev_fa_final_index in prev_fa_final_indices {
                        rep.transitions_insert((prev_fa_final_index, Interval::empty(), fa_initial_index));
                        rep.transitions_insert((prev_fa_final_index, Interval::empty(), rep_final_index));
                    }
                    for fa_final_index in fa.final_indices() {
                        let fa_final_index = states_contains_from(&rep, &fa, fa_final_index).expect("state does not exist");
                        rep.transitions_insert((fa_final_index, Interval::empty(), rep_final_index));
                        rep.transitions_insert((fa_final_index, Interval::empty(), fa_initial_index));
                    }
                }
                rep.set_final(rep_final_index);
                rep
            },
        }
    }
}

impl From<&Re> for Enfa<u32, u32> {
    fn from(re: &Re) -> Enfa<u32, u32> {
        re.as_enfa(&mut SimpleStateGenerator::new())
    }
}

impl From<&Re> for Nfa<Set<u32>, u32> {
    fn from(re: &Re) -> Nfa<Set<u32>, u32> {
        Nfa::from(&Enfa::from(re))
    }
}

impl From<&Re> for Dfa<Set<u32>, u32> {
    fn from(re: &Re) -> Dfa<Set<u32>, u32> {
        Dfa::from(&Enfa::from(re))
    }
}

#[macro_export]
macro_rules! sym {
    ($($x:expr),*) => {{
        #[allow(unused_mut)]
        let mut temp_vec = Vec::new();
        $(temp_vec.push($x);)*
        $crate::re::Re::new($crate::re::Expression::SymbolSet { intervals: temp_vec })
    }}
}

#[macro_export]
macro_rules! neg {
    ($($x:expr),*) => {{
        #[allow(unused_mut)]
        let mut temp_vec = Vec::new();
        $(temp_vec.push($x);)*
        $crate::re::Re::new($crate::re::Expression::NegatedSymbolSet { intervals: temp_vec })
    }}
}

#[macro_export]
macro_rules! alt {
    ($($x:expr),*) => {{
        #[allow(unused_mut)]
        let mut temp_vec = Vec::new();
        $(temp_vec.push($x);)*
        $crate::re::Re::new($crate::re::Expression::Alternation { res: temp_vec })
    }}
}

#[macro_export]
macro_rules! con {
    ($($x:expr),*) => {{
        #[allow(unused_mut)]
        let mut temp_vec = Vec::new();
        $(temp_vec.push($x);)*
        $crate::re::Re::new($crate::re::Expression::Concatenation { res: temp_vec })
    }}
}

#[macro_export]
macro_rules! rep {
    ($x:expr, $y:expr, $z:expr) => {{
        $crate::re::Re::new($crate::re::Expression::Repetition { re: Box::new($x), min: $y, max: $z })
    }}
}

#[macro_export]
macro_rules! ast { // asterisk
    ($x:expr) => {{
        $crate::re::Re::new($crate::re::Expression::Repetition { re: Box::new($x), min: None, max: None })
    }}
}

#[macro_export]
macro_rules! plu { // plus sign
    ($x:expr) => {{
        $crate::re::Re::new($crate::re::Expression::Repetition { re: Box::new($x), min: Some(1), max: None })
    }}
}

#[macro_export]
macro_rules! que { // question mark
    ($x:expr) => {{
        $crate::re::Re::new($crate::re::Expression::Repetition { re: Box::new($x), min: None, max: Some(1) })
    }}
}

#[macro_export]
macro_rules! sgl { // singleton
    ($x:expr) => {{
        interval_map::Interval::singleton(u32::from($x))
    }}
}

#[macro_export]
macro_rules! rng { // range
    ($x:expr, $y:expr) => {{
        interval_map::Interval::closed(u32::from($x), u32::from($y))
    }}
}

#[cfg(test)]
mod tests {
    use std::{
        collections::BTreeSet as Set,
        fmt::Debug,
        u32,
    };
    use interval_map::Interval;
    use finite_automata::{
        Enfa,
        Dfa,
    };

    #[test]
    fn test_enfa_epsilon() {
         let expected = ExpectedEnfa {
            initial: 0,
            transitions: set![
                (0, Interval::empty(), 1)
            ],
            finals: set![1]
        };
        // r"[]"
        let actual = Enfa::from(&sym![]);
        assert_enfa_eq(expected, actual);
    }

    #[test]
    fn test_enfa_symbol() {
        let expected = ExpectedEnfa {
            initial: 0,
            transitions: set![
                (0, Interval::singleton(u32::from('A')), 1)
            ],
            finals: set![1]
        };
        // r"A"
        let actual = Enfa::from(&sym![sgl!('A')]);
        assert_enfa_eq(expected, actual);
    }

    #[test]
    fn test_enfa_negated_symbol() {
        let expected = ExpectedEnfa {
            initial: 0,
            transitions: set![
                (0, Interval::less_than(u32::from('A')), 1),
                (0, Interval::greater_than(u32::from('A')), 1)
            ],
            finals: set![1]
        };
        // r"[^A]"
        let actual = Enfa::from(&neg![sgl!('A')]);
        assert_enfa_eq(expected, actual);
    }

    #[test]
    fn test_enfa_symbol_set() {
        let expected = ExpectedEnfa {
            initial: 0,
            transitions: set![
                (0, Interval::closed(u32::from('A'), u32::from('Z')), 1),
                (0, Interval::closed(u32::from('a'), u32::from('z')), 1)
            ],
            finals: set![1]
        };
        // r"[A-Za-z]"
        let actual = Enfa::from(&sym![rng!('A', 'Z'), rng!('a', 'z')]);
        assert_enfa_eq(expected, actual);
    }

    #[test]
    fn test_enfa_negated_symbol_set() {
        let expected = ExpectedEnfa {
            initial: 0,
            transitions: set![
                (0, Interval::less_than(u32::from('A')), 1),
                (0, Interval::open(u32::from('Z'), u32::from('a')), 1),
                (0, Interval::greater_than(u32::from('z')), 1)
            ],
            finals: set![1]
        };
        // r"[^A-Za-z]"
        let actual = Enfa::from(&neg![rng!('A', 'Z'), rng!('a', 'z')]);
        assert_enfa_eq(expected, actual);
    }

    #[test]
    fn test_enfa_alternation() {
        let expected = ExpectedEnfa {
            initial: 0,
            transitions: set![
                (0, Interval::empty(), 2),
                (0, Interval::empty(), 4),
                (2, Interval::singleton(u32::from('A')), 3),
                (3, Interval::empty(), 1),
                (4, Interval::singleton(u32::from('B')), 5),
                (5, Interval::empty(), 1)
            ],
            finals: set![1]
        };
        // r"A|B"
        let actual = Enfa::from(&alt![sym![sgl!('A')], sym![sgl!('B')]]);
        assert_enfa_eq(expected, actual);
    }

    #[test]
    fn test_enfa_concatenation() {
        let expected = ExpectedEnfa {
            initial: 0,
            transitions: set![
                (0, Interval::empty(), 2),
                (2, Interval::singleton(u32::from('A')), 3),
                (3, Interval::empty(), 4),
                (4, sgl!('B'), 5),
                (5, Interval::empty(), 1)
            ],
            finals: set![1]
        };
        // r"AB"
        let actual = Enfa::from(&con![sym![sgl!('A')], sym![sgl!('B')]]);
        assert_enfa_eq(expected, actual);
    }

    #[test]
    fn test_enfa_repetition() {
        let expected = ExpectedEnfa {
            initial: 0,
            transitions: set![
                (0, Interval::empty(), 2),
                (0, Interval::empty(), 1),
                (2, Interval::singleton(u32::from('A')), 3),
                (3, Interval::empty(), 1),
                (3, Interval::empty(), 2)
            ],
            finals: set![1]
        };
        // r"A*"
        let actual = Enfa::from(&ast!(sym![sgl!('A')]));
        assert_enfa_eq(expected, actual);
    }

    #[test]
    fn test_dfa_epsilon() {
        let expected = ExpectedDfa {
            initial: set![0, 1],
            transitions: set![],
            finals: set![set![0, 1]]
        };
        // r"[]"
        let actual = Dfa::from(&sym![]);
        assert_dfa_eq(expected, actual);
    }

    #[test]
    fn test_dfa_symbol() {
        let expected = ExpectedDfa {
            initial: set![0],
            transitions: set![
                (set![0], Interval::singleton(u32::from('A')), set![1])
            ],
            finals: set![set![1]]
        };
        // r"A"
        let actual = Dfa::from(&sym![sgl!('A')]);
        assert_dfa_eq(expected, actual);
    }

    #[test]
    fn test_dfa_negated_symbol() {
        let expected = ExpectedDfa {
            initial: set![0],
            transitions: set![
                (set![0], Interval::less_than(u32::from('A')), set![1]),
                (set![0], Interval::greater_than(u32::from('A')), set![1])
            ],
            finals: set![set![1]]
        };
        // r"[^A]"
        let actual = Dfa::from(&neg![sgl!('A')]);
        assert_dfa_eq(expected, actual);
    }

    #[test]
    fn test_dfa_symbol_set() {
        let expected = ExpectedDfa {
            initial: set![0],
            transitions: set![
                (set![0], Interval::closed(u32::from('A'), u32::from('Z')), set![1]),
                (set![0], Interval::closed(u32::from('a'), u32::from('z')), set![1])
            ],
            finals: set![set![1]]
        };
        // r"[A-Za-z]"
        let actual = Dfa::from(&sym![rng!('A', 'Z'), rng!('a', 'z')]);
        assert_dfa_eq(expected, actual);
    }

    #[test]
    fn test_dfa_negated_symbol_set() {
        let expected = ExpectedDfa {
            initial: set![0],
            transitions: set![
                (set![0], Interval::less_than(u32::from('A')), set![1]),
                (set![0], Interval::open(u32::from('Z'), u32::from('a')), set![1]),
                (set![0], Interval::greater_than(u32::from('z')), set![1])
            ],
            finals: set![set![1]]
        };
        // r"[^A-Za-z]"
        let actual = Dfa::from(&neg![rng!('A', 'Z'), rng!('a', 'z')]);
        assert_dfa_eq(expected, actual);
    }

    #[test]
    fn test_dfa_alternation() {
        let expected = ExpectedDfa {
            initial: set![0, 2, 4],
            transitions: set![
                (set![0, 2, 4], Interval::singleton(u32::from('A')), set![1, 3]),
                (set![0, 2, 4], Interval::singleton(u32::from('B')), set![1, 5])
            ],
            finals: set![set![1, 3], set![1, 5]]
        };
        // r"A|B"
        let actual = Dfa::from(&alt![sym![sgl!('A')], sym![sgl!('B')]]);
        assert_dfa_eq(expected, actual);
    }

    #[test]
    fn test_dfa_concatenation() {
        let expected = ExpectedDfa {
            initial: set![0, 2],
            transitions: set![
                (set![0, 2], Interval::singleton(u32::from('A')), set![3, 4]),
                (set![3, 4], Interval::singleton(u32::from('B')), set![1, 5])
            ],
            finals: set![set![1, 5]]
        };
        // r"AB"
        let actual = Dfa::from(&con![sym![sgl!('A')], sym![sgl!('B')]]);
        assert_dfa_eq(expected, actual);
    }

    #[test]
    fn test_dfa_repetition() {
        let expected = ExpectedDfa {
            initial: set![0, 1, 2],
            transitions: set![
                (set![0, 1, 2], Interval::singleton(u32::from('A')), set![1, 2, 3]),
                (set![1, 2, 3], Interval::singleton(u32::from('A')), set![1, 2, 3])
            ],
            finals: set![set![0, 1, 2], set![1, 2, 3]]
        };
        // r"A*"
        let actual = Dfa::from(&ast!(sym![sgl!('A')]));
        assert_dfa_eq(expected, actual);
    }

    #[test]
    fn test_match_epsilon_1() {
        // r"[]"
        let mut re = sym![];
        re.compile();
        assert!(re.is_match(""));
    }

    #[test]
    fn test_match_epsilon_2() {
        // r"[]"
        let mut re = sym![];
        re.compile();
        assert!(!re.is_match("A"));
    }

    #[test]
    fn test_match_symbol_1() {
        // r"A"
        let mut re = sym![sgl!('A')];
        re.compile();
        assert!(re.is_match("A"));
    }

    #[test]
    fn test_match_symbol_2() {
        // r"A"
        let mut re = sym![sgl!('A')];
        re.compile();
        assert!(!re.is_match("B"));
    }

    #[test]
    fn test_match_negated_symbol_1() {
        // r"[^A]"
        let mut re = neg![sgl!('A')];
        re.compile();
        assert!(re.is_match("B"));
    }

    #[test]
    fn test_match_negated_symbol_2() {
        // r"[^A]"
        let mut re = neg![sgl!('A')];
        re.compile();
        assert!(!re.is_match("A"));
    }

    #[test]
    fn test_match_symbol_set_1() {
        // r"[A-Za-z]"
        let mut re = sym![rng!('A', 'Z'), rng!('a', 'z')];
        re.compile();
        assert!(re.is_match("D"));
    }

    #[test]
    fn test_match_symbol_set_2() {
        // r"[A-Za-z]"
        let mut re = sym![rng!('A', 'Z'), rng!('a', 'z')];
        re.compile();
        assert!(!re.is_match("_"));
    }

    #[test]
    fn test_match_negated_symbol_set_1() {
        // r"[^A-Za-z]"
        let mut re = neg![rng!('A', 'Z'), rng!('a', 'z')];
        re.compile();
        assert!(re.is_match("_"));
    }

    #[test]
    fn test_match_negated_symbol_set_2() {
        // r"[^A-Za-z]"
        let mut re = neg![rng!('A', 'Z'), rng!('a', 'z')];
        re.compile();
        assert!(!re.is_match("D"));
    }

    #[test]
    fn test_match_alternation_1() {
        // r"A|B"
        let mut re = alt![sym![sgl!('A')], sym![sgl!('B')]];
        re.compile();
        assert!(re.is_match("A"));
    }

    #[test]
    fn test_match_alternation_2() {
        // r"A|B"
        let mut re = alt![sym![sgl!('A')], sym![sgl!('B')]];
        re.compile();
        assert!(re.is_match("B"));
    }

    #[test]
    fn test_match_alternation_3() {
        // r"A|B"
        let mut re = alt![sym![sgl!('A')], sym![sgl!('B')]];
        re.compile();
        assert!(!re.is_match("C"));
    }

    #[test]
    fn test_match_concatenation_1() {
        // r"AB"
        let mut re = con![sym![sgl!('A')], sym![sgl!('B')]];
        re.compile();
        assert!(re.is_match("AB"));
    }

    #[test]
    fn test_match_concatenation_2() {
        // r"AB"
        let mut re = con![sym![sgl!('A')], sym![sgl!('B')]];
        re.compile();
        assert!(!re.is_match("AA"));
    }

    #[test]
    fn test_match_repetition_1() {
        // r"A*"
        let mut re = ast!(sym![sgl!('A')]);
        re.compile();
        assert!(re.is_match("AAAAAAAAA"));
    }

    #[test]
    fn test_match_repetition_2() {
        // r"A*"
        let mut re = ast!(sym![sgl!('A')]);
        re.compile();
        assert!(!re.is_match("AAAABAAAA"));
    }

    #[test]
    fn test_match_repetition_3() {
        // r"A*"
        let mut re = ast!(sym![sgl!('A')]);
        re.compile();
        assert!(re.is_match(""));
    }

    #[test]
    fn test_match_repetition_4() {
        // r"A+"
        let mut re = plu!(sym![sgl!('A')]);
        re.compile();
        assert!(re.is_match("AAAAAAAAAA"));
    }

    #[test]
    fn test_match_repetition_5() {
        // r"A+"
        let mut re = plu!(sym![sgl!('A')]);
        re.compile();
        assert!(!re.is_match("AAAABAAAAA"));
    }

    #[test]
    fn test_match_repetition_6() {
        // r"A+"
        let mut re = plu!(sym![sgl!('A')]);
        re.compile();
        assert!(!re.is_match(""));
    }

    #[test]
    fn test_match_repetition_7() {
        // r"A?"
        let mut re = que!(sym![sgl!('A')]);
        re.compile();
        assert!(re.is_match("A"));
    }

    #[test]
    fn test_match_repetition_8() {
        // r"A?"
        let mut re = que!(sym![sgl!('A')]);
        re.compile();
        assert!(!re.is_match("B"));
    }

    #[test]
    fn test_match_repetition_9() {
        // r"A?"
        let mut re = que!(sym![sgl!('A')]);
        re.compile();
        assert!(re.is_match(""));
    }

    #[test]
    fn test_match_repetition_10() {
        // r"A{,3}"
        let mut re = rep!(sym![sgl!('A')], None, Some(3));
        re.compile();
        assert!(re.is_match(""));
    }

    #[test]
    fn test_match_repetition_11() {
        // r"A{,3}"
        let mut re = rep!(sym![sgl!('A')], None, Some(3));
        re.compile();
        assert!(re.is_match("AA"));
    }

    #[test]
    fn test_match_repetition_12() {
        // r"A{,3}"
        let mut re = rep!(sym![sgl!('A')], None, Some(3));
        re.compile();
        assert!(re.is_match("AAA"));
    }

    #[test]
    fn test_match_repetition_13() {
        // r"A{,3}"
        let mut re = rep!(sym![sgl!('A')], None, Some(3));
        re.compile();
        assert!(!re.is_match("AAAA"));
    }

    #[test]
    fn test_match_repetition_14() {
        // r"A{3,}"
        let mut re = rep!(sym![sgl!('A')], Some(3), None);
        re.compile();
        assert!(!re.is_match(""));
    }

    #[test]
    fn test_match_repetition_15() {
        // r"A{3,}"
        let mut re = rep!(sym![sgl!('A')], Some(3), None);
        re.compile();
        assert!(!re.is_match("AA"));
    }

    #[test]
    fn test_match_repetition_16() {
        // r"A{3,}"
        let mut re = rep!(sym![sgl!('A')], Some(3), None);
        re.compile();
        assert!(re.is_match("AAA"));
    }

    #[test]
    fn test_match_repetition_17() {
        // r"A{3,}"
        let mut re = rep!(sym![sgl!('A')], Some(3), None);
        re.compile();
        assert!(re.is_match("AAAA"));
    }

    #[test]
    fn test_match_repetition_18() {
        // r"A{3}"
        let mut re = rep!(sym![sgl!('A')], Some(3), Some(3));
        re.compile();
        assert!(!re.is_match(""));
    }

    #[test]
    fn test_match_repetition_19() {
        // r"A{3}"
        let mut re = rep!(sym![sgl!('A')], Some(3), Some(3));
        re.compile();
        assert!(!re.is_match("AA"));
    }

    #[test]
    fn test_match_repetition_20() {
        // r"A{3}"
        let mut re = rep!(sym![sgl!('A')], Some(3), Some(3));
        re.compile();
        assert!(re.is_match("AAA"));
    }

    #[test]
    fn test_match_repetition_21() {
        // r"A{3}"
        let mut re = rep!(sym![sgl!('A')], Some(3), Some(3));
        re.compile();
        assert!(!re.is_match("AAAA"));
    }

    #[test]
    fn test_match_repetition_22() {
        // r"A{2,3}"
        let mut re = rep!(sym![sgl!('A')], Some(2), Some(3));
        re.compile();
        assert!(!re.is_match(""));
    }

    #[test]
    fn test_match_repetition_23() {
        // r"A{2,3}"
        let mut re = rep!(sym![sgl!('A')], Some(2), Some(3));
        re.compile();
        assert!(re.is_match("AA"));
    }

    #[test]
    fn test_match_repetition_24() {
        // r"A{2,3}"
        let mut re = rep!(sym![sgl!('A')], Some(2), Some(3));
        re.compile();
        assert!(re.is_match("AAA"));
    }

    #[test]
    fn test_match_repetition_25() {
        // r"A{2,3}"
        let mut re = rep!(sym![sgl!('A')], Some(2), Some(3));
        re.compile();
        assert!(!re.is_match("AAAA"));
    }

    #[test]
    fn test_match_complex_1() {
        // r"(A|B)*(AAA|BBB)(A|B)*"
        let mut re = con![ast![alt![sym![sgl!('A')], sym![sgl!('B')]]], alt![con![sym![sgl!('A')], sym![sgl!('A')], sym![sgl!('A')]], con![sym![sgl!('B')], sym![sgl!('B')], sym![sgl!('B')]]], ast![alt![sym![sgl!('A')], sym![sgl!('B')]]]];
        re.compile();
        assert!(re.is_match("ABBBA"));
    }

    #[test]
    fn test_match_complex_2() {
        // r"(A|B)*(AAA|BBB)(A|B)*"
        let mut re = con![ast![alt![sym![sgl!('A')], sym![sgl!('B')]]], alt![con![sym![sgl!('A')], sym![sgl!('A')], sym![sgl!('A')]], con![sym![sgl!('B')], sym![sgl!('B')], sym![sgl!('B')]]], ast![alt![sym![sgl!('A')], sym![sgl!('B')]]]];
        re.compile();
        assert!(!re.is_match("ABBA"));
    }

    #[test]
    fn test_match_complex_3() {
        // r"(A|B)*(AAA|BBB)(A|B)*"
        let mut re = con![ast![alt![sym![sgl!('A')], sym![sgl!('B')]]], alt![con![sym![sgl!('A')], sym![sgl!('A')], sym![sgl!('A')]], con![sym![sgl!('B')], sym![sgl!('B')], sym![sgl!('B')]]], ast![alt![sym![sgl!('A')], sym![sgl!('B')]]]];
        re.compile();
        assert!(re.is_match("ABBAAA"));
    }

    struct ExpectedEnfa<S, T> {
        initial: S,
        transitions: Set<(S, Interval<T>, S)>,
        finals: Set<S>,
    }

    fn assert_enfa_eq<S: Clone + Debug + Ord, T: Clone + Debug + Ord>(expected: ExpectedEnfa<S, T>, actual: Enfa<S, T>) {
        assert_eq!(expected.initial, actual.states_index(actual.initial_index()).clone());
        assert_eq!(expected.transitions, actual.transitions_slice(actual.transition_indices()).map(|(source, transition, target)| (actual.states_index(source).clone(), transition.clone(), actual.states_index(target).clone())).collect());
        assert_eq!(expected.finals, actual.states_slice(actual.final_indices()).cloned().collect());
    }

    struct ExpectedDfa<S, T> {
        initial: S,
        transitions: Set<(S, Interval<T>, S)>,
        finals: Set<S>,
    }

    fn assert_dfa_eq<S: Clone + Debug + Ord, T: Clone + Debug + Ord>(expected: ExpectedDfa<Set<S>, T>, actual: Dfa<Set<S>, T>) {
        assert_eq!(expected.initial, actual.states_index(actual.initial_index()).clone());
        assert_eq!(expected.transitions, actual.transitions_slice(actual.transition_indices()).map(|(source, transition, target)| (actual.states_index(source).clone(), transition.clone(), actual.states_index(target).clone())).collect());
        assert_eq!(expected.finals, actual.states_slice(actual.final_indices()).cloned().collect());
    }
}
