use std::collections::BTreeSet as Set;

use finite_automata::{DFA, NFA, ENFA, Insert, Subsume, Contains, At, ContainsFrom, ContainsAllFrom};

use crate::{StateGenerator, SimpleStateGenerator};

#[derive(Clone, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum RE {
    Epsilon,
    Symbol { symbol: char },
    Alternation { res: Vec<RE> },
    Concatenation { res: Vec<RE> },
    Repetition { re: Box<RE> },
}

impl RE {
    pub fn into_enfa<S: Clone + Ord, G: StateGenerator<State = S>>(&self, states: &mut G) -> ENFA<S, char> {
        match self {
            RE::Epsilon => {
                let mut eps = ENFA::new(states.next_initial());
                let eps_final_index = eps.insert(states.next_final());
                eps.set_final(eps_final_index);
                eps.insert((eps.initial_index(), None, eps_final_index));
                eps
            },
            RE::Symbol { symbol } => {
                let mut sym = ENFA::new(states.next_initial());
                let sym_final_index = sym.insert(states.next_final());
                sym.set_final(sym_final_index);
                sym.insert((sym.initial_index(), Some(symbol.clone()), sym_final_index));
                sym
            },
            RE::Alternation { res } => {
                let mut alt = ENFA::new(states.next_initial());
                let alt_final_index = alt.insert(states.next_final());
                alt.set_final(alt_final_index);
                for re in res {
                    let re = re.into_enfa(states.disable_final());
                    alt.subsume(&re);
                    let re_initial_index = alt.contains_from(&re, re.initial_index()).expect("state does not exist");
                    alt.insert((alt.initial_index(), None, re_initial_index));
                    for re_final_index in re.final_indices() {
                        let re_final_index = alt.contains_from(&re, re_final_index).expect("state does not exist");
                        alt.insert((re_final_index, None, alt_final_index));
                    }
                }
                alt
            },
            RE::Concatenation { res } => {
                let mut cat = ENFA::new(states.next_initial());
                let cat_final_index = cat.insert(states.next_final());
                cat.set_final(cat_final_index);
                let mut prev_re_final_indices = set![cat.initial_index()];
                for re in res {
                    let re = re.into_enfa(states.disable_final());
                    cat.subsume(&re);
                    let re_initial_index = cat.contains_from(&re, re.initial_index()).expect("state does not exist");
                    for prev_re_final_index in prev_re_final_indices {
                        cat.insert((prev_re_final_index, None, re_initial_index));
                    }
                    prev_re_final_indices = cat.contains_all_from(&re, re.final_indices()).expect("not all states exist").collect();
                }
                for prev_re_final_index in prev_re_final_indices {
                    cat.insert((prev_re_final_index, None, cat_final_index));
                }
                cat
            },
            RE::Repetition { re } => {
                let mut rep = ENFA::new(states.next_initial());
                let rep_final_index = rep.insert(states.next_final());
                rep.set_final(rep_final_index);
                let re = re.into_enfa(states.disable_final());
                rep.subsume(&re);
                let re_initial_index = rep.contains_from(&re, re.initial_index()).expect("state does not exist");
                rep.insert((rep.initial_index(), None, re_initial_index));
                for re_final_index in re.final_indices() {
                    let re_final_index = rep.contains_from(&re, re_final_index).expect("state does not exist");
                    rep.insert((re_final_index, None, rep_final_index));
                    rep.insert((re_final_index, None, re_initial_index));
                }
                rep.insert((rep.initial_index(), None, rep_final_index));
                rep
            },
        }
    }

    pub fn is_match(&self, text: &str) -> bool {
        let dfa: DFA<Set<u32>, char> = DFA::from(self.into_enfa(&mut SimpleStateGenerator::new())); // TODO: compilation should be pulled out
        let mut source_index = dfa.initial_index();
        for character in text.chars() {
            if let Some(transition_index) = dfa.contains(&(source_index, &character)) {
                let (_, _, target_index) = dfa.at(transition_index);
                source_index = target_index;
            } else {
                return false;
            }
        }
        dfa.is_final(source_index)
    }
}

impl From<RE> for ENFA<u32, char> {
    fn from(re: RE) -> ENFA<u32, char> {
        re.into_enfa(&mut SimpleStateGenerator::new())
    }
}

impl From<RE> for NFA<Set<u32>, char> {
    fn from(re: RE) -> NFA<Set<u32>, char> {
        NFA::from(ENFA::from(re))
    }
}

impl From<RE> for DFA<Set<u32>, char> {
    fn from(re: RE) -> DFA<Set<u32>, char> {
        DFA::from(ENFA::from(re))
    }
}

#[macro_export]
macro_rules! eps {
    () => {{
        $crate::re::RE::Epsilon
    }}
}

#[macro_export]
macro_rules! sym {
    ($x:expr) => {{
        $crate::re::RE::Symbol { symbol: $x }
    }}
}

#[macro_export]
macro_rules! alt {
    ($($x:expr),*) => {{
        let mut temp_vec = Vec::new();
        $(temp_vec.push($x);)*
        $crate::re::RE::Alternation { res: temp_vec }
    }}
}

#[macro_export]
macro_rules! cat {
    ($($x:expr),*) => {{
        let mut temp_vec = Vec::new();
        $(temp_vec.push($x);)*
        $crate::re::RE::Concatenation { res: temp_vec }
    }}
}

#[macro_export]
macro_rules! rep {
    ($x:expr) => {{
        $crate::re::RE::Repetition { re: Box::new($x) }
    }}
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet as Set;
    use std::fmt::Debug;

    use finite_automata::{At, Slice};
    use finite_automata::enfa::ENFA;
    use finite_automata::dfa::DFA;

    struct ExpectedENFA<S, T> {
        initial: S,
        transitions: Set<(S, Option<T>, S)>,
        finals: Set<S>,
    }

    fn assert_enfa_eq<S: Clone + Debug + Ord, T: Clone + Debug + Ord>(expected: ExpectedENFA<S, T>, actual: ENFA<S, T>) {
        assert_eq!(expected.initial, actual.at(actual.initial_index()).clone());
        assert_eq!(expected.transitions, actual.slice(actual.transition_indices()).map(|(source, transition, target)| (actual.at(source).clone(), transition.clone(), actual.at(target).clone())).collect());
        assert_eq!(expected.finals, actual.final_indices().map(|final_index| actual.at(final_index).clone()).collect());
    }

    struct ExpectedDFA<S, T> {
        initial: S,
        transitions: Set<(S, T, S)>,
        finals: Set<S>,
    }

    fn assert_dfa_eq<S: Clone + Debug + Ord, T: Clone + Debug + Ord>(expected: ExpectedDFA<Set<S>, T>, actual: DFA<Set<S>, T>) {
        assert_eq!(expected.initial, actual.at(actual.initial_index()).clone());
        assert_eq!(expected.transitions, actual.slice(actual.transition_indices()).map(|(source, transition, target)| (actual.at(source).clone(), transition.clone(), actual.at(target).clone())).collect());
        assert_eq!(expected.finals, actual.final_indices().map(|final_index| actual.at(final_index).clone()).collect());
    }

    #[test]
    fn test_1() {
         let expected = ExpectedENFA {
            initial: 0,
            transitions: set![
                (0, None,      1)
            ],
            finals: set![1]
        };
        // r""
        let actual = ENFA::from(eps!());
        assert_enfa_eq(expected, actual);
    }

     #[test]
    fn test_2() {
        let expected = ExpectedENFA {
            initial: 0,
            transitions: set![
                (0, Some('A'), 1)
            ],
            finals: set![1]
        };
        // r"A"
        let actual = ENFA::from(sym!('A'));
        assert_enfa_eq(expected, actual);
    }

    #[test]
    fn test_3() {
        let expected = ExpectedENFA {
            initial: 0,
            transitions: set![
                (0, None,      2),
                (0, None,      4),
                (2, None,      3),
                (4, Some('A'), 5),
                (3, None,      1),
                (5, None,      1)
            ],
            finals: set![1]
        };
        // r"|A"
        let actual = ENFA::from(alt![eps!(), sym!('A')]);
        assert_enfa_eq(expected, actual);
    }

    #[test]
    fn test_4() {
        let expected = ExpectedENFA {
            initial: 0,
            transitions: set![
                (0, None,      2),
                (2, Some('A'), 3),
                (3, None,      4),
                (4, None,      5),
                (5, None,      1)
            ],
            finals: set![1]
        };
        // r"A"
        let actual = ENFA::from(cat![sym!('A'), eps!()]);
        assert_enfa_eq(expected, actual);
    }

    #[test]
    fn test_5() {
        let expected = ExpectedENFA {
            initial: 0,
            transitions: set![
                (0, None,      1),
                (0, None,      2),
                (2, Some('A'), 3),
                (3, None,      2),
                (3, None,      1)
            ],
            finals: set![1]
        };
        // r"A*"
        let actual = ENFA::from(rep!(sym!('A')));
        assert_enfa_eq(expected, actual);
    }

    #[test]
    fn test_6() {
        let expected = ExpectedDFA::<_, char> {
            initial: set![0, 1],
            transitions: set![],
            finals: set![set![0, 1]]
        };
        // r""
        let actual = DFA::from(eps!());
        assert_dfa_eq(expected, actual);
    }

    #[test]
    fn test_7() {
        let expected = ExpectedDFA {
            initial: set![0],
            transitions: set![
                (set![0], 'A', set![1])
            ],
            finals: set![set![1]]
        };
        // r"A"
        let actual = DFA::from(sym!('A'));
        assert_dfa_eq(expected, actual);
    }

    #[test]
    fn test_8() {
        let expected = ExpectedDFA {
            initial: set![0, 1, 2, 3, 4],
            transitions: set![
                (set![0, 1, 2, 3, 4], 'A', set![1, 5])
            ],
            finals: set![set![0, 1, 2, 3, 4], set![1, 5]]
        };
        // r"|A"
        let actual = DFA::from(alt![eps!(), sym!('A')]);
        assert_dfa_eq(expected, actual);
    }

    #[test]
    fn test_9() {
        let expected = ExpectedDFA {
            initial: set![0, 2],
            transitions: set![
                (set![0, 2], 'A', set![1, 3, 4, 5])
            ],
            finals: set![set![1, 3, 4, 5]]
        };
        // r"A"
        let actual = DFA::from(cat![sym!('A'), eps!()]);
        assert_dfa_eq(expected, actual);
    }

    #[test]
    fn test_10() {
        let expected = ExpectedDFA {
            initial: set![0, 1, 2],
            transitions: set![
                (set![0, 1, 2], 'A', set![1, 2, 3]),
                (set![1, 2, 3], 'A', set![1, 2, 3])
            ],
            finals: set![set![0, 1, 2], set![1, 2, 3]]
        };
        // r"A*"
        let actual = DFA::from(rep!(sym!('A')));
        assert_dfa_eq(expected, actual);
    }

    #[test]
    fn test_11() {
        let expected = true;
        // r""
        let actual = eps!().is_match("");
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_12() {
        let expected = false;
        // r""
        let actual = eps!().is_match("A");
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_13() {
        let expected = true;
        // r"A"
        let actual = sym!('A').is_match("A");
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_14() {
        let expected = false;
        // r"A"
        let actual = sym!('A').is_match("B");
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_15() {
        let expected = true;
        // r"A|B"
        let actual = alt![sym!('A'), sym!('B')].is_match("A");
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_16() {
        let expected = true;
        // r"A|B"
        let actual = alt![sym!('A'), sym!('B')].is_match("B");
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_17() {
        let expected = false;
        // r"A|B"
        let actual = alt![sym!('A'), sym!('B')].is_match("C");
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_18() {
        let expected = true;
        // r"AB"
        let actual = cat![sym!('A'), sym!('B')].is_match("AB");
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_19() {
        let expected = false;
        // r"AB"
        let actual = cat![sym!('A'), sym!('B')].is_match("AA");
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_20() {
        let expected = true;
        // r"A*"
        let actual = rep!(sym!('A')).is_match("AAAAAAAAA");
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_21() {
        let expected = false;
        // r"A*"
        let actual = rep!(sym!('A')).is_match("AAAABAAAA");
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_22() {
        let expected = true;
        // r"(0|1)*(000|111)(0|1)*"
        let actual = cat![rep![alt![sym!('0'), sym!('1')]], alt![cat![sym!('0'), sym!('0'), sym!('0')], cat![sym!('1'), sym!('1'), sym!('1')]], rep![alt![sym!('0'), sym!('1')]]].is_match("01110");
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_23() {
        let expected = false;
        // r"(0|1)*(000|111)(0|1)*"
        let actual = cat![rep![alt![sym!('0'), sym!('1')]], alt![cat![sym!('0'), sym!('0'), sym!('0')], cat![sym!('1'), sym!('1'), sym!('1')]], rep![alt![sym!('0'), sym!('1')]]].is_match("0110");
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_24() {
        let expected = true;
        // r"(0|1)*(000|111)(0|1)*"
        let actual = cat![rep![alt![sym!('0'), sym!('1')]], alt![cat![sym!('0'), sym!('0'), sym!('0')], cat![sym!('1'), sym!('1'), sym!('1')]], rep![alt![sym!('0'), sym!('1')]]].is_match("011000");
        assert_eq!(expected, actual);
    }
}
