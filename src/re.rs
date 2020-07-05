use std::collections::BTreeSet as Set;

use finite_automata::{
    Etr,
    Tr,
    Subsume,
    states_contains_from,
    states_contains_all_from,
    Enfa,
    Nfa,
    Dfa,
};

use crate::{
    StateGenerator,
    SimpleStateGenerator
};

#[derive(Clone, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum RE {
    Epsilon,
    Symbol { symbol: char },
    Alternation { res: Vec<RE> },
    Concatenation { res: Vec<RE> },
    Repetition { re: Box<RE> },
}

impl RE {
    pub fn into_enfa<S: Clone + Ord, G: StateGenerator<State = S>>(&self, states: &mut G) -> Enfa<S, char> {
        match self {
            RE::Epsilon => {
                let mut eps = Enfa::new(states.next_initial());
                let eps_final_index = eps.states_insert(states.next_final());
                eps.set_final(eps_final_index);
                eps.transitions_insert((eps.initial_index(), Etr::None, eps_final_index));
                eps
            },
            RE::Symbol { symbol } => {
                let mut sym = Enfa::new(states.next_initial());
                let sym_final_index = sym.states_insert(states.next_final());
                sym.set_final(sym_final_index);
                sym.transitions_insert((sym.initial_index(), Etr::Some(symbol.clone()), sym_final_index));
                sym
            },
            RE::Alternation { res } => {
                let mut alt = Enfa::new(states.next_initial());
                let alt_final_index = alt.states_insert(states.next_final());
                alt.set_final(alt_final_index);
                for re in res {
                    let re = re.into_enfa(states.disable_final());
                    alt.subsume(&re);
                    let re_initial_index = states_contains_from(&alt, &re, re.initial_index()).expect("state does not exist");
                    alt.transitions_insert((alt.initial_index(), Etr::None, re_initial_index));
                    for re_final_index in re.final_indices() {
                        let re_final_index = states_contains_from(&alt, &re, re_final_index).expect("state does not exist");
                        alt.transitions_insert((re_final_index, Etr::None, alt_final_index));
                    }
                }
                alt
            },
            RE::Concatenation { res } => {
                let mut cat = Enfa::new(states.next_initial());
                let cat_final_index = cat.states_insert(states.next_final());
                cat.set_final(cat_final_index);
                let mut prev_re_final_indices = set![cat.initial_index()];
                for re in res {
                    let re = re.into_enfa(states.disable_final());
                    cat.subsume(&re);
                    let re_initial_index = states_contains_from(&cat, &re, re.initial_index()).expect("state does not exist");
                    for prev_re_final_index in prev_re_final_indices {
                        cat.transitions_insert((prev_re_final_index, Etr::None, re_initial_index));
                    }
                    prev_re_final_indices = states_contains_all_from(&cat, &re, re.final_indices()).expect("not all states exist").collect();
                }
                for prev_re_final_index in prev_re_final_indices {
                    cat.transitions_insert((prev_re_final_index, Etr::None, cat_final_index));
                }
                cat
            },
            RE::Repetition { re } => {
                let mut rep = Enfa::new(states.next_initial());
                let rep_final_index = rep.states_insert(states.next_final());
                rep.set_final(rep_final_index);
                let re = re.into_enfa(states.disable_final());
                rep.subsume(&re);
                let re_initial_index = states_contains_from(&rep, &re, re.initial_index()).expect("state does not exist");
                rep.transitions_insert((rep.initial_index(), Etr::None, re_initial_index));
                for re_final_index in re.final_indices() {
                    let re_final_index = states_contains_from(&rep, &re, re_final_index).expect("state does not exist");
                    rep.transitions_insert((re_final_index, Etr::None, rep_final_index));
                    rep.transitions_insert((re_final_index, Etr::None, re_initial_index));
                }
                rep.transitions_insert((rep.initial_index(), Etr::None, rep_final_index));
                rep
            },
        }
    }

    pub fn is_match(&self, text: &str) -> bool {
        let dfa: Dfa<Set<u32>, char> = Dfa::from(&self.into_enfa(&mut SimpleStateGenerator::new())); // TODO: compilation should be pulled out
        let mut source_index = dfa.initial_index();
        for character in text.chars() {
            if let Some(transition_index) = dfa.transitions_contains_outgoing((source_index, &Tr::Some(character))) {
                let (_, _, target_index) = dfa.transitions_index(transition_index);
                source_index = target_index;
            } else {
                return false;
            }
        }
        dfa.is_final(source_index)
    }
}

impl From<&RE> for Enfa<u32, char> {
    fn from(re: &RE) -> Enfa<u32, char> {
        re.into_enfa(&mut SimpleStateGenerator::new())
    }
}

impl From<&RE> for Nfa<Set<u32>, char> {
    fn from(re: &RE) -> Nfa<Set<u32>, char> {
        Nfa::from(&Enfa::from(re))
    }
}

impl From<&RE> for Dfa<Set<u32>, char> {
    fn from(re: &RE) -> Dfa<Set<u32>, char> {
        Dfa::from(&Enfa::from(re))
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
    use std::{
        collections::BTreeSet as Set,
        fmt::Debug,
    };

    use finite_automata::{
        Etr,
        Tr,
        Enfa,
        Dfa,
    };

    struct ExpectedEnfa<S, T> {
        initial: S,
        transitions: Set<(S, Etr<T>, S)>,
        finals: Set<S>,
    }

    fn assert_enfa_eq<S: Clone + Debug + Ord, T: Clone + Debug + Ord>(expected: ExpectedEnfa<S, T>, actual: Enfa<S, T>) {
        assert_eq!(expected.initial, actual.states_index(actual.initial_index()).clone());
        assert_eq!(expected.transitions, actual.transitions_slice(actual.transition_indices()).map(|(source, transition, target)| (actual.states_index(source).clone(), transition.clone(), actual.states_index(target).clone())).collect());
        assert_eq!(expected.finals, actual.states_slice(actual.final_indices()).cloned().collect());
    }

    struct ExpectedDfa<S, T> {
        initial: S,
        transitions: Set<(S, Tr<T>, S)>,
        finals: Set<S>,
    }

    fn assert_dfa_eq<S: Clone + Debug + Ord, T: Clone + Debug + Ord>(expected: ExpectedDfa<Set<S>, T>, actual: Dfa<Set<S>, T>) {
        assert_eq!(expected.initial, actual.states_index(actual.initial_index()).clone());
        assert_eq!(expected.transitions, actual.transitions_slice(actual.transition_indices()).map(|(source, transition, target)| (actual.states_index(source).clone(), transition.clone(), actual.states_index(target).clone())).collect());
        assert_eq!(expected.finals, actual.states_slice(actual.final_indices()).cloned().collect());
    }

    #[test]
    fn test_1() {
         let expected = ExpectedEnfa {
            initial: 0,
            transitions: set![
                (0, Etr::None, 1)
            ],
            finals: set![1]
        };
        // r""
        let actual = Enfa::from(&eps!());
        assert_enfa_eq(expected, actual);
    }

     #[test]
    fn test_2() {
        let expected = ExpectedEnfa {
            initial: 0,
            transitions: set![
                (0, Etr::Some('A'), 1)
            ],
            finals: set![1]
        };
        // r"A"
        let actual = Enfa::from(&sym!('A'));
        assert_enfa_eq(expected, actual);
    }

    #[test]
    fn test_3() {
        let expected = ExpectedEnfa {
            initial: 0,
            transitions: set![
                (0, Etr::None, 2),
                (0, Etr::None, 4),
                (2, Etr::None, 3),
                (4, Etr::Some('A'), 5),
                (3, Etr::None, 1),
                (5, Etr::None, 1)
            ],
            finals: set![1]
        };
        // r"|A"
        let actual = Enfa::from(&alt![eps!(), sym!('A')]);
        assert_enfa_eq(expected, actual);
    }

    #[test]
    fn test_4() {
        let expected = ExpectedEnfa {
            initial: 0,
            transitions: set![
                (0, Etr::None, 2),
                (2, Etr::Some('A'), 3),
                (3, Etr::None, 4),
                (4, Etr::None, 5),
                (5, Etr::None, 1)
            ],
            finals: set![1]
        };
        // r"A"
        let actual = Enfa::from(&cat![sym!('A'), eps!()]);
        assert_enfa_eq(expected, actual);
    }

    #[test]
    fn test_5() {
        let expected = ExpectedEnfa {
            initial: 0,
            transitions: set![
                (0, Etr::None, 1),
                (0, Etr::None, 2),
                (2, Etr::Some('A'), 3),
                (3, Etr::None, 2),
                (3, Etr::None, 1)
            ],
            finals: set![1]
        };
        // r"A*"
        let actual = Enfa::from(&rep!(sym!('A')));
        assert_enfa_eq(expected, actual);
    }

    #[test]
    fn test_6() {
        let expected = ExpectedDfa::<_, char> {
            initial: set![0, 1],
            transitions: set![],
            finals: set![set![0, 1]]
        };
        // r""
        let actual = Dfa::from(&eps!());
        assert_dfa_eq(expected, actual);
    }

    #[test]
    fn test_7() {
        let expected = ExpectedDfa {
            initial: set![0],
            transitions: set![
                (set![0], Tr::Some('A'), set![1])
            ],
            finals: set![set![1]]
        };
        // r"A"
        let actual = Dfa::from(&sym!('A'));
        assert_dfa_eq(expected, actual);
    }

    #[test]
    fn test_8() {
        let expected = ExpectedDfa {
            initial: set![0, 1, 2, 3, 4],
            transitions: set![
                (set![0, 1, 2, 3, 4], Tr::Some('A'), set![1, 5])
            ],
            finals: set![set![0, 1, 2, 3, 4], set![1, 5]]
        };
        // r"|A"
        let actual = Dfa::from(&alt![eps!(), sym!('A')]);
        assert_dfa_eq(expected, actual);
    }

    #[test]
    fn test_9() {
        let expected = ExpectedDfa {
            initial: set![0, 2],
            transitions: set![
                (set![0, 2], Tr::Some('A'), set![1, 3, 4, 5])
            ],
            finals: set![set![1, 3, 4, 5]]
        };
        // r"A"
        let actual = Dfa::from(&cat![sym!('A'), eps!()]);
        assert_dfa_eq(expected, actual);
    }

    #[test]
    fn test_10() {
        let expected = ExpectedDfa {
            initial: set![0, 1, 2],
            transitions: set![
                (set![0, 1, 2], Tr::Some('A'), set![1, 2, 3]),
                (set![1, 2, 3], Tr::Some('A'), set![1, 2, 3])
            ],
            finals: set![set![0, 1, 2], set![1, 2, 3]]
        };
        // r"A*"
        let actual = Dfa::from(&rep!(sym!('A')));
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
