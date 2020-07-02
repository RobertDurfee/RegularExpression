use std::ops::Range;
use std::u32;

use finite_automata::{ENFA, Insert, Subsume, ContainsFrom, ContainsAllFrom};

pub enum RE {
    Epsilon,
    Symbol { symbol: char },
    Alternation { res: Vec<RE> },
    Concatenation { res: Vec<RE> },
    Repetition { re: Box<RE> },
}

impl RE {
    fn into_enfa(self, ids: &mut Range<u32>) -> ENFA<u32, char> {
        match self {
            RE::Epsilon => {
                let mut eps = ENFA::new(ids.next().expect("no more ids"));
                let eps_final_index = eps.insert(ids.next().expect("no more ids"));
                eps.set_final(eps_final_index);
                eps.insert((eps.initial_index(), None, eps_final_index));
                eps
            },
            RE::Symbol { symbol } => {
                let mut sym = ENFA::new(ids.next().expect("no more ids"));
                let sym_final_index = sym.insert(ids.next().expect("no more ids"));
                sym.set_final(sym_final_index);
                sym.insert((sym.initial_index(), Some(symbol), sym_final_index));
                sym
            },
            RE::Alternation { res } => {
                let mut alt: ENFA<u32, char> = ENFA::new(ids.next().expect("no more ids"));
                let alt_final_index = alt.insert(ids.next().expect("no more ids"));
                alt.set_final(alt_final_index);
                for re in res {
                    let re = re.into_enfa(ids);
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
                let mut cat = ENFA::new(ids.next().expect("no more ids"));
                let cat_final_index = cat.insert(ids.next().expect("no more ids"));
                cat.set_final(cat_final_index);
                let mut prev_re_final_indices = set![cat.initial_index()];
                for re in res {
                    let re = re.into_enfa(ids);
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
                let mut rep = ENFA::new(ids.next().expect("no more ids"));
                let rep_final_index = rep.insert(ids.next().expect("no more ids"));
                rep.set_final(rep_final_index);
                let re = re.into_enfa(ids);
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
}

impl From<RE> for ENFA<u32, char> {
    fn from(re: RE) -> ENFA<u32, char> {
        re.into_enfa(&mut (0..u32::MAX))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet as Set;
    use std::fmt::Debug;

    use finite_automata::{At, Slice};
    use finite_automata::enfa::ENFA;
    use finite_automata::dfa::DFA;

    use crate::RE;

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
        let actual = ENFA::from(RE::Epsilon);
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
        let actual = ENFA::from(RE::Symbol { symbol: 'A' });
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
        let actual = ENFA::from(RE::Alternation {
            res: vec![
                RE::Epsilon,
                RE::Symbol { symbol: 'A' }
            ]
        });
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
        let actual = ENFA::from(RE::Concatenation {
            res: vec![
                RE::Symbol { symbol: 'A' },
                RE::Epsilon
            ]
        });
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
        let actual = ENFA::from(RE::Repetition {
            re: Box::new(RE::Symbol { symbol: 'A' })
        });
        assert_enfa_eq(expected, actual);
    }

    #[test]
    fn test_6() {
        let expected = ExpectedDFA::<_, char> {
            initial: set![0, 1],
            transitions: set![],
            finals: set![set![0, 1]]
        };
        let actual = DFA::from(ENFA::from(RE::Epsilon));
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
        let actual = DFA::from(ENFA::from(RE::Symbol { symbol: 'A' }));
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
        let actual = DFA::from(ENFA::from(RE::Alternation {
            res: vec![
                RE::Epsilon,
                RE::Symbol { symbol: 'A' }
            ]
        }));
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
        let actual = DFA::from(ENFA::from(RE::Concatenation {
            res: vec![
                RE::Symbol { symbol: 'A' },
                RE::Epsilon
            ]
        }));
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
        let actual = DFA::from(ENFA::from(RE::Repetition {
            re: Box::new(RE::Symbol { symbol: 'A' })
        }));
        assert_dfa_eq(expected, actual);
    }
}
