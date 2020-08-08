use std::collections::BTreeSet as Set;
use finite_automata::{
    Enfa,
    Nfa,
    Dfa,
};
use re_bootstrap::{
    Re as ReBootstrap,
    Expression,
};
use crate::grammar::{
    LEXER_PRODUCTIONS,
    PARSER_PRODUCTIONS,
    Nonterminal,
    as_expression,
};
use lexer_bootstrap::Lexer;
use parser_bootstrap::Parser;

pub struct Re {
    re: ReBootstrap,
}

impl Re {
    pub fn new(expression: &str) -> Re {
        let mut lexer = Lexer::new(LEXER_PRODUCTIONS.clone()); lexer.compile();
        let parser = Parser::new(PARSER_PRODUCTIONS.clone(), Nonterminal::Root);
        let tokens = lexer.lex(expression).expect("lexer error");
        let parse_tree = parser.parse(&tokens).expect("parser error");
        let expression = as_expression(&parse_tree);
        Re { re: ReBootstrap::new(expression) }
    }

    pub fn compile(&mut self) {
        self.re.compile()
    }

    pub fn is_match(&self, text: &str) -> bool {
        self.re.is_match(text)
    }

    pub fn into_expression(self) -> Expression {
        self.re.into_expression()
    }

    pub fn as_expression(&self) -> &Expression {
        self.re.as_expression()
    }
}

impl From<Re> for Expression {
    fn from(re: Re) -> Expression {
        re.into_expression()
    }
}

impl From<&Re> for Enfa<u32, u32> {
    fn from(re: &Re) -> Enfa<u32, u32> {
        Enfa::from(re.as_expression())
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
    use re_bootstrap::set;
    use crate::Re;

    #[test]
    fn test_enfa_epsilon() {
         let expected = ExpectedEnfa::<_, u32> {
            initial: 0,
            transitions: set![
                (0, Interval::empty(), 1)
            ],
            finals: set![1]
        };
        let actual = Enfa::from(&Re::new(r"[]"));
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
        let actual = Enfa::from(&Re::new(r"A"));
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
        let actual = Enfa::from(&Re::new(r"[^A]"));
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
        let actual = Enfa::from(&Re::new(r"[A-Za-z]"));
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
        let actual = Enfa::from(&Re::new(r"[^A-Za-z]"));
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
        let actual = Enfa::from(&Re::new("A|B"));
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
                (4, Interval::singleton(u32::from('B')), 5),
                (5, Interval::empty(), 1)
            ],
            finals: set![1]
        };
        let actual = Enfa::from(&Re::new(r"AB"));
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
        let actual = Enfa::from(&Re::new(r"A*"));
        assert_enfa_eq(expected, actual);
    }

    #[test]
    fn test_dfa_epsilon() {
        let expected = ExpectedDfa {
            initial: set![0, 1],
            transitions: set![],
            finals: set![set![0, 1]]
        };
        let actual = Dfa::from(&Re::new(r"[]"));
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
        let actual = Dfa::from(&Re::new(r"A"));
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
        let actual = Dfa::from(&Re::new(r"[^A]"));
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
        let actual = Dfa::from(&Re::new(r"[A-Za-z]"));
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
        let actual = Dfa::from(&Re::new(r"[^A-Za-z]"));
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
        let actual = Dfa::from(&Re::new(r"A|B"));
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
        let actual = Dfa::from(&Re::new(r"AB"));
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
        let actual = Dfa::from(&Re::new(r"A*"));
        assert_dfa_eq(expected, actual);
    }

    #[test]
    fn test_match_epsilon_1() {
        let mut re = Re::new(r"[]");
        re.compile();
        assert!(re.is_match(""));
    }

    #[test]
    fn test_match_epsilon_2() {
        let mut re = Re::new(r"[]");
        re.compile();
        assert!(!re.is_match("A"));
    }

    #[test]
    fn test_match_symbol_1() {
        let mut re = Re::new(r"A");
        re.compile();
        assert!(re.is_match("A"));
    }

    #[test]
    fn test_match_symbol_2() {
        let mut re = Re::new(r"A");
        re.compile();
        assert!(!re.is_match("B"));
    }

    #[test]
    fn test_match_negated_symbol_1() {
        let mut re = Re::new(r"[^A]");
        re.compile();
        assert!(re.is_match("B"));
    }

    #[test]
    fn test_match_negated_symbol_2() {
        let mut re = Re::new(r"[^A]");
        re.compile();
        assert!(!re.is_match("A"));
    }

    #[test]
    fn test_match_symbol_set_1() {
        let mut re = Re::new(r"[A-Za-z]");
        re.compile();
        assert!(re.is_match("D"));
    }

    #[test]
    fn test_match_symbol_set_2() {
        let mut re = Re::new(r"[A-Za-z]");
        re.compile();
        assert!(!re.is_match("_"));
    }

    #[test]
    fn test_match_negated_symbol_set_1() {
        let mut re = Re::new(r"[^A-Za-z]");
        re.compile();
        assert!(re.is_match("_"));
    }

    #[test]
    fn test_match_negated_symbol_set_2() {
        let mut re = Re::new(r"[^A-Za-z]");
        re.compile();
        assert!(!re.is_match("D"));
    }

    #[test]
    fn test_match_alternation_1() {
        let mut re = Re::new(r"A|B");
        re.compile();
        assert!(re.is_match("A"));
    }

    #[test]
    fn test_match_alternation_2() {
        let mut re = Re::new(r"A|B");
        re.compile();
        assert!(re.is_match("B"));
    }

    #[test]
    fn test_match_alternation_3() {
        let mut re = Re::new(r"A|B");
        re.compile();
        assert!(!re.is_match("C"));
    }

    #[test]
    fn test_match_concatenation_1() {
        let mut re = Re::new(r"AB");
        re.compile();
        assert!(re.is_match("AB"));
    }

    #[test]
    fn test_match_concatenation_2() {
        let mut re = Re::new(r"AB");
        re.compile();
        assert!(!re.is_match("AA"));
    }

    #[test]
    fn test_match_repetition_1() {
        let mut re = Re::new(r"A*");
        re.compile();
        assert!(re.is_match("AAAAAAAAA"));
    }

    #[test]
    fn test_match_repetition_2() {
        let mut re = Re::new(r"A*");
        re.compile();
        assert!(!re.is_match("AAAABAAAA"));
    }

    #[test]
    fn test_match_repetition_3() {
        let mut re = Re::new(r"A*");
        re.compile();
        assert!(re.is_match(""));
    }

    #[test]
    fn test_match_repetition_4() {
        let mut re = Re::new(r"A+");
        re.compile();
        assert!(re.is_match("AAAAAAAAAA"));
    }

    #[test]
    fn test_match_repetition_5() {
        let mut re = Re::new(r"A+");
        re.compile();
        assert!(!re.is_match("AAAABAAAAA"));
    }

    #[test]
    fn test_match_repetition_6() {
        let mut re = Re::new(r"A+");
        re.compile();
        assert!(!re.is_match(""));
    }

    #[test]
    fn test_match_repetition_7() {
        let mut re = Re::new(r"A?");
        re.compile();
        assert!(re.is_match("A"));
    }

    #[test]
    fn test_match_repetition_8() {
        let mut re = Re::new(r"A?");
        re.compile();
        assert!(!re.is_match("B"));
    }

    #[test]
    fn test_match_repetition_9() {
        let mut re = Re::new(r"A?");
        re.compile();
        assert!(re.is_match(""));
    }

    #[test]
    fn test_match_repetition_10() {
        let mut re = Re::new(r"A{,3}");
        re.compile();
        assert!(re.is_match(""));
    }

    #[test]
    fn test_match_repetition_11() {
        let mut re = Re::new(r"A{,3}");
        re.compile();
        assert!(re.is_match("AA"));
    }

    #[test]
    fn test_match_repetition_12() {
        let mut re = Re::new(r"A{,3}");
        re.compile();
        assert!(re.is_match("AAA"));
    }

    #[test]
    fn test_match_repetition_13() {
        let mut re = Re::new(r"A{,3}");
        re.compile();
        assert!(!re.is_match("AAAA"));
    }

    #[test]
    fn test_match_repetition_14() {
        let mut re = Re::new(r"A{3,}");
        re.compile();
        assert!(!re.is_match(""));
    }

    #[test]
    fn test_match_repetition_15() {
        let mut re = Re::new(r"A{3,}");
        re.compile();
        assert!(!re.is_match("AA"));
    }

    #[test]
    fn test_match_repetition_16() {
        let mut re = Re::new(r"A{3,}");
        re.compile();
        assert!(re.is_match("AAA"));
    }

    #[test]
    fn test_match_repetition_17() {
        let mut re = Re::new(r"A{3,}");
        re.compile();
        assert!(re.is_match("AAAA"));
    }

    #[test]
    fn test_match_repetition_18() {
        let mut re = Re::new(r"A{3}");
        re.compile();
        assert!(!re.is_match(""));
    }

    #[test]
    fn test_match_repetition_19() {
        let mut re = Re::new(r"A{3}");
        re.compile();
        assert!(!re.is_match("AA"));
    }

    #[test]
    fn test_match_repetition_20() {
        let mut re = Re::new(r"A{3}");
        re.compile();
        assert!(re.is_match("AAA"));
    }

    #[test]
    fn test_match_repetition_21() {
        let mut re = Re::new(r"A{3}");
        re.compile();
        assert!(!re.is_match("AAAA"));
    }

    #[test]
    fn test_match_repetition_22() {
        let mut re = Re::new(r"A{2,3}");
        re.compile();
        assert!(!re.is_match(""));
    }

    #[test]
    fn test_match_repetition_23() {
        let mut re = Re::new(r"A{2,3}");
        re.compile();
        assert!(re.is_match("AA"));
    }

    #[test]
    fn test_match_repetition_24() {
        let mut re = Re::new(r"A{2,3}");
        re.compile();
        assert!(re.is_match("AAA"));
    }

    #[test]
    fn test_match_repetition_25() {
        let mut re = Re::new(r"A{2,3}");
        re.compile();
        assert!(!re.is_match("AAAA"));
    }

    #[test]
    fn test_match_complex_1() {
        let mut re = Re::new(r"(A|B)*(AAA|BBB)(A|B)*");
        re.compile();
        assert!(re.is_match("ABBBA"));
    }

    #[test]
    fn test_match_complex_2() {
        let mut re = Re::new(r"(A|B)*(AAA|BBB)(A|B)*");
        re.compile();
        assert!(!re.is_match("ABBA"));
    }

    #[test]
    fn test_match_complex_3() {
        let mut re = Re::new(r"(A|B)*(AAA|BBB)(A|B)*");
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
