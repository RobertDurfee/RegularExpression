use std::collections::BTreeSet as Set;
use finite_automata::{
    Enfa,
    Nfa,
    Dfa,
};
use re_bootstrap::Re as ReBootstrap;
use crate::{
    grammar::{
        LEXER_PRODUCTIONS,
        PARSER_PRODUCTIONS,
        Nonterminal,
        as_expression,
    },
    Expression,
};
use lexer_bootstrap::Lexer;
use parser_bootstrap::Parser;

type Result<T> = std::result::Result<T, &'static str>;

pub struct Re {
    re: ReBootstrap,
}

impl Re {
    pub fn new(expression: &str) -> Result<Re> {
        let mut lexer = Lexer::new(LEXER_PRODUCTIONS.clone()); lexer.compile();
        let parser = Parser::new(PARSER_PRODUCTIONS.clone(), Nonterminal::Root);
        let tokens = lexer.lex(expression)?;
        let parse_tree = parser.parse(&tokens)?;
        let expression = as_expression(&parse_tree)?;
        Ok(Re { re: ReBootstrap::new(expression) })
    }

    pub fn compile(&mut self) {
        self.re.compile()
    }

    pub fn is_match(&self, text: &str) -> Result<bool> {
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
    use crate::{
        Re,
        set,
    };
    use super::Result;

    #[test]
    fn test_enfa_epsilon() -> Result<()> {
         let expected = ExpectedEnfa::<_, u32> {
            initial: 0,
            transitions: set![
                (0, Interval::empty(), 1)
            ],
            finals: set![1]
        };
        let actual = Enfa::from(&Re::new(r"[]")?);
        assert_enfa_eq(expected, actual);
        Ok(())
    }

    #[test]
    fn test_enfa_symbol() -> Result<()> {
        let expected = ExpectedEnfa {
            initial: 0,
            transitions: set![
                (0, Interval::singleton(u32::from('A')), 1)
            ],
            finals: set![1]
        };
        let actual = Enfa::from(&Re::new(r"A")?);
        assert_enfa_eq(expected, actual);
        Ok(())
    }

    #[test]
    fn test_enfa_negated_symbol() -> Result<()> {
        let expected = ExpectedEnfa {
            initial: 0,
            transitions: set![
                (0, Interval::less_than(u32::from('A')), 1),
                (0, Interval::greater_than(u32::from('A')), 1)
            ],
            finals: set![1]
        };
        let actual = Enfa::from(&Re::new(r"[^A]")?);
        assert_enfa_eq(expected, actual);
        Ok(())
    }

    #[test]
    fn test_enfa_symbol_set() -> Result<()> {
        let expected = ExpectedEnfa {
            initial: 0,
            transitions: set![
                (0, Interval::closed(u32::from('A'), u32::from('Z')), 1),
                (0, Interval::closed(u32::from('a'), u32::from('z')), 1)
            ],
            finals: set![1]
        };
        let actual = Enfa::from(&Re::new(r"[A-Za-z]")?);
        assert_enfa_eq(expected, actual);
        Ok(())
    }

    #[test]
    fn test_enfa_negated_symbol_set() -> Result<()> {
        let expected = ExpectedEnfa {
            initial: 0,
            transitions: set![
                (0, Interval::less_than(u32::from('A')), 1),
                (0, Interval::open(u32::from('Z'), u32::from('a')), 1),
                (0, Interval::greater_than(u32::from('z')), 1)
            ],
            finals: set![1]
        };
        let actual = Enfa::from(&Re::new(r"[^A-Za-z]")?);
        assert_enfa_eq(expected, actual);
        Ok(())
    }

    #[test]
    fn test_enfa_alternation() -> Result<()> {
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
        let actual = Enfa::from(&Re::new("A|B")?);
        assert_enfa_eq(expected, actual);
        Ok(())
    }

    #[test]
    fn test_enfa_concatenation() -> Result<()> {
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
        let actual = Enfa::from(&Re::new(r"AB")?);
        assert_enfa_eq(expected, actual);
        Ok(())
    }

    #[test]
    fn test_enfa_repetition() -> Result<()> {
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
        let actual = Enfa::from(&Re::new(r"A*")?);
        assert_enfa_eq(expected, actual);
        Ok(())
    }

    #[test]
    fn test_dfa_epsilon() -> Result<()> {
        let expected = ExpectedDfa {
            initial: set![0, 1],
            transitions: set![],
            finals: set![set![0, 1]]
        };
        let actual = Dfa::from(&Re::new(r"[]")?);
        assert_dfa_eq(expected, actual);
        Ok(())
    }

    #[test]
    fn test_dfa_symbol() -> Result<()> {
        let expected = ExpectedDfa {
            initial: set![0],
            transitions: set![
                (set![0], Interval::singleton(u32::from('A')), set![1])
            ],
            finals: set![set![1]]
        };
        let actual = Dfa::from(&Re::new(r"A")?);
        assert_dfa_eq(expected, actual);
        Ok(())
    }

    #[test]
    fn test_dfa_negated_symbol() -> Result<()> {
        let expected = ExpectedDfa {
            initial: set![0],
            transitions: set![
                (set![0], Interval::less_than(u32::from('A')), set![1]),
                (set![0], Interval::greater_than(u32::from('A')), set![1])
            ],
            finals: set![set![1]]
        };
        let actual = Dfa::from(&Re::new(r"[^A]")?);
        assert_dfa_eq(expected, actual);
        Ok(())
    }

    #[test]
    fn test_dfa_symbol_set() -> Result<()> {
        let expected = ExpectedDfa {
            initial: set![0],
            transitions: set![
                (set![0], Interval::closed(u32::from('A'), u32::from('Z')), set![1]),
                (set![0], Interval::closed(u32::from('a'), u32::from('z')), set![1])
            ],
            finals: set![set![1]]
        };
        let actual = Dfa::from(&Re::new(r"[A-Za-z]")?);
        assert_dfa_eq(expected, actual);
        Ok(())
    }

    #[test]
    fn test_dfa_negated_symbol_set() -> Result<()> {
        let expected = ExpectedDfa {
            initial: set![0],
            transitions: set![
                (set![0], Interval::less_than(u32::from('A')), set![1]),
                (set![0], Interval::open(u32::from('Z'), u32::from('a')), set![1]),
                (set![0], Interval::greater_than(u32::from('z')), set![1])
            ],
            finals: set![set![1]]
        };
        let actual = Dfa::from(&Re::new(r"[^A-Za-z]")?);
        assert_dfa_eq(expected, actual);
        Ok(())
    }

    #[test]
    fn test_dfa_alternation() -> Result<()> {
        let expected = ExpectedDfa {
            initial: set![0, 2, 4],
            transitions: set![
                (set![0, 2, 4], Interval::singleton(u32::from('A')), set![1, 3]),
                (set![0, 2, 4], Interval::singleton(u32::from('B')), set![1, 5])
            ],
            finals: set![set![1, 3], set![1, 5]]
        };
        let actual = Dfa::from(&Re::new(r"A|B")?);
        assert_dfa_eq(expected, actual);
        Ok(())
    }

    #[test]
    fn test_dfa_concatenation() -> Result<()> {
        let expected = ExpectedDfa {
            initial: set![0, 2],
            transitions: set![
                (set![0, 2], Interval::singleton(u32::from('A')), set![3, 4]),
                (set![3, 4], Interval::singleton(u32::from('B')), set![1, 5])
            ],
            finals: set![set![1, 5]]
        };
        let actual = Dfa::from(&Re::new(r"AB")?);
        assert_dfa_eq(expected, actual);
        Ok(())
    }

    #[test]
    fn test_dfa_repetition() -> Result<()> {
        let expected = ExpectedDfa {
            initial: set![0, 1, 2],
            transitions: set![
                (set![0, 1, 2], Interval::singleton(u32::from('A')), set![1, 2, 3]),
                (set![1, 2, 3], Interval::singleton(u32::from('A')), set![1, 2, 3])
            ],
            finals: set![set![0, 1, 2], set![1, 2, 3]]
        };
        let actual = Dfa::from(&Re::new(r"A*")?);
        assert_dfa_eq(expected, actual);
        Ok(())
    }

    #[test]
    fn test_match_epsilon_1() -> Result<()> {
        let mut re = Re::new(r"[]")?;
        re.compile();
        assert!(re.is_match("")?);
        Ok(())
    }

    #[test]
    fn test_match_epsilon_2() -> Result<()> {
        let mut re = Re::new(r"[]")?;
        re.compile();
        assert!(!re.is_match("A")?);
        Ok(())
    }

    #[test]
    fn test_match_symbol_1() -> Result<()> {
        let mut re = Re::new(r"A")?;
        re.compile();
        assert!(re.is_match("A")?);
        Ok(())
    }

    #[test]
    fn test_match_symbol_2() -> Result<()> {
        let mut re = Re::new(r"A")?;
        re.compile();
        assert!(!re.is_match("B")?);
        Ok(())
    }

    #[test]
    fn test_match_negated_symbol_1() -> Result<()> {
        let mut re = Re::new(r"[^A]")?;
        re.compile();
        assert!(re.is_match("B")?);
        Ok(())
    }

    #[test]
    fn test_match_negated_symbol_2() -> Result<()> {
        let mut re = Re::new(r"[^A]")?;
        re.compile();
        assert!(!re.is_match("A")?);
        Ok(())
    }

    #[test]
    fn test_match_symbol_set_1() -> Result<()> {
        let mut re = Re::new(r"[A-Za-z]")?;
        re.compile();
        assert!(re.is_match("D")?);
        Ok(())
    }

    #[test]
    fn test_match_symbol_set_2() -> Result<()> {
        let mut re = Re::new(r"[A-Za-z]")?;
        re.compile();
        assert!(!re.is_match("_")?);
        Ok(())
    }

    #[test]
    fn test_match_negated_symbol_set_1() -> Result<()> {
        let mut re = Re::new(r"[^A-Za-z]")?;
        re.compile();
        assert!(re.is_match("_")?);
        Ok(())
    }

    #[test]
    fn test_match_negated_symbol_set_2() -> Result<()> {
        let mut re = Re::new(r"[^A-Za-z]")?;
        re.compile();
        assert!(!re.is_match("D")?);
        Ok(())
    }

    #[test]
    fn test_match_alternation_1() -> Result<()> {
        let mut re = Re::new(r"A|B")?;
        re.compile();
        assert!(re.is_match("A")?);
        Ok(())
    }

    #[test]
    fn test_match_alternation_2() -> Result<()> {
        let mut re = Re::new(r"A|B")?;
        re.compile();
        assert!(re.is_match("B")?);
        Ok(())
    }

    #[test]
    fn test_match_alternation_3() -> Result<()> {
        let mut re = Re::new(r"A|B")?;
        re.compile();
        assert!(!re.is_match("C")?);
        Ok(())
    }

    #[test]
    fn test_match_concatenation_1() -> Result<()> {
        let mut re = Re::new(r"AB")?;
        re.compile();
        assert!(re.is_match("AB")?);
        Ok(())
    }

    #[test]
    fn test_match_concatenation_2() -> Result<()> {
        let mut re = Re::new(r"AB")?;
        re.compile();
        assert!(!re.is_match("AA")?);
        Ok(())
    }

    #[test]
    fn test_match_repetition_1() -> Result<()> {
        let mut re = Re::new(r"A*")?;
        re.compile();
        assert!(re.is_match("AAAAAAAAA")?);
        Ok(())
    }

    #[test]
    fn test_match_repetition_2() -> Result<()> {
        let mut re = Re::new(r"A*")?;
        re.compile();
        assert!(!re.is_match("AAAABAAAA")?);
        Ok(())
    }

    #[test]
    fn test_match_repetition_3() -> Result<()> {
        let mut re = Re::new(r"A*")?;
        re.compile();
        assert!(re.is_match("")?);
        Ok(())
    }

    #[test]
    fn test_match_repetition_4() -> Result<()> {
        let mut re = Re::new(r"A+")?;
        re.compile();
        assert!(re.is_match("AAAAAAAAAA")?);
        Ok(())
    }

    #[test]
    fn test_match_repetition_5() -> Result<()> {
        let mut re = Re::new(r"A+")?;
        re.compile();
        assert!(!re.is_match("AAAABAAAAA")?);
        Ok(())
    }

    #[test]
    fn test_match_repetition_6() -> Result<()> {
        let mut re = Re::new(r"A+")?;
        re.compile();
        assert!(!re.is_match("")?);
        Ok(())
    }

    #[test]
    fn test_match_repetition_7() -> Result<()> {
        let mut re = Re::new(r"A?")?;
        re.compile();
        assert!(re.is_match("A")?);
        Ok(())
    }

    #[test]
    fn test_match_repetition_8() -> Result<()> {
        let mut re = Re::new(r"A?")?;
        re.compile();
        assert!(!re.is_match("B")?);
        Ok(())
    }

    #[test]
    fn test_match_repetition_9() -> Result<()> {
        let mut re = Re::new(r"A?")?;
        re.compile();
        assert!(re.is_match("")?);
        Ok(())
    }

    #[test]
    fn test_match_repetition_10() -> Result<()> {
        let mut re = Re::new(r"A{,3}")?;
        re.compile();
        assert!(re.is_match("")?);
        Ok(())
    }

    #[test]
    fn test_match_repetition_11() -> Result<()> {
        let mut re = Re::new(r"A{,3}")?;
        re.compile();
        assert!(re.is_match("AA")?);
        Ok(())
    }

    #[test]
    fn test_match_repetition_12() -> Result<()> {
        let mut re = Re::new(r"A{,3}")?;
        re.compile();
        assert!(re.is_match("AAA")?);
        Ok(())
    }

    #[test]
    fn test_match_repetition_13() -> Result<()> {
        let mut re = Re::new(r"A{,3}")?;
        re.compile();
        assert!(!re.is_match("AAAA")?);
        Ok(())
    }

    #[test]
    fn test_match_repetition_14() -> Result<()> {
        let mut re = Re::new(r"A{3,}")?;
        re.compile();
        assert!(!re.is_match("")?);
        Ok(())
    }

    #[test]
    fn test_match_repetition_15() -> Result<()> {
        let mut re = Re::new(r"A{3,}")?;
        re.compile();
        assert!(!re.is_match("AA")?);
        Ok(())
    }

    #[test]
    fn test_match_repetition_16() -> Result<()> {
        let mut re = Re::new(r"A{3,}")?;
        re.compile();
        assert!(re.is_match("AAA")?);
        Ok(())
    }

    #[test]
    fn test_match_repetition_17() -> Result<()> {
        let mut re = Re::new(r"A{3,}")?;
        re.compile();
        assert!(re.is_match("AAAA")?);
        Ok(())
    }

    #[test]
    fn test_match_repetition_18() -> Result<()> {
        let mut re = Re::new(r"A{3}")?;
        re.compile();
        assert!(!re.is_match("")?);
        Ok(())
    }

    #[test]
    fn test_match_repetition_19() -> Result<()> {
        let mut re = Re::new(r"A{3}")?;
        re.compile();
        assert!(!re.is_match("AA")?);
        Ok(())
    }

    #[test]
    fn test_match_repetition_20() -> Result<()> {
        let mut re = Re::new(r"A{3}")?;
        re.compile();
        assert!(re.is_match("AAA")?);
        Ok(())
    }

    #[test]
    fn test_match_repetition_21() -> Result<()> {
        let mut re = Re::new(r"A{3}")?;
        re.compile();
        assert!(!re.is_match("AAAA")?);
        Ok(())
    }

    #[test]
    fn test_match_repetition_22() -> Result<()> {
        let mut re = Re::new(r"A{2,3}")?;
        re.compile();
        assert!(!re.is_match("")?);
        Ok(())
    }

    #[test]
    fn test_match_repetition_23() -> Result<()> {
        let mut re = Re::new(r"A{2,3}")?;
        re.compile();
        assert!(re.is_match("AA")?);
        Ok(())
    }

    #[test]
    fn test_match_repetition_24() -> Result<()> {
        let mut re = Re::new(r"A{2,3}")?;
        re.compile();
        assert!(re.is_match("AAA")?);
        Ok(())
    }

    #[test]
    fn test_match_repetition_25() -> Result<()> {
        let mut re = Re::new(r"A{2,3}")?;
        re.compile();
        assert!(!re.is_match("AAAA")?);
        Ok(())
    }

    #[test]
    fn test_match_complex_1() -> Result<()> {
        let mut re = Re::new(r"(A|B)*(AAA|BBB)(A|B)*")?;
        re.compile();
        assert!(re.is_match("ABBBA")?);
        Ok(())
    }

    #[test]
    fn test_match_complex_2() -> Result<()> {
        let mut re = Re::new(r"(A|B)*(AAA|BBB)(A|B)*")?;
        re.compile();
        assert!(!re.is_match("ABBA")?);
        Ok(())
    }

    #[test]
    fn test_match_complex_3() -> Result<()> {
        let mut re = Re::new(r"(A|B)*(AAA|BBB)(A|B)*")?;
        re.compile();
        assert!(re.is_match("ABBAAA")?);
        Ok(())
    }

    #[test]
    fn test_match_all() -> Result<()> {
        let mut re = Re::new(r".")?;
        re.compile();
        assert!(re.is_match("A")?);
        Ok(())
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
