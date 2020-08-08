use std::collections::BTreeMap as Map;
use lazy_static::lazy_static;
use interval_map::Interval;
use re_bootstrap::{
    sym as rsym,
    neg as rneg,
    alt as ralt,
    con as rcon,
    rep as rrep,
    sgl as rsgl,
    rng as rrng,
    Expression,
    map,
};
use parser_bootstrap::{
    tok as ptok,
    non as pnon,
    alt as palt,
    con as pcon,
    ast as past,
    plu as pplu,
    que as pque,
    ParseTree,
};

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum TokenKind {
    FULL_STOP,
    VERTICAL_BAR,
    ASTERISK,
    PLUS_SIGN,
    QUESTION_MARK,
    LEFT_PARENTHESIS,
    RIGHT_PARENTHESIS,
    LEFT_SQUARE_BRACKET,
    RIGHT_SQUARE_BRACKET,
    LEFT_CURLY_BRACKET,
    RIGHT_CURLY_BRACKET,
    CARET,
    HYPHEN,
    COMMA,
    DIGIT,
    CONTROL,
    UNESCAPED,
    ESCAPED,
    OCTAL,
    HEXADECIMAL,
    UNICODE,
}
use TokenKind::*;

#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Nonterminal {
    Root,
    Alternation,
    Concatenation,
    Repetition,
    Atom,
    SymbolSet,
    NegatedSymbolSet,
    SymbolSetRange,
    Literal,
    RepetitionExact,
    RepetitionMinimum,
    RepetitionMaximum,
    RepetitionRange,
    Integer,
}
use Nonterminal::*;

pub fn as_expression(parse_tree: &ParseTree<Nonterminal, TokenKind>) -> Expression {
    match parse_tree {
        ParseTree::Nonterminal { nonterminal, children, .. } => {
            match nonterminal {
                // Root ::= Alternation;
                Root => {
                    as_expression(&children[0])
                },
                // Alternation :;= Concatenation (VERTICAL_BAR Concatenation)*;
                Alternation => {
                    if children.len() > 1 {
                        let mut expressions = Vec::new();
                        let mut skip = false;
                        for child in children {
                            if !skip {
                                expressions.push(as_expression(child));
                            }
                            skip = !skip;
                        }
                        Expression::Alternation { expressions }
                    } else {
                        as_expression(&children[0])
                    }
                },
                // Concatenation ::= Repetition+;
                Concatenation => {
                    if children.len() > 1 {
                        let mut expressions = Vec::new();
                        for child in children {
                            expressions.push(as_expression(child));
                        }
                        Expression::Concatenation { expressions }
                    } else {
                        as_expression(&children[0])
                    }
                },
                // Repetition ::= Atom (ASTERISK | PLUS_SIGN | QUESTION_MARK | RepetitionExact | RepetitionMinimum | RepetitionMaximum | RepetitionRange)?;
                Repetition => {
                    if children.len() > 1 {
                        let expression = Box::new(as_expression(&children[0]));
                        let (min, max) = as_range(&children[1]);
                        Expression::Repetition { expression, min, max }
                    } else {
                        as_expression(&children[0])
                    }
                },
                // Atom ::= SymbolSet | NegatedSymbolSet | Literal | FULL_STOP | LEFT_PARENTHESIS Alternation RIGHT_PARENTHESIS;
                Atom => {
                    if children.len() > 1 {
                        as_expression(&children[1])
                    } else {
                        as_expression(&children[0])
                    }
                },
                // SymbolSet ::= LEFT_SQUARE_BRACKET (SymbolSetRange | Literal)* RIGHT_SQUARE_BRACKET
                SymbolSet => {
                    let mut intervals = Vec::new();
                    for i in 1..children.len()-1 {
                        intervals.push(as_interval(&children[i]))
                    }
                    Expression::SymbolSet { intervals }
                },
                // NegatedSymbolSet ::= LEFT_SQUARE_BRACKET CARET (SymbolSetRange | Literal)* RIGHT_SQUARE_BRACKET
                NegatedSymbolSet => {
                    let mut intervals = Vec::new();
                    for i in 2..children.len()-1 {
                        intervals.push(as_interval(&children[i]))
                    }
                    Expression::NegatedSymbolSet { intervals }
                },
                // Literal ::= COMMA | DIGIT | CONTROL | UNESCAPED | ESCAPED | OCTAL | HEXADECIMAL | UNICODE;
                Literal => {
                    Expression::SymbolSet { intervals: vec![Interval::singleton(as_literal(&children[0]))] }
                },
                _ => panic!("not expression")
            }
        },
        ParseTree::Token { token } => {
            if let FULL_STOP = token.kind() {
                Expression::SymbolSet { intervals: vec![Interval::all()] }
            } else { panic!("not expression") }
        },
        _ => panic!("not expression")
    }
}

fn as_range(parse_tree: &ParseTree<Nonterminal, TokenKind>) -> (Option<u32>, Option<u32>) {
    match parse_tree {
        ParseTree::Nonterminal { nonterminal, children, .. } => {
            match nonterminal {
                // RepetitionExact ::= LEFT_CURLY_BRACKET Integer RIGHT_CURLY_BRACKET;
                RepetitionExact => {
                    let value = as_integer(&children[1]);
                    (Some(value), Some(value))
                },
                // RepetitionMinimum ::= LEFT_CURLY_BRACKET Integer COMMA RIGHT_CURLY_BRACKET;
                RepetitionMinimum => {
                    let min = as_integer(&children[1]);
                    (Some(min), None)
                },
                // RepetitionMaximum ::= LEFT_CURLY_BRACKET COMMA Integer RIGHT_CURLY_BRACKET;
                RepetitionMaximum => {
                    let max = as_integer(&children[2]);
                    (None, Some(max))
                },
                // RepetitionRange ::= LEFT_CURLY_BRACKET Integer COMMA Integer RIGHT_CURLY_BRACKET;
                RepetitionRange => {
                    let min = as_integer(&children[1]);
                    let max = as_integer(&children[3]);
                    (Some(min), Some(max))
                },
                _ => panic!("not range")
            }
        },
        ParseTree::Token { token } => {
            match token.kind() {
                // /\*/ => ASTERISK;
                ASTERISK => {
                    (None, None)
                },
                // /\+/ => PLUS_SIGN;
                PLUS_SIGN => {
                    (Some(1), None)
                },
                // /\?/ => QUESTION_MARK;
                QUESTION_MARK => {
                    (None, Some(1))
                },
                _ => panic!("not range")
            }
        },
        _ => panic!("not range")
    }
}

fn as_interval(parse_tree: &ParseTree<Nonterminal, TokenKind>) -> Interval<u32> {
    match parse_tree {
        ParseTree::Nonterminal { nonterminal, children, .. } => {
            match nonterminal {
                // SymbolSetRange ::= Literal HYPHEN Literal;
                SymbolSetRange => {
                    Interval::closed(*as_interval(&children[0]).lower(), *as_interval(&children[2]).lower())
                },
                // Literal ::= COMMA | DIGIT | CONTROL | UNESCAPED | ESCAPED | OCTAL | HEXADECIMAL | UNICODE;
                Literal => {
                    Interval::singleton(as_literal(&children[0]))
                },
                _ => panic!("not interval")
            }
        },
        _ => panic!("not interval")
    }
}

fn as_literal(parse_tree: &ParseTree<Nonterminal, TokenKind>) -> u32 {
    if let ParseTree::Token { token } = parse_tree {
        match token.kind() {
            // /,/ => COMMA;
            COMMA => {
                u32::from(',')
            },
            // /[0-9]/ => DIGIT;
            DIGIT => {
                u32::from(token.text().chars().next().expect("not literal"))
            },
            // /\\[nrt]/ => CONTROL;
            CONTROL => {
                let mut chars = token.text().chars();
                chars.next().expect("not literal"); // consume /\\/
                match chars.next().expect("not literal") {
                    'n' => {
                        u32::from('\n')
                    },
                    'r' => {
                        u32::from('\r')
                    },
                    't' => {
                        u32::from('\t')
                    },
                    _ => panic!("not literal")
                }
            },
            // /[^\.\/\|\*\+\?\(\)\[\]\{\}\^\-,0-9\n\r\t\\]/ => UNESCAPED;
            UNESCAPED => {
                u32::from(token.text().chars().next().expect("not literal"))
            },
            // /\\[\.\/\|\*\+\?\(\)\[\]\{\}\^\-\\]/ => ESCAPED;
            ESCAPED => {
                let mut chars = token.text().chars();
                chars.next().expect("not literal"); // consume /\\/
                u32::from(chars.next().expect("not literal"))
            },
            // /\\[0-7]{1,3}/ => OCTAL;
            OCTAL => {
                let mut chars = token.text().chars();
                chars.next().expect("not literal"); // consume /\\/
                let mut octal = 0;
                while let Some(digit) = chars.next() {
                    octal = (octal * 8) + digit.to_digit(8).expect("not literal")
                }
                octal
            },
            // /\\x[0-9a-fA-F]{1,2}/ => HEXADECIMAL;
            HEXADECIMAL => {
                let mut chars = token.text().chars();
                chars.next().expect("not literal"); // consume /\\/
                chars.next().expect("not literal"); // consume /x/
                let mut hexadecimal = 0;
                while let Some(digit) = chars.next() {
                    hexadecimal = (hexadecimal * 16) + digit.to_digit(16).expect("not literal")
                }
                hexadecimal
            },
            // /\\(u[0-9a-fA-F]{4}|U[0-9a-fA-F]{8})/ => UNICODE;
            UNICODE => {
                let mut chars = token.text().chars();
                chars.next().expect("not literal"); // consume /\\/
                chars.next().expect("not literal"); // consume /u|U/
                let mut unicode = 0;
                while let Some(digit) = chars.next() {
                    unicode = (unicode * 16) + digit.to_digit(16).expect("not literal")
                }
                unicode
            },
            _ => panic!("not literal")
        }
    } else { panic!("not literal") }
}

fn as_integer(parse_tree: &ParseTree<Nonterminal, TokenKind>) -> u32 {
    if let ParseTree::Nonterminal { nonterminal, children, .. } = parse_tree {
        if let Integer = nonterminal {
            let mut integer = 0;
            for child in children {
                integer = (integer * 10) + as_digit(child);
            }
            integer
        } else { panic!("not integer") }
    } else { panic!("not integer") }
}

fn as_digit(parse_tree: &ParseTree<Nonterminal, TokenKind>) -> u32 {
    if let ParseTree::Token { token } = parse_tree {
        if let DIGIT = token.kind() {
            token.text().parse::<u32>().expect("not digit")
        } else { panic!("not digit") }
    } else { panic!("not digit") }
}

lazy_static! {
    // /\./ => FULL_STOP;
    // /\|/ => VERTICAL_BAR;
    // /\*/ => ASTERISK;
    // /\+/ => PLUS_SIGN;
    // /\?/ => QUESTION_MARK;
    // /\(/ => LEFT_PARENTHESIS;
    // /\)/ => RIGHT_PARENTHESIS;
    // /\[/ => LEFT_SQUARE_BRACKET;
    // /\]/ => RIGHT_SQUARE_BRACKET;
    // /\{/ => LEFT_CURLY_BRACKET;
    // /\}/ => RIGHT_CURLY_BRACKET;
    // /\^/ => CARET;
    // /\-/ => HYPHEN;
    // /,/ => COMMA;
    // /[0-9]/ => DIGIT;
    // /\\[nrt]/ => CONTROL;
    // /[^\.\/\|\*\+\?\(\)\[\]\{\}\^\-,0-9\n\r\t\\]/ => UNESCAPED;
    // /\\[\.\/\|\*\+\?\(\)\[\]\{\}\^\-\\]/ => ESCAPED;
    // /\\[0-7]{1,3}/ => OCTAL;
    // /\\x[0-9a-fA-F]{1,2}/ => HEXADECIMAL;
    // /\\(u[0-9a-fA-F]{4}|U[0-9a-fA-F]{8})/ => UNICODE;
    pub(crate) static ref LEXER_PRODUCTIONS: Map<re_bootstrap::Expression, Option<TokenKind>> = map![
        rsym![rsgl!('.')] => Some(FULL_STOP),
        rsym![rsgl!('|')] => Some(VERTICAL_BAR),
        rsym![rsgl!('*')] => Some(ASTERISK),
        rsym![rsgl!('+')] => Some(PLUS_SIGN),
        rsym![rsgl!('?')] => Some(QUESTION_MARK),
        rsym![rsgl!('(')] => Some(LEFT_PARENTHESIS),
        rsym![rsgl!(')')] => Some(RIGHT_PARENTHESIS),
        rsym![rsgl!('[')] => Some(LEFT_SQUARE_BRACKET),
        rsym![rsgl!(']')] => Some(RIGHT_SQUARE_BRACKET),
        rsym![rsgl!('{')] => Some(LEFT_CURLY_BRACKET),
        rsym![rsgl!('}')] => Some(RIGHT_CURLY_BRACKET),
        rsym![rsgl!('^')] => Some(CARET),
        rsym![rsgl!('-')] => Some(HYPHEN),
        rsym![rsgl!(',')] => Some(COMMA),
        rsym![rrng!('0', '9')] => Some(DIGIT),
        rcon![
            rsym![rsgl!('\\')],
            rsym![
                rsgl!('n'),
                rsgl!('r'),
                rsgl!('t')
            ]
        ] => Some(CONTROL),
        rneg![
            rsgl!('.'),
            rsgl!('/'),
            rsgl!('|'),
            rsgl!('*'),
            rsgl!('+'),
            rsgl!('?'),
            rsgl!('('),
            rsgl!(')'),
            rsgl!('['),
            rsgl!(']'),
            rsgl!('{'),
            rsgl!('}'),
            rsgl!('^'),
            rsgl!('-'),
            rsgl!(','),
            rrng!('0', '9'),
            rsgl!('\n'),
            rsgl!('\r'),
            rsgl!('\t'),
            rsgl!('\\')
        ] => Some(UNESCAPED),
        rcon![
            rsym![rsgl!('\\')],
            rsym![
                rsgl!('.'),
                rsgl!('/'),
                rsgl!('|'),
                rsgl!('*'),
                rsgl!('+'),
                rsgl!('?'),
                rsgl!('('),
                rsgl!(')'),
                rsgl!('['),
                rsgl!(']'),
                rsgl!('{'),
                rsgl!('}'),
                rsgl!('^'),
                rsgl!('-'),
                rsgl!('\\')
            ]
        ] => Some(ESCAPED),
        rcon![
            rsym![rsgl!('\\')],
            rrep!(rsym![rrng!('0', '7')], Some(1), Some(3))
        ] => Some(OCTAL),
        rcon![
            rsym![rsgl!('\\')],
            rsym![rsgl!('x')],
            rrep!(rsym![
                 rrng!('0', '9'),
                 rrng!('a', 'f'),
                 rrng!('A', 'F')
            ], Some(1), Some(2))
        ] => Some(HEXADECIMAL),
        rcon![
            rsym![rsgl!('\\')],
            ralt![
                rcon![
                    rsym![rsgl!('u')],
                    rrep!(rsym![
                        rrng!('0', '9'),
                        rrng!('a', 'f'),
                        rrng!('A', 'F')
                    ], Some(4), Some(4))
                ],
                rcon![
                    rsym![rsgl!('U')],
                    rrep!(rsym![
                        rrng!('0', '9'),
                        rrng!('a', 'f'),
                        rrng!('A', 'F')
                    ], Some(8), Some(8))
                ]
            ]
        ] => Some(UNICODE)
    ];

    // Root ::= Alternation;
    // Alternation ::= Concatenation (VERTICAL_BAR Concatenation)*;
    // Concatenation ::= Repetition+;
    // Repetition ::= Atom (ASTERISK | PLUS_SIGN | QUESTION_MARK | RepetitionExact | RepetitionMinimum | RepetitionMaximum | RepetitionRange)?;
    // Atom ::= SymbolSet | NegatedSymbolSet | Literal | FULL_STOP | LEFT_PARENTHESIS Alternation RIGHT_PARENTHESIS;
    // SymbolSet ::= LEFT_SQUARE_BRACKET (SymbolSetRange | Literal)* RIGHT_SQUARE_BRACKET
    // NegatedSymbolSet ::= LEFT_SQUARE_BRACKET CARET (SymbolSetRange | Literal)* RIGHT_SQUARE_BRACKET
    // SymbolSetRange ::= Literal HYPHEN Literal;
    // Literal ::= COMMA | DIGIT | CONTROL | UNESCAPED | ESCAPED | OCTAL | HEXADECIMAL | UNICODE;
    // RepetitionExact ::= LEFT_CURLY_BRACKET Integer RIGHT_CURLY_BRACKET;
    // RepetitionMinimum ::= LEFT_CURLY_BRACKET Integer COMMA RIGHT_CURLY_BRACKET;
    // RepetitionMaximum ::= LEFT_CURLY_BRACKET COMMA Integer RIGHT_CURLY_BRACKET;
    // RepetitionRange ::= LEFT_CURLY_BRACKET Integer COMMA Integer RIGHT_CURLY_BRACKET;
    // Integer ::= DIGIT+;
    pub(crate) static ref PARSER_PRODUCTIONS: Map<Nonterminal, parser_bootstrap::Expression<Nonterminal, TokenKind>> = map![
        Root => pnon!(Alternation),
        Alternation => pcon![
            pnon!(Concatenation),
            past!(pcon![
                 ptok!(VERTICAL_BAR),
                 pnon!(Concatenation)
            ])
        ],
        Concatenation => pplu!(pnon!(Repetition)),
        Repetition => pcon![
            pnon!(Atom),
            pque!(palt![
                ptok!(ASTERISK),
                ptok!(PLUS_SIGN),
                ptok!(QUESTION_MARK),
                pnon!(RepetitionExact),
                pnon!(RepetitionMinimum),
                pnon!(RepetitionMaximum),
                pnon!(RepetitionRange)
            ])
        ],
        Atom => palt![
            pnon!(SymbolSet),
            pnon!(NegatedSymbolSet),
            pnon!(Literal),
            ptok!(FULL_STOP),
            pcon![
                ptok!(LEFT_PARENTHESIS),
                pnon!(Alternation),
                ptok!(RIGHT_PARENTHESIS)
            ]
        ],
        SymbolSet => pcon![
            ptok!(LEFT_SQUARE_BRACKET),
            past!(palt![
                 pnon!(SymbolSetRange),
                 pnon!(Literal)
            ]),
            ptok!(RIGHT_SQUARE_BRACKET)
        ],
        NegatedSymbolSet => pcon![
            ptok!(LEFT_SQUARE_BRACKET),
            ptok!(CARET),
            past!(palt![
                 pnon!(SymbolSetRange),
                 pnon!(Literal)
            ]),
            ptok!(RIGHT_SQUARE_BRACKET)
        ],
        SymbolSetRange => pcon![
            pnon!(Literal),
            ptok!(HYPHEN),
            pnon!(Literal)
        ],
        Literal => palt![
            ptok!(COMMA),
            ptok!(DIGIT),
            ptok!(CONTROL),
            ptok!(UNESCAPED),
            ptok!(ESCAPED),
            ptok!(OCTAL),
            ptok!(HEXADECIMAL),
            ptok!(UNICODE)
        ],
        RepetitionExact => pcon![
            ptok!(LEFT_CURLY_BRACKET),
            pnon!(Integer),
            ptok!(RIGHT_CURLY_BRACKET)
        ],
        RepetitionMinimum => pcon![
            ptok!(LEFT_CURLY_BRACKET),
            pnon!(Integer),
            ptok!(COMMA),
            ptok!(RIGHT_CURLY_BRACKET)
        ],
        RepetitionMaximum => pcon![
            ptok!(LEFT_CURLY_BRACKET),
            ptok!(COMMA),
            pnon!(Integer),
            ptok!(RIGHT_CURLY_BRACKET)
        ],
        RepetitionRange => pcon![
            ptok!(LEFT_CURLY_BRACKET),
            pnon!(Integer),
            ptok!(COMMA),
            pnon!(Integer),
            ptok!(RIGHT_CURLY_BRACKET)
        ],
        Integer => pplu!(ptok!(DIGIT))
    ];
}
