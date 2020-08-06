use std::collections::BTreeMap as Map;
use lazy_static::lazy_static;
use interval_map;
use re_bootstrap::{
    sym as rsym,
    neg as rneg,
    alt as ralt,
    con as rcon,
    rep as rrep,
    sgl as rsgl,
    rng as rrng,
    Expression,
    Re,
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
#[derive(Eq, Ord, PartialEq, PartialOrd)]
pub enum TokenKind {
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

#[derive(Eq, Ord, PartialEq, PartialOrd)]
pub enum Nonterminal {
    Expression,
    Alternation,
    Concatenation,
    Repetition,
    Atom,
    SymbolSet,
    SymbolSetRange,
    Literal,
    RepetitionExact,
    RepetitionMinimum,
    RepetitionMaximum,
    RepetitionRange,
    Integer,
}
use Nonterminal::*;

pub enum Grammar {
}

impl Grammar {
    pub fn new(_parse_tree: ParseTree<Nonterminal, TokenKind>) -> Grammar {
        panic!("Not implemented")
    }

    pub fn expression(&self) -> Expression {
        panic!("Not implemented")
    }
}

lazy_static! {
    // "\|" => VERTICAL_BAR;
    // "\*" => ASTERISK;
    // "\+" => PLUS_SIGN;
    // "\?" => QUESTION_MARK;
    // "\(" => LEFT_PARENTHESIS;
    // "\)" => RIGHT_PARENTHESIS;
    // "\[" => LEFT_SQUARE_BRACKET;
    // "\]" => RIGHT_SQUARE_BRACKET;
    // "\{" => LEFT_CURLY_BRACKET;
    // "\}" => RIGHT_CURLY_BRACKET;
    // "\^" => CARET;
    // "\-" => HYPHEN;
    // "," => COMMA;
    // "[0-9]" => DIGIT;
    // "\\[nrt]" => CONTROL;
    // "[^\|\*\+\?\(\)\[\]\{\}\^\-,0-9\n\r\t\\]" => UNESCAPED;
    // "\\[\|\*\+\?\(\)\[\]\{\}\^\-\\]" => ESCAPED;
    // "\\[0-7]{1,3}" => OCTAL;
    // "\\x[0-9a-fA-F]{1,2}" => HEXADECIMAL;
    // "\\(u[0-9a-fA-F]{4}|U[0-9a-fA-F]{8})" => UNICODE;
    static ref LEXER_PRODUCTIONS: Map<Re, Option<TokenKind>> = map![
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

    // Expression ::= Alternation;
    // Alternation :;= Concatenation (VERTICAL_BAR Concatenation)*;
    // Concatenation ::= Repetition+;
    // Repetition ::= Atom (ASTERISK | PLUS_SIGN | QUESTION_MARK | RepetitionExact | RepetitionMinimum | RepetitionMaximum | RepetitionRange)?;
    // Atom ::= SymbolSet | Literal | LEFT_PARENTHESIS Alternation RIGHT_PARENTHESIS;
    // SymbolSet ::= LEFT_SQUARE_BRACKET CARET? (SymbolSetRange | Literal)* RIGHT_SQUARE_BRACKET
    // SymbolSetRange ::= Literal HYPHEN Literal;
    // Literal ::= COMMA | DIGIT | CONTROL | UNESCAPED | ESCAPED | OCTAL | HEXADECIMAL | UNICODE;
    // RepetitionExact ::= LEFT_CURLY_BRACKET Integer RIGHT_CURLY_BRACKET;
    // RepetitionMinimum ::= LEFT_CURLY_BRACKET Integer COMMA RIGHT_CURLY_BRACKET;
    // RepetitionMaximum ::= LEFT_CURLY_BRACKET COMMA Integer RIGHT_CURLY_BRACKET;
    // RepetitionRange ::= LEFT_CURLY_BRACKET Integer COMMA Integer RIGHT_CURLY_BRACKET;
    // Integer ::= DIGIT+;
    static ref PARSER_PRODUCTIONS: Map<Nonterminal, parser_bootstrap::Expression<Nonterminal, TokenKind>> = map![
        Expression => pnon!(Alternation),
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
            pnon!(Literal),
            pcon![
                ptok!(LEFT_PARENTHESIS),
                pnon!(Alternation),
                ptok!(RIGHT_PARENTHESIS)
            ]
        ],
        SymbolSet => pcon![
            ptok!(LEFT_SQUARE_BRACKET),
            pque!(ptok!(CARET)),
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
