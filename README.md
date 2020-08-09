# Module `re`

A library for parsing, compiling, and executing [regular expressions](https://en.wikipedia.org/wiki/Regular_expression).

## Structs

### `re::Re`

```rust
pub struct Re { /* fields omitted */ }
```

A regular expression.

#### Examples

```rust
let re = Re::new(r"[0-9]+\.?[0-9]*");

assert_eq!(re.is_match("12"), true);
assert_eq!(re.is_match("12.345"), true);
assert_eq!(re.is_match("A"), false);
```

#### Implementations

##### `pub fn new(expression: &str) -> Result<Re>`

Constructs a new regular expression by parsing and compiling the provided expression.

###### Examples

```rust
let re = Re::new(r"[0-9a-fA-F]{1,2}");
```

##### `pub fn is_match(text: &str) -> bool`

Checks whether the provided text matches the regular expression exactly.

###### Examples

```rust
let re = Re::new(r"A*");

assert_eq!(re.is_match("AAA"), true);
assert_eq!(re.is_match("BAA"), false);
```

##### `pub fn into_expression(self) -> Expression`

Consume the regular expression and extract the underlying `Expression` represenation. This is useful for manipulating regular expressions and their associated `FiniteAutomata`.

###### Examples

```rust
let re = Re::new(r"[^A]");

assert_eq!(re.into_expression(), Expression::NegatedSymbolSet {
  intervals: vec![
    Interval::singleton(u32::from('A'))
  ]
})
```

##### `pub fn as_expression(&self) -> &Expression`

Get an immutable reference to the underlying `Expression` representation.

###### Examples

```rust
let re = Re::new(r"[^A]");

assert_eq!(re.as_expression(), &Expression::NegatedSymbolSet {
  intervals: vec![
    Interval::singleton(u32::from('A'))
  ]
})
```

## Enums

### `re::Expression`

```rust
pub enum Expression {
  SymbolSet { intervals: Vec<Interval<u32>> },
  NegatedSymbolSet { intervals: Vec<Interval<u32>> },
  Alternation { expressions: Vec<Expression> },
  Concatenation { expressions: Vec<Expression> },
  Repetition { expression: Box<Expression>, min: Option<u32>, max: Option<u32> },
}
```

#### Examples

```rust
let expression = Expression::Repetition { 
  expression: Box::new(Expresion::SymbolSet {
    intervals: vec![
      Interval::closed(u32::from('0'), u32::from('9')),
      Interval::closed(u32::from('a'), u32::from('f')),
      Interval::closed(u32::from('A'), u32::from('F')),
    ]
  }),
  min: Some(1),
  max: Some(2),
}

assert_eq!(expression, Re::new(r"[0-9a-fA-F]{1,2}").into_expression());
```

#### Variants

##### `SymbolSet { intervals: Vec<Interval<u32>> }`

A set of sybmols to match. Equivalent to the `[...]` syntax in standard regular expressions. For single symbol, use `Interval::singleton(...)`. For a range of symbols, use `Interval::closed(...)`. 

Note: To support contiguous ranges, characters must be converted to `u32`.

##### `NegatedSymbolSet { intervals: Vec<Interval<u32>> }`

A set of symbols to not match. Equivalent to the `[^...]` syntax in standard regular expressions. For single symbol, use `Interval::singleton(...)`. For a range of symbols, use `Interval::closed(...)`. 

Note: To support contiguous ranges, characters must be converted to `u32`.

##### `Alternation { expressions: Vec<Expression> }`

A collection of expressions of which to match a single one. If multiple expressions match, the longest one is used. Equivalent to the `|` operator in standard regular expressions.

##### `Concatenation { expressions: Vec<Expression> }`

A collection of expressions to match all concatenated together. Equivalent to simply placed expressions side-by-side in standard regular expressions.

##### `Repetition { expression: Box<Expression>, min: Option<u32>, max: Option<u32> }`

Match a single expression a given number of times. The following are equivalent in standard regular expressions:

- `min: None, max: None` and `*`
- `min: Some(1), max: None` and `+`
- `min: None, max: Some(1)` and `?`
- `min: Some(x), max: None` and `{x,}`
- `min: None, max: Some(y)` and `{,y}`
- `min: Some(x), Some(y)` and `{x,y}`

#### Implementations

##### `pub fn as_enfa<S, G>(&self, states: &mut G) -> Enfa<S, u32>`

Construction a nondeterministic finite automaton with epsilon moves from the `Expression` using the provided state generator.

###### Examples

```rust
let enfa = Expression::NegatedSymbolSet {
  intervals: vec![
    Interval::singleton(u32::from('A'))
  ]
}.as_enfa(&mut SimpleStateGenerator::new());

assert_eq!(enfa, Enfa {
  initial: 0,
  transitions: set![
    (0, Interval::less_than(u32::from('A')), 1),
    (0, Interval::greater_than(u32::from('A')), 1)
  ],
  finals: set![1]
})
```

Note: The internals of `Enfa` are much more complicated than what is shown. Therefore, the preceding example is somewhat pseudocode. More complete examples exist in the testing suite.

## Traits

### `re::StateGenerator`

```rust
pub trait StateGenerator {
  type State;
  fn next_initial(&mut self) -> Self::State;
  fn next_final(&mut self) -> Self::State;
  fn disable_final(&mut self) -> &mut Self;
  fn enable_final(&mut self) -> &mut Self;
}
```

A state generator for converting expressions into finite automata.

#### Examples

This is a simple state generator that does not support disabling final state generation.

```rust
pub struct SimpleStateGenerator {
  state: u64,
}

impl SimpleStateGenerator {
  pub fn new() -> SimpleStateGenerator {
    SimpleStateGenerator { state: 0u64 }
  }
}

impl StateGenerator for SimpleStateGenerator {
  type State = u64;
  
  fn next_initial(&mut self) -> u64 {
    let state = self.state;
    self.state += 1;
    state
  }
  
  fn next_final(&mut self) -> u64 {
    let state = self.state;
    self.state += 1;
    state
  }
  
  fn disable_final(&mut self) -> &mut SimpleStateGenerator {
    self
  }
  
  fn enable_final(&mut self) -> &mut SimpleStateGenerator {
    self
  }
}
```

#### Associated Types

##### `type State`

The associated state that is returned when generating states.

#### Required Methods

##### `fn next_initial(&mut self) -> Self::State`

Generate a new initial state.

##### `fn next_final(&mut self) -> Self::State`

Generate a new final state.

##### `fn disable_final(&mut self) -> &mut Self`

Temporarily disable final state generation. This is useful for nesting state generation where final states should only exist on the top-most level.

##### `fn enable_final(&mut self) -> &mut Self`

Re-enable final state generation.

## Macros

### `re::sym`

```rust
macro_rules! sym {
  ($($x:expr),*) => { ... }
}
```

Construct a `SymboleSet` expression with the provided intervals.

### `re::neg`

```rust
macro_rules! neg {
  ($($x:expr),*) => { ... }
}
```

Construct a `NegatedSymbolSet` expression with the provided intervals.

### `re::alt`

```rust
macro_rules! alt {
  ($($x:expr),*) => { ... }
}
```

Construct an `Alternation` expression with the provided expressions.

### `re::con`

```rust
macro_rules! con {
  ($($x:expr),*) => { ... }
}
```

Construct a `Concatenation` expression with the provided expressions.

### `re::rep`

```rust
macro_rules! rep {
  ($x:expr, $y:expr, $z:expr) => { ... }
}
```

Construct a `Repetition` expression with the provided expression, minimum, and maximum.

### `re::ast`

```rust
macro_rules! ast {
  ($x:expr) => { ... }
}
```

Construct a `Repetition` expression with the provided expression repeating any number of times. Equivalent to the `*` (asterisk) operator in standard regular expressions.

### `re::plu`

```rust
macro_rules! plu {
  ($x:expr) => { ... }
}
```

Construct a `Repetition` expression with the provided expression repeating at least one time. Equivalent to the `+` (plus sign) operator in standard regular expressions.

### `re::que`

```rust
macro_rules! que {
  ($x:expr) => { ... }
}
```

Construct a `Repetition` expression with the provided expression repeating at most one time. Equivalent to the `?` (question mark) operator in standard regular expressions.

### `re::sgl`

```rust
macro_rules! sgl {
  ($x:expr) => { ... }
}
```

Construct an `Interval::<u32>::singleton(...)` from the provided character.

### `re::rng`

```rust
macro_rules! rng {
  ($x:expr, $y:expr) => { ... }
}
```

Construct an `Interval::<u32>::closed(...)` from the provided pair of characters.

### `re::all`

```rust
macro_rules! all {
  () => { ... }
}
```

Construct an `Interval::<u32>::all(...)`.
