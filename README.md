# BIL395HW2 - Multi-Language Calculator Implementations

This project contains implementations of a simple calculator in multiple programming languages: Ada, Perl, Prolog, Rust, and Scheme. Each implementation follows similar principles but leverages the unique features of its respective language.

## Common Features Across All Implementations

All calculator implementations share these core features:
- Variable assignment and retrieval
- Basic arithmetic operations (addition, subtraction, multiplication, division)
- Parentheses support for expression grouping
- REPL (Read-Eval-Print Loop) interface
- Error handling for common issues (division by zero, undefined variables, etc.)

## Ada Calculator

### Implementation Details

The Ada calculator uses a recursive descent parser to evaluate mathematical expressions. It's implemented with strong typing and Ada's package system.

#### Key Components:
- **Variable Storage**: Uses an associative array to store variable values
- **Tokenizer**: Converts input strings into tokens (numbers, identifiers, operators)
- **Parser**: Implements a recursive descent parser with separate functions for different precedence levels
- **REPL**: Provides a command-line interface for user interaction

### How to Run
``` 
cd ada_calculator
gnatmake simple_calculator.adb
./simple_calculator
```

## Perl Calculator

### Implementation Details

The Perl calculator uses regular expressions and recursive parsing to evaluate expressions. It leverages Perl's powerful text processing capabilities.

#### Key Components:
- **Variable Storage**: Uses a Perl hash to store variable values
- **Expression Parsing**: Splits expressions while preserving parenthesized subexpressions
- **Operator Precedence**: Handles operator precedence through separate parsing functions
- **Error Handling**: Provides informative error messages for syntax errors and undefined variables

### How to Run
``` 
cd perl_calculator
perl perl_calculator.pl
```

## Prolog Calculator

### Implementation Details

The Prolog calculator uses logic programming and pattern matching to parse and evaluate expressions. It leverages Prolog's declarative nature and unification mechanism.

#### Key Components:
- **Variable Storage**: Uses Prolog's dynamic predicates to store variable values
- **Tokenizer**: Converts input strings into token lists
- **Expression Evaluation**: Uses pattern matching to recognize and evaluate different expression types
- **Operator Precedence**: Implemented through the structure of the parsing rules

### How to Run
``` 
cd prolog_calculator
swipl -s calculator.pl
```

## Rust Calculator

### Implementation Details

The Rust calculator uses a combination of lexing and parsing techniques with Rust's strong type system and pattern matching.

#### Key Components:
- **Variable Storage**: Uses a HashMap to store variable values
- **Lexer**: Converts input strings into a stream of tokens
- **Parser**: Implements a recursive descent parser with proper error handling
- **Memory Safety**: Leverages Rust's ownership model for safe memory management

### How to Run
``` 
cd rust_calculator
cargo run
```

## Scheme Calculator

### Implementation Details

The Scheme calculator uses functional programming techniques and recursive functions to parse and evaluate expressions.

#### Key Components:
- **Variable Storage**: Uses an association list to store variable values
- **Tokenizer**: Converts input strings into lists of tokens
- **Recursive Descent Parser**: Implements parsing through mutually recursive functions
- **Error Handling**: Uses Scheme's exception handling for error conditions

### How to Run
``` 
cd scheme_calculator
racket -f calculator.scm
```

## Usage Examples

All calculators support similar syntax:

```
> x = 5          # Variable assignment
> y = 3          # Another assignment
> x + y          # Addition
> (x + y) * 2    # Parenthesized expression
> z = x * (y + 2) # Combining operations
> exit           # Exit the calculator
```

## Implementation Comparison

| Language | Paradigm | Variable Storage | Parsing Technique | Error Handling |
|----------|----------|------------------|-------------------|----------------|
| Ada      | Imperative, OOP | Associative Array | Recursive Descent | Exceptions |
| Perl     | Imperative, Procedural | Hash | Regex & Recursive Parsing | Error Messages |
| Prolog   | Logic, Declarative | Dynamic Predicates | Pattern Matching | Failure/Backtracking |
| Rust     | Imperative, Functional | HashMap | Recursive Descent | Result/Option Types |
| Scheme   | Functional | Association List | Recursive Functions | Exception Handling |

## Conclusion

This project demonstrates how the same computational problem can be solved using different programming languages and paradigms. Each implementation showcases the strengths and unique features of its respective language while maintaining the same core functionality.

## Technical Implementation Details

### Parsing Strategy

All calculators implement a similar parsing strategy with different syntax based on their language:

1. **Tokenization**: Convert the input string into tokens (numbers, identifiers, operators)
2. **Recursive Descent Parsing**: Parse expressions according to operator precedence
   - Parse assignment expressions
   - Parse addition/subtraction expressions
   - Parse multiplication/division expressions
   - Parse exponentiation expressions
   - Parse primary expressions (numbers, variables, parenthesized expressions)

### Error Handling

Each implementation handles errors differently:

- **Ada**: Uses Ada's exception mechanism
- **Perl**: Returns undefined values and prints error messages
- **Prolog**: Uses Prolog's failure mechanism and prints error messages
- **Rust**: Uses Rust's Result and Option types for error propagation
- **Scheme**: Uses Scheme's exception handling with with-handlers

### Variable Management

Variables are managed differently in each implementation:

- **Ada**: Uses an associative array with string keys and float values
- **Perl**: Uses a hash with string keys and numeric values
- **Prolog**: Uses dynamic predicates to store variable bindings
- **Rust**: Uses a HashMap with String keys and f64 values
- **Scheme**: Uses an association list with string keys and numeric values

## Challenges and Solutions

### Parentheses Handling

Handling nested parentheses was a challenge in all implementations:

- **Ada**: Uses a recursive descent parser to handle nested parentheses
- **Perl**: Uses a custom split_with_parentheses function to preserve parenthesized expressions
- **Prolog**: Uses pattern matching to identify and evaluate parenthesized expressions
- **Rust**: Uses a token-based approach to handle nested parentheses
- **Scheme**: Uses recursive functions to handle nested parentheses

### Operator Precedence

Ensuring correct operator precedence was implemented through:

- Separate parsing functions for different precedence levels
- Recursive calls to handle higher precedence operations first
- Proper handling of parentheses to override default precedence