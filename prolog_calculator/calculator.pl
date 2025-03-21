% Simple calculator with variables, assignments, and basic arithmetic

% Dynamic predicate to store variables
:- dynamic var_value/2.

% Main predicate
main :-
    writeln('Prolog Calculator'),
    writeln('Enter expressions with variables and assignments (e.g., "x = 5", "y = x + 3")'),
    writeln('Type "exit" to quit'),
    repl.

% REPL (Read-Eval-Print Loop)
repl :-
    write('> '),
    flush_output,
    read_line_to_codes(user_input, Line),
    (   Line == end_of_file
    ->  true
    ;   atom_codes(Input, Line),
        (   Input == "exit"
        ->  true
        ;   process_input(Line),
            repl
        )
    ).

% Process user input
process_input(Line) :-
    (   Line == []
    ->  true  % Skip empty lines
    ;   catch(
            (   parse_and_evaluate(Line, Result),
                format('~w~n', [Result])
            ),
            Error,
            handle_error(Error)
        )
    ).

% Parse and evaluate expressions
parse_and_evaluate(Line, Result) :-
    tokenize(Line, Tokens),
    parse_expression(Tokens, Result).

% Tokenize input string into a list of tokens
tokenize(Line, Tokens) :-
    tokenize_chars(Line, Tokens).

% Tokenize characters
tokenize_chars([], []).
tokenize_chars([C|Cs], Tokens) :-
    (   code_type(C, space)
    ->  tokenize_chars(Cs, Tokens)
    ;   tokenize_token(C, Cs, Token, Rest),
        tokenize_chars(Rest, RestTokens),
        Tokens = [Token|RestTokens]
    ).

% Tokenize a single token
tokenize_token(C, Cs, Token, Rest) :-
    (   code_type(C, digit)
    ->  tokenize_number([C|Cs], NumChars, Rest),
        number_codes(Token, NumChars)
    ;   code_type(C, alpha)
    ->  tokenize_identifier([C|Cs], IdChars, Rest),
        atom_codes(Token, IdChars)
    ;   operator_char(C)
    ->  atom_chars(Token, [C]),
        Rest = Cs
    ;   Token = C,  % Other characters as is
        Rest = Cs
    ).

% Tokenize a number
tokenize_number([], [], []).
tokenize_number([C|Cs], [C|NumChars], Rest) :-
    code_type(C, digit),
    !,
    tokenize_number(Cs, NumChars, Rest).
tokenize_number(Cs, [], Cs).

% Tokenize an identifier
tokenize_identifier([], [], []).
tokenize_identifier([C|Cs], [C|IdChars], Rest) :-
    (code_type(C, alnum) ; C = 0'_),
    !,
    tokenize_identifier(Cs, IdChars, Rest).
tokenize_identifier(Cs, [], Cs).

% Operator characters
operator_char(0'+).  % +
operator_char(0'-).  % -
operator_char(0'*).  % *
operator_char(0'/).  % /
operator_char(0'^).  % ^
operator_char(0'=).  % =
operator_char(0'().  % (
operator_char(0')).  % )

% Parse expression
parse_expression([Var, '='|Rest], Result) :-
    atom(Var),  % Variable name
    parse_expr(Rest, Value),
    retractall(var_value(Var, _)),
    assertz(var_value(Var, Value)),
    Result = Value.
parse_expression(Tokens, Result) :-
    parse_expr(Tokens, Result).

% Parse arithmetic expressions with operator precedence
parse_expr(Tokens, Result) :-
    parse_term(Tokens, Left, Rest),
    parse_expr_rest(Left, Rest, Result).

parse_expr_rest(Left, ['+' | Rest], Result) :-
    parse_term(Rest, Right, NewRest),
    Sum is Left + Right,
    parse_expr_rest(Sum, NewRest, Result).
parse_expr_rest(Left, ['-' | Rest], Result) :-
    parse_term(Rest, Right, NewRest),
    Diff is Left - Right,
    parse_expr_rest(Diff, NewRest, Result).
parse_expr_rest(Result, [], Result).
parse_expr_rest(Result, [')' | Rest], Result) :-  % Handle closing parenthesis
    Rest = [].  % Ensure no tokens after closing parenthesis

% Parse term (multiplication and division)
parse_term(Tokens, Result, Rest) :-
    parse_factor(Tokens, Left, TempRest),
    parse_term_rest(Left, TempRest, Result, Rest).

parse_term_rest(Left, ['*' | Rest], Result, FinalRest) :-
    parse_factor(Rest, Right, NewRest),
    Product is Left * Right,
    parse_term_rest(Product, NewRest, Result, FinalRest).
parse_term_rest(Left, ['/' | Rest], Result, FinalRest) :-
    parse_factor(Rest, Right, NewRest),
    (   Right =:= 0
    ->  throw(error(division_by_zero, _))
    ;   Quotient is Left / Right,
        parse_term_rest(Quotient, NewRest, Result, FinalRest)
    ).
parse_term_rest(Result, Rest, Result, Rest).

% Parse factor (exponentiation)
parse_factor(Tokens, Result, Rest) :-
    parse_primary(Tokens, Left, TempRest),
    parse_factor_rest(Left, TempRest, Result, Rest).

parse_factor_rest(Left, ['^' | Rest], Result, FinalRest) :-
    parse_primary(Rest, Right, NewRest),
    Power is Left ** Right,
    parse_factor_rest(Power, NewRest, Result, FinalRest).
parse_factor_rest(Result, Rest, Result, Rest).

% Parse primary expressions (numbers, variables, parenthesized expressions)
parse_primary([Number | Rest], Number, Rest) :-
    number(Number),
    !.
parse_primary([Var | Rest], Value, Rest) :-
    atom(Var),
    !,
    (   var_value(Var, Value)
    ->  true
    ;   throw(error(undefined_variable(Var), _))
    ).
parse_primary(['(' | Rest], Result, [')' | FinalRest]) :-
    parse_expr(Rest, Result),
    !,
    FinalRest = [].
parse_primary(['-' | Rest], Result, FinalRest) :-
    parse_primary(Rest, Value, FinalRest),
    Result is -Value.
parse_primary(Tokens, _, _) :-
    throw(error(syntax_error(_), _)).

% Error handling
handle_error(error(division_by_zero, _)) :-
    writeln('Error: Division by zero').
handle_error(error(undefined_variable(Var), _)) :-
    format('Error: Undefined variable ~w~n', [Var]).
handle_error(error(syntax_error(_), _)) :-
    writeln('Error: Syntax error in expression').
handle_error(Error) :-
    format('Error: ~w~n', [Error]).

% Initialize
:- initialization(main).
