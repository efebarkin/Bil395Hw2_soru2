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
    read_line_to_string(user_input, Input),
    (   Input == end_of_file
    ->  true
    ;   Input == "exit"
    ->  true
    ;   process_input(Input),
        repl
    ).

% Process user input
process_input(Input) :-
    (   Input == ""
    ->  true  % Skip empty lines
    ;   catch(
            (   tokenize_input(Input, Tokens),
                evaluate_tokens(Tokens, Result),
                format('~w~n', [Result])
            ),
            Error,
            handle_error(Error)
        )
    ).

% Tokenize input string
tokenize_input(Input, Tokens) :-
    % Add spaces around operators and parentheses for easier tokenization
    add_spaces_around_operators(Input, SpacedInput),
    % Split by whitespace
    split_string(SpacedInput, " \t\n\r", " \t\n\r", RawTokens),
    % Remove empty tokens
    delete(RawTokens, "", Tokens).

% Add spaces around operators and parentheses
add_spaces_around_operators(Input, Output) :-
    % Replace operators with spaced versions
    replace_all(Input, "+", " + ", T1),
    replace_all(T1, "-", " - ", T2),
    replace_all(T2, "*", " * ", T3),
    replace_all(T3, "/", " / ", T4),
    replace_all(T4, "^", " ^ ", T5),
    replace_all(T5, "=", " = ", T6),
    replace_all(T6, "(", " ( ", T7),
    replace_all(T7, ")", " ) ", Output).

% Replace all occurrences of a substring
replace_all(String, Old, New, Result) :-
    split_string(String, Old, "", Parts),
    atomics_to_string(Parts, New, Result).

% Evaluate tokens
evaluate_tokens(Tokens, Result) :-
    % Convert string tokens to terms
    maplist(convert_token, Tokens, Terms),
    % Parse and evaluate
    parse_expression(Terms, Result, []).

% Convert a token to its appropriate term
convert_token(Token, Term) :-
    (   % Try to convert to number
        number_string(Number, Token)
    ->  Term = Number
    ;   % Check if it's an operator
        member(Token, ["+", "-", "*", "/", "^", "=", "(", ")"])
    ->  atom_string(Term, Token)
    ;   % Otherwise it's a variable
        atom_string(Term, Token)
    ).

% Parse expression
parse_expression([Var, '='|Rest], Result, []) :-
    atom(Var),  % Variable name
    parse_expr(Rest, Value, []),
    retractall(var_value(Var, _)),
    assertz(var_value(Var, Value)),
    Result = Value.
parse_expression(Tokens, Result, Rest) :-
    parse_expr(Tokens, Result, Rest).

% Parse arithmetic expressions with operator precedence
parse_expr(Tokens, Result, Rest) :-
    parse_term(Tokens, Left, TempRest),
    parse_expr_rest(Left, TempRest, Result, Rest).

parse_expr_rest(Left, ['+'|Rest], Result, FinalRest) :-
    parse_term(Rest, Right, TempRest),
    Sum is Left + Right,
    parse_expr_rest(Sum, TempRest, Result, FinalRest).
parse_expr_rest(Left, ['-'|Rest], Result, FinalRest) :-
    parse_term(Rest, Right, TempRest),
    Diff is Left - Right,
    parse_expr_rest(Diff, TempRest, Result, FinalRest).
parse_expr_rest(Result, Rest, Result, Rest).

% Parse term (multiplication and division)
parse_term(Tokens, Result, Rest) :-
    parse_factor(Tokens, Left, TempRest),
    parse_term_rest(Left, TempRest, Result, Rest).

parse_term_rest(Left, ['*'|Rest], Result, FinalRest) :-
    parse_factor(Rest, Right, TempRest),
    Product is Left * Right,
    parse_term_rest(Product, TempRest, Result, FinalRest).
parse_term_rest(Left, ['/'|Rest], Result, FinalRest) :-
    parse_factor(Rest, Right, TempRest),
    (   Right =:= 0
    ->  throw(error(division_by_zero, _))
    ;   Quotient is Left / Right,
        parse_term_rest(Quotient, TempRest, Result, FinalRest)
    ).
parse_term_rest(Result, Rest, Result, Rest).

% Parse factor (exponentiation)
parse_factor(Tokens, Result, Rest) :-
    parse_primary(Tokens, Left, TempRest),
    parse_factor_rest(Left, TempRest, Result, Rest).

parse_factor_rest(Left, ['^'|Rest], Result, FinalRest) :-
    parse_primary(Rest, Right, TempRest),
    Power is Left ** Right,
    parse_factor_rest(Power, TempRest, Result, FinalRest).
parse_factor_rest(Result, Rest, Result, Rest).

% Parse primary expressions (numbers, variables, parenthesized expressions)
parse_primary([Number|Rest], Number, Rest) :-
    number(Number), !.
parse_primary([Var|Rest], Value, Rest) :-
    atom(Var), !,
    (   var_value(Var, Value)
    ->  true
    ;   throw(error(undefined_variable(Var), _))
    ).
parse_primary(['('|Rest], Result, FinalRest) :-
    !,
    parse_expr(Rest, Result, [')'|NewRest]),
    !,
    FinalRest = NewRest.
parse_primary(['-'|Rest], Result, FinalRest) :-
    !,
    parse_primary(Rest, Value, FinalRest),
    Result is -Value.
parse_primary(Tokens, _, _) :-
    throw(error(syntax_error(Tokens), _)).

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
