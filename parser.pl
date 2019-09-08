:- module(parser, [parse/2]).

% parsing an entire equation in the form: X+Y=U+V
equation(e(L, R)) --> plusParts(L), ['='], plusParts(R).

% parsing one side of the equation in the form: X+Y
plusParts([H | T]) --> elementList(H), ['+'], !, plusParts(T).
plusParts([T])     --> elementList(T).

% parsing one molecule
elementList([H | T]) --> (subgroup(H) | elementWithNum(H)), elementList(T).
elementList([])      --> [].

% parsing a subgroup in a molecule, such as (CH3)2 in (CH3)2CHOH
subgroup(c(S, C)) --> ['('], elementList(S), [')'], nr(C).

% parsing a single element in a molecule, such as H2 in H2O
elementWithNum(a(A, N)) --> element(AL), nr(N), { string_chars(A, AL) }.

% parsing the symbol of a single element (first character + following characters)
element([A | T]) --> [A], { char_type(A, upper) }, restElement(T).

% parsing the tail characters of a symbol
restElement([A | T]) --> [A], { char_type(A, lower) }, !, restElement(T).
restElement([])      --> [].

% parsing a number
nr(N1) --> (number_(N) | number(N)), { number_chars(N1, N) }.

% parsing a compound number (made up of consequent digits, such as 15)
number_([D | Ds]) --> digit(D), number_(Ds).
number_([D])      --> digit(D).

% parsing a single digit
digit(D) --> [D], { char_type(D, digit) }.

% parsing a missing number, such as the implicit 1 in H2O(1)
number(['1']), [T] --> [T], !.
number(['1'])      --> [].

removeSpaces([], []).
removeSpaces([' ' | T], Out) :-
    !,
    removeSpaces(T, Out).
removeSpaces([H | T], Out) :-
    removeSpaces(T, OT),
    Out = [H | OT].

parse(String, R) :-
    string_chars(String, In),
    removeSpaces(In, Stripped),
    phrase(equation(R), Stripped).
