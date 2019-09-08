:- module(solver, [equation/4]).

possibleCount(X) :-
    between(1, 15, X).

appendNum([], []).
appendNum([H | T], R) :-
    appendNum(T, R1),
    possibleCount(X),
    R = [m(H, X) | R1].

multi([], C, []).
multi([H | T], C, R) :-
    multi(T, C, R1),
    atomHelper(H, C, R2),
    append(R2, R1, R).

atomHelper(m(M, MC), C, R) :-
    sumForAtoms(m(M, MC), R1),
    multi(R1, C, R).

atomHelper(a(A, AC), C, R) :-
    S is AC * C,
    R = [a(A, S)].

sumForAtoms(m([], C), []).
sumForAtoms(m(M, C), R) :-
    M = [A | T],
    sumForAtoms(m(T, C), R1),
    atomHelper(A, C, R2),
    append(R2, R1, R).

sumForMolecules([], []).
sumForMolecules([m(M, C) | T], R) :-
    sumForAtoms(m(M, C), RA),
    sumForMolecules(T, R1),
    R = [RA | R1].

replaceValue([], a(A, C), []).
replaceValue([a(EA, EC) | T], a(A, C), R) :-
    \+ EA = A,
    replaceValue(T, a(A, C), R1),
    R = [a(EA, EC) | R1].
replaceValue([a(A, EC) | T], a(A, C), R) :-
    R = [a(A, C) | T].

sumAtoms([], []).
sumAtoms([H | T], R) :-
    sumAtoms(T, R1),
    a(A, C) = H,
    (member(a(A, X), R1) ->
        S is X + C,
        replaceValue(R1, a(A, S), R)
    ; R = [H | R1]).

equation(Left, Right, BalancedLeft, BalancedRight) :-
    appendNum(Left, BalancedLeft),
    appendNum(Right, BalancedRight),
    sumForMolecules(BalancedLeft, ILeft),
    sumForMolecules(BalancedRight, IRight),
    flatten(ILeft, ILeft3),
    flatten(IRight, IRight3),
    sumAtoms(ILeft3, ILeft4),
    sumAtoms(IRight3, IRight4),
    list_to_set(ILeft4, ILeft5),
    list_to_set(IRight4, IRight5),
    subtract(ILeft5, IRight5, []),
    subtract(IRight5, ILeft5, []).
