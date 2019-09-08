:- module(solver, [balanced/4]).

balanced(Left, Right, BalancedLeft, BalancedRight) :-
    assignMultiplier(Left, BalancedLeft),
    assignMultiplier(Right, BalancedRight),
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

% convert a list of molecules with no multipliers to a list of multiplied molecules
% [M] -> [m(M, C)] where M is the molecule, C is the multiplier
assignMultiplier([], []).
assignMultiplier([H | T], R) :-
    assignMultiplier(T, R1),
    possibleCount(X),
    R = [m(H, X) | R1].

% the possible values for the multipliers in front of the participating molecules
possibleCount(X) :-
    between(1, 15, X).

sumForMolecules([], []).
sumForMolecules([m(M, C) | T], R) :-
    sumForComponents(M, C, RA),
    sumForMolecules(T, R1),
    R = [RA | R1].

sumForComponents([], _, []).
sumForComponents(M, C, R) :-
    M = [A | T],
    sumForComponents(T, C, R1),
    componentHelper(A, C, R2),
    append(R2, R1, R).

componentHelper(c(M, MC), C, R) :-
    sumForComponents(M, MC, R1),
    multi(R1, C, R).
componentHelper(a(A, AC), C, R) :-
    S is AC * C,
    R = [a(A, S)].

multi([], _, []).
multi([H | T], C, R) :-
    multi(T, C, R1),
    componentHelper(H, C, R2),
    append(R2, R1, R).

sumAtoms([], []).
sumAtoms([H | T], R) :-
    sumAtoms(T, R1),
    a(A, C) = H,
    (member(a(A, X), R1) ->
        S is X + C,
        replaceValue(R1, a(A, S), R)
    ; R = [H | R1]).

replaceValue([], a(_, _), []).
replaceValue([a(A, _) | T], a(A, C), R) :-
    !,
    R = [a(A, C) | T].
replaceValue([a(EA, EC) | T], a(A, C), R) :-
    replaceValue(T, a(A, C), R1),
    R = [a(EA, EC) | R1].
