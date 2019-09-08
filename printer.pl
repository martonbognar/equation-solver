:- module(printer, [printBalanced/2]).

printBalanced(L, R) :-
    printSide(L),
    write(' = '),
    printSide(R).

printSide([H]) :-
    !,
    printMolecule(H).
printSide([H | T]) :-
    printMolecule(H),
    write(' + '),
    printSide(T).

printMolecule(m(M, 1)) :-
    !,
    printElements(M).
printMolecule(m(M, C)) :-
    write(C),
    write(' '),
    printElements(M).

printElements([]).
printElements([a(A, 1) | T]) :-
    !,
    write(A),
    printElements(T).
printElements([a(A, C) | T]) :-
    write(A),
    write(C),
    printElements(T).
printElements([c(M, 1) | T]) :-
    !,
    write('('),
    printElements(M),
    write(')'),
    printElements(T).
printElements([c(M, C) | T]) :-
    write('('),
    printElements(M),
    write(')'),
    write(C),
    printElements(T).
