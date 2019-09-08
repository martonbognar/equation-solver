:- module(main, []).

:- use_module(parser).
:- use_module(solver).
:- use_module(printer).

go :- 
    parser:parse("MoO3+(C4H9)4NOH=((C4H9)4N)4Mo8O26+H2O", e(L, R)),
    solver:balanced(L, R, BL, BR),
    printer:printBalanced(BL, BR).
