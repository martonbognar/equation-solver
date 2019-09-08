:- module(main, []).

:- use_module(parser).
:- use_module(solver).

go :- 
    solver:equation([
        [a("Mo", 1), a("O", 3)],                                                                                                                                  
        [m([a("C", 4), a("H", 9)], 4), a("N", 1), a("O", 1), a("H", 1)]
    ], [        
        [m([m([a("C", 4), a("H", 9)], 4), a("N", 1)], 4), a("Mo", 8), a("O", 26)],         
        [a("H", 2), a("O", 1)]
    ], BL, BR),
    writeln(BL),
    writeln(BR).

