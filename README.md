# A chemical equation parser and solver

Currently a work in progress.

## Usage

1. You need to have [SWI-Prolog](https://www.swi-prolog.org/download/stable) installed on your system (you can use other Prolog distributions at your own risk).
2. Clone this repository or download the files to a directory on your computer.
3. Start `swipl` from this directory, you will be able to use the predicates provided by the modules in this interpreter.

## Example

```prolog
?- use_module(parser).
true.

?- use_module(solver).
true.

?- use_module(printer).
true.

?- parser:parse("H2O+CO2=C6H12O6+O2", e(L, R)), solver:balanced(L, R, BL, BR), printBalanced(BL, BR).
6 H2O + 6 CO2 = C6H12O6 + 6 O2
L = [[a("H", 2), a("O", 1)], [a("C", 1), a("O", 2)]],
R = [[a("C", 6), a("H", 12), a("O", 6)], [a("O", 2)]],
BL = [m([a("H", 2), a("O", 1)], 6), m([a("C", 1), a("O", 2)], 6)],
BR = [m([a("C", 6), a("H", 12), a("O", 6)], 1), m([a("O", 2)], 6)] .
```
