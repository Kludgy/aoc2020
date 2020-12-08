:- use_module(library(dcg/basics)).

op_val(Op:Val) -->
    string(Op0),
    { length(Op0, 3), atom_codes(Op, Op0) },
    blanks,
    integer(Val),
    blanks_to_nl.

instructions([]) --> blanks_to_nl.
instructions([I|Instructions]) --> op_val(I), program(Instructions).

:- initialization load_prog.

load_prog :-
    phrase_from_file(instructions(R), "p08.txt"),
    !,
    assertz(prog(R)).

