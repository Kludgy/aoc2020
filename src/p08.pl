:- use_module(library(dcg/basics)).

op_val(Op:Val) -->
    string(Op0),
    { length(Op0, 3), atom_codes(Op, Op0) },
    blanks,
    integer(Val),
    blanks_to_nl.

instructions([]) --> blanks_to_nl.
instructions([I|Instructions]) --> op_val(I), instructions(Instructions).

:- initialization load_prog.

load_prog :-
    phrase_from_file(instructions(R), "p08.txt"),
    !,
    assertz(prog(R)).

% Pc: program counter
% Acc: accumulator
pc_acc_op_val_step(Pc0, Acc0, acc, Val, Pc, Acc ) :- Pc is Pc0 + 1,    Acc is Acc0 + Val.
pc_acc_op_val_step(Pc0, Acc0, jmp, Val, Pc, Acc0) :- Pc is Pc0 + Val.
pc_acc_op_val_step(Pc0, Acc0, nop, _,   Pc, Acc0) :- Pc is Pc0 + 1.

pc_acc_step(Pc0, Acc0, Pc, Acc) :-
    prog(I),
    nth0(Pc0, I, Op:Val),
    pc_acc_op_val_step(Pc0, Acc0, Op, Val, Pc, Acc).
