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

pc_acc_step(Prog, Pc0, Acc0, Pc, Acc) :-
    nth0(Pc0, Prog, Op:Val),
    pc_acc_op_val_step(Pc0, Acc0, Op, Val, Pc, Acc).

% Trace the program path until we revisit a pc value in our history.
% The state of the accumulator just before revisiting a pc for the
% first time is LastAcc
looptrace_(_, Pc0, Acc0, History, History, Acc0) :-
    member(Pc0:_, History).
looptrace_(Prog, Pc0, Acc0, History, LastHistory, LastAcc) :-
    \+ member(Pc0:_, History),
    pc_acc_step(Prog, Pc0, Acc0, Pc1, Acc1),
    looptrace_(Prog, Pc1, Acc1, [Pc0:Acc0|History], LastHistory, LastAcc).

looptrace(Prog, History, Acc) :- looptrace_(Prog, 0, 0, [], History, Acc), !.

sol1(N) :- prog(I), !, looptrace(I, _, N).
% sol1(N), N = 2080.

% termtrace_ is true for programs whose program counter finally leaves
% the address domain.
termtrace_(Prog, Pc0, Acc0, Acc0) :-
    \+ pc_acc_step(Prog, Pc0, Acc0, _, _).
termtrace_(Prog, Pc0, Acc0, FinalAcc) :-
    pc_acc_step(Prog, Pc0, Acc0, Pc1, Acc1),
    termtrace_(Prog, Pc1, Acc1, FinalAcc).

termtrace(Prog, FinalAcc) :-
    \+ looptrace(Prog, _, _),         % a terminating program mustn't loop
    termtrace_(Prog, 0, 0, FinalAcc).  % unify the final accumulator value of the terminating program

% The strategy for sol2/1 will be to back-track all unique programs replacing a jmp with nop,
% testing each new program with termtrace until we get an answer.
noptrace(Prog, FinalAcc) :-
    select(jmp:Val, Prog, nop:Val, NewProg), % select/4 backtracks on substitutions until Prog=NewProg, or it exhausts all combinations.
    termtrace(NewProg, FinalAcc).

sol2(N) :- prog(I), !, noptrace(I, N).
% sol2(N), N = 2477.
