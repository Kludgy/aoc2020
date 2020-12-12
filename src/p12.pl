:- use_module(library(dcg/basics)).
:- initialization load_db.

load_db :-
    retractall(db(_)),
    phrase_from_file(commands(A), "p12.txt"),
    !,
    assertz(db(A)).

command(Instr-Val) --> string(I), { length(I, 1), atom_codes(Instr, I) }, integer(Val), blanks_to_nl.

commands([]) --> eos.
commands([C|Commands]) --> command(C), commands(Commands).

% step(Command, Now, Next).
step('N'-N, [X,Y,A], [X,Y1,A]) :- Y1 is Y + N.
step('S'-N, [X,Y,A], [X,Y1,A]) :- Y1 is Y - N.
step('E'-N, [X,Y,A], [X1,Y,A]) :- X1 is X + N.
step('W'-N, [X,Y,A], [X1,Y,A]) :- X1 is X - N.
step('L'-N, [X,Y,A], [X,Y,A1]) :- A1 is (A + N) rem 360.
step('R'-N, [X,Y,A], [X,Y,A1]) :- A1 is (A + 360 - N) rem 360.
step('F'-N, [X,Y,0], B) :- step('E'-N, [X,Y,0], B).
step('F'-N, [X,Y,90], B) :- step('N'-N, [X,Y,90], B).
step('F'-N, [X,Y,180], B) :- step('W'-N, [X, Y,180], B).
step('F'-N, [X,Y,270], B) :- step('S'-N, [X,Y,270], B).

manhattan([X,Y], C) :- C is abs(X) + abs(Y).

sol1(N) :- db(Commands), foldl(step, Commands, [0,0,0], [X,Y,_]), manhattan([X,Y], N), !.
% sol1(N), N = 381.

% wp_step(Command, Now, Next).

% NSEWLR only affect the waypoint
wp_step('N'-N, [X,Y]-S, [X,Y1]-S) :- Y1 is Y + N.
wp_step('S'-N, [X,Y]-S, [X,Y1]-S) :- Y1 is Y - N.
wp_step('E'-N, [X,Y]-S, [X1,Y]-S) :- X1 is X + N.
wp_step('W'-N, [X,Y]-S, [X1,Y]-S) :- X1 is X - N.
wp_step('L'-0, A, A).
wp_step('L'-90, [X,Y]-S, [X1,Y1]-S) :- X1 is -Y, Y1 is X.
wp_step('L'-180, [X,Y]-S, [X1,Y1]-S) :- X1 is -X, Y1 is -Y.
wp_step('L'-270, [X,Y]-S, [X1,Y1]-S) :- X1 is Y, Y1 is -X.
wp_step('R'-0, A, A).
wp_step('R'-90, A, B) :- wp_step('L'-270, A, B).
wp_step('R'-180, A, B) :- wp_step('L'-180, A, B).
wp_step('R'-270, A, B) :- wp_step('L'-90, A, B).

% F only affects the ship
wp_step('F'-N, [X,Y]-[SX,SY], [X,Y]-[SX1,SY1]) :-
    SX1 is SX + X*N,
    SY1 is SY + Y*N.

sol2(N) :- db(Commands), foldl(wp_step, Commands, [10,1]-[0,0], _-FinalPos), manhattan(FinalPos, N), !.
% sol2(N), N = 28591.
