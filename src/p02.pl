:- use_module(library(dcg/basics)).

line(Min,Max,Letter,Pass) --> number(Min), "-", number(Max), " ", [Letter], ": ", string(Pass), blanks_to_nl.

lines([line(Min,Max,Letter,Pass)|Lines]) --> line(Min,Max,Letter,Pass), lines(Lines).
lines([]) --> blanks.

db(Lines) :-
    phrase_from_file(lines(Lines), 'p02.txt').

min_max_letter_pass(line(Min, Max, Letter, Pass)) :-
    findall(_, member(Letter, Pass), Bag),
    length(Bag, Freq),
    Freq >= Min,
    Freq =< Max.

pos_pos_letter_pass(line(Pos1, Pos2, Letter, Pass)) :-    nth1(Pos1, Pass, Letter), \+ nth1(Pos2, Pass, Letter).
pos_pos_letter_pass(line(Pos1, Pos2, Letter, Pass)) :- \+ nth1(Pos1, Pass, Letter),    nth1(Pos2, Pass, Letter).

valid_count(ValidTempl, Count) :-
    db(L),
    include(ValidTempl, L, V),
    length(V, Count),
    !.

sol1(N) :- valid_count(min_max_letter_pass, N).
sol2(N) :- valid_count(pos_pos_letter_pass, N).

% ?- sol1(N).
% N = 483.
%
% ?- sol2(N).
% N = 482.
