:- use_module(library(dcg/basics)).

adapters([]) --> eos.
adapters([X|Xs]) --> blanks, integer(X), blanks, adapters(Xs).

:- initialization load_db.

load_db :-
    phrase_from_file(adapters(A), "p10.txt"),
    !,
    assertz(db(A)).


% input can be 1-3 jolts less than adapter joltage rating
% device has a joltage 3 greater than rating of max adapter
% charging outlet has effective joltage of 0

device_joltage(X) :- db(A), max_list(A, X0), X is X0 + 3.

plan([], Input, _) :-
    % Must satisfy the device
    device_joltage(A),
    Amin is Input + 1, A >= Amin,
    Amax is Input + 3, A =< Amax.

plan(Adapters, Input, Path) :-
    member(A, Adapters),
    Amin is Input + 1, A >= Amin,
    Amax is Input + 3, A =< Amax,
    delete(Adapters, A, FewerAdapters),
    plan(FewerAdapters, A, [Input|Path]).
