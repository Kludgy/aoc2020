:- use_module(library(dcg/basics)).

adapters([]) --> eos.
adapters([X|Xs]) --> blanks, integer(X), blanks, adapters(Xs).

:- initialization load_db.

load_db :-
    phrase_from_file(adapters(A), "p10.txt"),
    !,
    assertz(db(A)).
