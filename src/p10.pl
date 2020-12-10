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

% I think I convinced myself that we must have to sort the input,
% insert 0 at the beginning and the device at the end. Oooof.

path(P) :-
    db(A),
    device_joltage(X),
    A2 = [0,X|A],
    sort(A2, P).

diffs_([], [_|_], []).
diffs_([_|_], [], []).
diffs_([L1|List1], [L2|List2], [Diff|Diffs]) :-
    Diff is L2 - L1,
    diffs_(List1, List2, Diffs).

diffs([L|List], Diffs) :-
    diffs_([L|List], List, Diffs).

value_length(K-Values, K-Len) :- length(Values, Len).

frequencies(List, Freqs) :-
    msort(List, List1),
    pairs_keys_values(Pairs0, List1, _),
    group_pairs_by_key(Pairs0, Pairs1),
    maplist(value_length, Pairs1, Freqs).

% plan([], Input, _) :-
%     % Must satisfy the device
%     device_joltage(A),
%     Amin is Input + 1, A >= Amin,
%     Amax is Input + 3, A =< Amax.
%
% plan(AdapterSet, Input, Path) :-
%     member(A, Adapters),
%     Amin is Input + 1, A >= Amin,
%     Amax is Input + 3, A =< Amax,
%     ord_del_element(AdapterSet, A, FewerAdapters),
%     plan(FewerAdapters, A, [Input|Path]).
