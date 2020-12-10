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

freqs(List, Freqs) :-
    msort(List, List1),                     % msort/2 is like sort/2 except is doesn't eliminate duplicates
    pairs_keys_values(Pairs0, List1, _),
    group_pairs_by_key(Pairs0, Pairs1),     % cheap way to get groups of the same digits partitioned
    maplist(value_length, Pairs1, Freqs).   % and count the number of elements in each group

prod_list([N], N).
prod_list([X|Xs], N) :-
    prod_list(Xs, N0),
    N is X * N0.

sol1(N) :-
    path(P),
    diffs(P, D),
    freqs(D, F),
    pairs_values(F, V),
    prod_list(V, N),
    !.
% sol1(N), N=2368.

% Part 2 is going to require an enumeration of gaps placed in the path.
% One strategy is to focus on the problem of what it means to remove one
% element, then two elements, etc. until we get to a number of elements
% that exceeds the number that can be removed.
%
% This still sounds like it will be far too many.
%
% It is given that we cannot remove the first or last element of P in
% path(P).
%
% We also know that we cannot remove any element whose neighbours'
% fused gaps would exceed 3.
