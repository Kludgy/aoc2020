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

% Part 2 Solution Discussion
% --------------------------
%
% I ended up solving part 2 by hand first because it was actually quicker than writing the code by
% the time I'd figured out what was happening.
%
% If we look at any supplied sequence for this entire day's problems we notice there are never any
% differences of 2. All steps are either of 1 or 3. This makes the solution much easier to find, as
% we will see below.
%
% Further, if we take a look at any example, or the problem input, runs with steps of 1 are always
% very small (only 2, 3, 4, or 5 of which there are then only 0, 1, 2 or 3 elements that can possibly
% be removed in the runs). This is small enough that we can write out the combinations:
%
%   (0),(1)        X can't remove any here
%   (0),1,(2)      <-- 2 permutations, because we can remove nothing or 1.
%   (0),1,2,(3)    <-- 4 permutations, because we can either remove nothing, 1, 2 or both.
%   (0),1,2,3,(4)  <-- 7 permutations, because only 7 of 8 combos are valid (removing 1&2&3 would leave a gap >3)
%
% So, taking all such runs and multiplying the permutations together gives the requested solution,
%
%   7*4*7*2*7*4*7*2*4*2*4*4*2*2*4*4*7*7*7*4 = 1727094849536
%
% Doing the first example from part 2:
%
%   0,1,4,5,6,7,10,11,12,15,16,19,22
%
% Start by marking all the numbers that could possibly be removed:
%
%   0,1,4,5,6,7,10,11,12,15,16,19,22
%         x x       x
%          4   *    2   =   8
%    1 3 1 1 1 3  1  1  3  1  3  3


sol2(N) :- N is 7*4*7* 2*7*4* 7*2*4* 2*4*4* 2*2*4* 4*7*7* 7*4.
% sol2(N), N = 1727094849536.


% diffrun_perms(DiffRunLen, NumPermutations).
%
% The correspondence between lengths of *1-difference-runs* of length DiffRunLen
% and the number of valid permutations we can get by removing elements these
% runs correspond to.
%
% Ex.
%
%  Original sequence:  1,4,5,6,7,10,12,15
%  The only 1-run in the sequence:  4,5,6,7
%  The only 1-difference-run:  (5-4),(6-5),(7-6) = 1,1,1.
%
% This 1-difference-run as a length of 3, so the corresponding
% permutation count P is given by diffrun(3,P).
%
diffrun_perms(0, 0).
diffrun_perms(1, 0).
diffrun_perms(2, 2).
diffrun_perms(3, 4).
diffrun_perms(4, 7).
