:- use_module(library(dcg/basics)).

group([]) --> blanks_to_nl.
group([P|Group]) --> string(P), blanks_to_nl, group(Group).

survey([]) --> eos.
survey([G|Groups]) --> group(G), survey(Groups).

db(G) :- phrase_from_file(survey(G), "p06.txt"), !.

group_union(Group, Merged) :- maplist(list_to_ord_set, Group, S), ord_union(S, Merged).
group_intersection(Group, Merged) :- maplist(list_to_ord_set, Group, S), ord_intersection(S, Merged).

merged_count(MergeTempl, Total) :-
    db(G),
    maplist(MergeTempl, G, M),
    maplist(length, M, N),
    sum_list(N, Total).

sol1(N) :- merged_count(group_union, N). % 6542
sol2(N) :- merged_count(group_intersection, N). % 3299
