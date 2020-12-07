:- use_module(library(dcg/basics)).
:- initialization load_db.

inner_bag(Count-Style) -->
    integer(Count),
    blanks, string(Style0), { atom_codes(Style, Style0) },
    blanks, ("bag" ; "bags").

inner_bags([]) --> blanks, "no other bags.", blanks_to_nl.
inner_bags([]) --> blanks_to_nl.
inner_bags([B|Bags]) -->
    blanks, inner_bag(B), ("," ; "."), inner_bags(Bags).

outer_bag(Style) -->
    string(Style0), { atom_codes(Style, Style0) },
    blanks, "bags".

rule(OuterBag:InnerBags) -->
    outer_bag(OuterBag),
    blanks, "contain",
    blanks, inner_bags(InnerBags).

rules([]) --> eos.
rules([R|Rules]) --> rule(R), rules(Rules).

% Parse the input text once at initialisation.
load_db :-
    phrase_from_file(rules(R), "p07.txt"),
    !,
    assertz(db(R)).

% parsing complete

% Inner bag fields.
innerbag_count_style(Count-Style, Count, Style).

% True when Style is a member of the given rule's InnerBags.
member_inner_style(Style, _:InnerBags) :-
    maplist(innerbag_style, InnerBags, _, InnerStyles),
    member(Style, InnerStyles).

% All Rules whose inner bags directly refer to the given Style.
inner_style_rules(InnerStyle, Rules) :-
    db(AllRules),
    include(member_inner_style(InnerStyle), AllRules, Rules).

% Outer bag style.
rule_outer_style(OuterStyle:_, OuterStyle).

% Fan out to discover all bags indirectly referring to InnerStyle.
inner_style_rules_fanout(InnerStyle, Rules) :-
    inner_style_rules(InnerStyle, R0),
    R0 \== [],
    maplist(rule_outer_style, R0, S0),
    maplist(inner_style_rules_fanout, S0, ParentRules),
    flatten([R0|ParentRules], RulesDups),
    list_to_ord_set(RulesDups, Rules),
    !.
inner_style_rules_fanout(_, []).

sol1(N) :-
    inner_style_rules_fanout('shiny gold', Rules),
    maplist(rule_outer_style, Rules, Styles),
    length(Styles, N).

outerstyle_rule(OuterStyle, OuterStyle:InnerBags) :-
    db(R),
    member(OuterStyle:InnerBags, R),
    !.

rule_innerbags_total(_:[], 0).
rule_innerbags_total(_:InnerBags, Total) :-
    maplist(innerbag_count_style, InnerBags, InnerBagCounts, InnerBagStyles),
    maplist(outerstyle_rule, InnerBagStyles, Rules),
    maplist(rule_innerbags_total, Rules, Subtotals),
    sum_list(Subtotals, Total0),
    sum_list(InnerBagCounts, SumInnerBagCounts),
    Total is SumInnerBagCounts + Total0,
    !.

sol2(N) :-
    outerstyle_rule('shiny gold', R),
    rule_innerbags_total(R, N).
