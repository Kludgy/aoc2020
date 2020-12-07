:- use_module(library(dcg/basics)).

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

db(R) :- phrase_from_file(rules(R), "p07.txt"), !.

% parsing complete

% Inner bag Style.
innerbag_style(_-Style, Style).

% True when Style is a member of the given rule's InnerBags.
member_inner_style(Style, _:InnerBags) :-
    maplist(innerbag_style, InnerBags, InnerStyles),
    member(Style, InnerStyles).

% All Rules whose inner bags directly refer to the given Style.
inner_style_rules(InnerStyle, Rules) :-
    db(AllRules),
    include(member_inner_style(InnerStyle), AllRules, Rules).

% Outer bag style.
rule_outer_style(OuterStyle:_, OuterStyle).

% Fan out to discover all 
% It's really slow :)
inner_style_rules_fanout(InnerStyle, Rules) :-
    inner_style_rules(InnerStyle, R0),
    length(R0, N),
    N >= 0,
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
