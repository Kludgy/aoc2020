:- use_module(library(dcg/basics)).

inner_bag(Count-Style) -->
    integer(Count),
    blanks, string(Style0), { atom_codes(Style, Style0) },
    blanks, ("bag" ; "bags").

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
