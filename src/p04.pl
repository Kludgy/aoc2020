:- use_module(library(dcg/basics)).

%-----------------------------------------------------------------------------
% Database db(P) of passports P, where each passport is a list of
% key-value pairs kv(Key, Value) where Key and Value are tokens.
%-----------------------------------------------------------------------------

token(T) --> string_without("\r\n\t :", T).

field(kv(Key, Value)) --> token(Key0), { atom_codes(Key, Key0) }, ":", token(Value).

fields([F]) --> field(F), blanks_to_nl, blanks_to_nl.
fields([F|Fields]) --> field(F), blanks, fields(Fields).

passports([]) --> eos.
passports([P|Passports]) --> fields(P), passports(Passports).

db(Passports) :- phrase_from_file(passports(Passports), "p04.txt"), !.

%-----------------------------------------------------------------------------
% Validation queries to bring more meaning to the db tokens.
%-----------------------------------------------------------------------------

% required passport keys
required_key(byr).
required_key(iyr).
required_key(eyr).
required_key(hgt).
required_key(hcl).
required_key(ecl).
required_key(pid).

required_keys(Keys) :- setof(K, required_key(K), Keys).

eye_color(amb).
eye_color(blu).
eye_color(brn).
eye_color(gry).
eye_color(grn).
eye_color(hzl).
eye_color(oth).

hex6 --> "#", xdigits(D), { length(D, 6) }.

units_amount(Units, Amount) --> integer(Amount), string(C), { atom_codes(Units, C) }.

hgt_units_amount(in, X) :- X >= 59, X =< 76.
hgt_units_amount(cm, X) :- X >= 150, X =< 193.

% field validation
valid_field(kv(byr, X)) :- phrase(number(Year), X), Year >= 1920, Year =< 2002.
valid_field(kv(iyr, X)) :- phrase(number(Year), X), Year >= 2010, Year =< 2020.
valid_field(kv(eyr, X)) :- phrase(number(Year), X), Year >= 2020, Year =< 2030.
valid_field(kv(hgt, X)) :- phrase(units_amount(U, A), X), hgt_units_amount(U, A).
valid_field(kv(hcl, X)) :- phrase(hex6, X).
valid_field(kv(ecl, X)) :- atom_codes(C, X), eye_color(C).
valid_field(kv(pid, X)) :- length(X, 9), number_codes(N, X), integer(N), N >= 0.
valid_field(kv(cid, _)).

% set of keys for passport fields
passport_keys_([], []).
passport_keys_([kv(K,_)|Fields], [K|Keys]) :- passport_keys(Fields, Keys).
passport_keys(Passport, Keys) :- passport_keys_(Passport, K), list_to_ord_set(K, Keys).

% required-key validation
valid_passport_keys(P) :-
    passport_keys(P, K),
    required_keys(Kreq),
    intersection(K, Kreq, Kreq).

% full key and field validation
valid_passport(P) :-
    valid_passport_keys(P),
    maplist(valid_field, P).

%-----------------------------------------------------------------------------
% Solutions
%-----------------------------------------------------------------------------

valid_count(ValidTempl, Count) :-
    db(P),
    include(ValidTempl, P, V),
    length(V, Count).

sol1(N) :- valid_count(valid_passport_keys, N).
sol2(N) :- valid_count(valid_passport, N).

% ?- sol1(N).
% N = 196.
% 
% ?- sol2(N).
% N = 114.
