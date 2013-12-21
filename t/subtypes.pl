:- use_module(library(mavis)).

:- use_module(library(tap)).

has_subtype(integer, integer).  % trivial subtype

has_subtype(integer, positive_integer).
has_subtype(integer, nonneg).
has_subtype(integer, between(3,7)).

has_subtype(number, integer).
has_subtype(number, float).
has_subtype(number, oneof([1,2.718,3.1415])).

has_subtype(atomic, atom).

has_subtype(text, atom).
has_subtype(text, string).

% make sure we don't get false positives
not(has_subtype(float, integer)).
not(has_subtype(integer, float)).
not(has_subtype(atom, integer)).
not(has_subtype(list, string)).
