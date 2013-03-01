:- use_module(mavis).
:- use_module(library(dcg/basics), [string//1]).
:- mavis:activate(user).

%% stuff(+Greeting:integer, +X:atom) is semidet.
stuff(Greeting, Name) :-
    format('~w, ~w~n', [Greeting,Name]).

%%  grammar(-Name:codes)//
grammar(Name) -->
    "hello ",
    string(Name).
