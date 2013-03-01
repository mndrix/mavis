:- use_module(mavis).
:- mavis:activate(user).

%% stuff(+Greeting:integer, +X:atom) is semidet.
stuff(Greeting, Name) :-
    format('~w, ~w~n', [Greeting,Name]).
