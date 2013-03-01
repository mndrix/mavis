:- use_module(mavis).
:- mavis:activate(user).

%% stuff(X:atom) is semidet.
stuff(Name) :-
    format('hello ~w~n', Name).
