:- use_module(library(mavis)).

% copied and pasted from pldoc/doc_modes.
% should rework that module so this is unnecessary.
:- op(750, xf, ...).                    % Repeated argument: Arg...
:- op(650, fx, +).                      % allow +Arg
:- op(650, fx, -).                      % allow -Arg
:- op(650, fx, ?).                      % allow ?Arg
:- op(650, fx, :).                      % allow :Arg
:- op(650, fx, @).                      % allow @Arg
:- op(650, fx, !).                      % allow !Arg
:- op(200, xf, //).                     % allow for Head// is det.


%%	nm(+Mode, -Args, -Det).
nm(Mode, Args, Det) :-
    mavis:normalize_mode(Mode, Args0, Det0),
    Args == Args0,
    Det == Det0.

:- use_module(library(tap)).

nm( foo(A:integer)
  , [ arg(?,A,integer)
    ]
  , nondet
  ).

nm( foo(A:integer, +B:atom) is semidet
  , [ arg(?,A,integer)
    , arg(+,B,atom)
    ]
  , semidet
  ).

nm( sum_list(+List, -Sum)
  , [ arg(+, List, any)
    , arg(-, Sum, any)
    ]
  , nondet
  ).

nm( head(+List:list(T), -Head:T) is semidet
  , [ arg(+, List, list(T))
    , arg(-, Head, T)
    ]
  , semidet
  ).

nm( plain(X)
  , [ arg(?, X, any)
    ]
  , nondet
  ).
