:- module(mavis, [ the/2
                 , has_intersection/2
                 , has_subtype/2
                 , known_type/1
                 ]).


:- use_module(library(quickcheck)).
:- use_module(library(error)).


/** <module> Optional type declarations
Declare optional types which are checked during development time.
See pack documentation for more information.
*/

module_wants_mavis(Module) :-
    Module \= mavis,
    predicate_property(Module:the(_,_), imported_from(mavis)).

%%	the(+Type:type, ?Value) is det.
%
%	Declare that Value has the given Type.
%	Succeeds if Value is bound to a value that's compatible
%	with Type.  Throws an informative exception if Value
%	is bound to a value that's not compatible with Type.
%	If Value is not bound, the type check is delayed until
%	Value becomes ground.
%
%	When optimizations are enabled
%	(=|current_prolog_flag(optimise, true)|=) a macro removes =the=
%	entirely so that it always succeeds.
:- if(current_prolog_flag(optimise,true)).

the(_,_).  % avoid "Exported procedure mavis:the/2 is not defined"
user:goal_expansion(the(_,_), true).

:- else.

:- use_module(library(apply), [exclude/3]).
:- use_module(library(charsio), [read_term_from_chars/3]).
:- use_module(library(list_util), [xfy_list/3]).
:- use_module(library(pldoc)).
:- use_module(library(pldoc/doc_wiki), [indented_lines/3]).
:- use_module(library(when), [when/2]).
:- doc_collect(true).

% extract mode declaration from a structured comment
mode_declaration(Comment, ModeCodes) :-
    string_to_list(Comment, Codes),
    phrase(pldoc_process:structured_comment(Prefixes,_), Codes, _),
    indented_lines(Codes, Prefixes, Lines),
    pldoc_modes:mode_lines(Lines, ModeCodes, [], _).

% read a raw mode declaration from character codes
read_mode_declaration(ModeCodes, Mode) :-
    Options = [module(pldoc_modes), variable_names(Vars)],
    read_term_from_chars(ModeCodes, Mode, Options),
    maplist(call,Vars).

% convert mode declarations to a standard form
normalize_mode(Mode0, Args, Det) :-
    (Mode0 = is(Mode1, Det) -> true; Mode1=Mode0, Det=nondet),
    (Mode1 = //(Mode2) -> Slash='//'; Mode2=Mode1, Slash='/' ),
    _ = Slash, % avoid singleton warnings (until Slash is needed)
    Mode2 =.. [_|RawArgs],
    maplist(normalize_args, RawArgs, Args).

normalize_args(X0, arg(Mode,Name,Type)) :-
    ( var(X0) -> X1 = ?(X0:any) ; X1=X0 ),
    ( X1 =.. [Mode0,Arg] -> true; Mode0='?', Arg=X1 ),
    ( member(Mode0, [+,-,?,:,@,!]) -> Mode=Mode0; Mode='?' ),
    ( nonvar(Arg), Arg=Name:Type -> true; Name=Arg, Type=any).

the(Type, Value) :-
    when(ground(Value), error:must_be(Type, Value)).

% create a the/2 type assertion based on a variable and
% the declared mode information for that variable.
type_declaration(Var, arg(_,_,Type), the(Type, Var)).

% convert a clause head into a goal which asserts all types
% associated with that head.  Slash is '/' for a normal
% predicate and '//' for a DCG.  Pneumonic: foo/1 vs foo//1
build_type_assertions(Slash, Head, TypeGoal) :-
    % does this module want mavis type assertions?
    prolog_load_context(module, Module),
    mavis:module_wants_mavis(Module),

    % fetch this predicate's structured comment
    functor(Head, Name, Arity),
    Indicator =.. [Slash, Name, Arity],
    pldoc_process:doc_comment(Module:Indicator,_,_,Comment),

    % parse and normalize mode description
    mode_declaration(Comment, ModeText),
    %debug(mavis, "~q has modeline `~s`~n", [Module:Indicator, ModeText]),
    read_mode_declaration(ModeText, RawMode),
    normalize_mode(RawMode, ModeArgs, _Determinism),

    Head =.. [Name|HeadArgs],
    maplist(type_declaration, HeadArgs, ModeArgs, AllTypes),
    exclude(=@=(the(any, _)), AllTypes, Types),
    xfy_list(',', TypeGoal, Types).

user:term_expansion((Head:-Body), (Head:-TypeGoal,Body)) :-
    Slash = '/',
    build_type_assertions(Slash, Head, TypeGoal).

user:term_expansion((Head-->Body), (Head-->{TypeGoal},Body)) :-
    Slash = '//',
    build_type_assertions(Slash, Head, TypeGoal).

:- endif.


% below here, code that loads all the time

%% type_subtype(?Type, ?Subtype)
%
%  Multifile predicate for declaring that a Type has a Subtype. It
%  should only be necessary to add clauses to this predicate if
%  has_subtype/2 has trouble deriving this information based on
%  your definition of `quickcheck:arbitrary/2`.
:- dynamic type_subtype/2.
:- multifile type_subtype/2.

%% has_subtype(+Type, +Subtype) is semidet.
%
%  True if all values of Subtype are also values of Type. This can be
%  used to determine whether arguments of one type can be passed to a
%  predicate which demands arguments of another type.
%
%  This predicate performs probabilistic subtype detection by leveraging
%  your definitions for `error:has_type/2` and `quickcheck:arbitrary/2`.
%  If this predicate is not detecting your types correctly, either
%  improve your quickcheck:arbitrary/2 definition or add clauses to
%  the multifile predicate type_subtype/2.
has_subtype(Type, Subtype) :-
    ( var(Type); var(Subtype) ),
    !,
    fail.
has_subtype(Type, Subtype) :-
    type_subtype(Type, Subtype),
    !.
has_subtype(Type, Subtype) :-
    error:must_be(nonvar, Type),
    error:must_be(arbitrary_type, Subtype),
    \+ counter_example(Type, Subtype, _),
    assert(type_subtype(Type, Subtype)).

% find a value (Example) which belongs to Subtype but not
% to Type.  This demonstrates that Subtype is not a strict
% subset of Type.
counter_example(Type, Subtype, Example) :-
    between(1,100,_),
    quickcheck:arbitrary(Subtype, Example),
    \+ error:is_of_type(Type, Example),
    !.


%% type_intersection(?Type, ?IntersectionType)
%
%  Multifile predicate for declaring that Type has an IntersectionType.
%  See type_subtype/2 for further details.
:- dynamic type_intersection/2.
:- multifile type_intersection/2.


%% has_intersection(Type, IntersectionType) is semidet
%
%  True if some value of IntersectionType is also of Type. See
%  has_subtype/2 for further details.
has_intersection(Type, Intersection) :-
    ( var(Type); var(Intersection) ),
    !,
    fail.
has_intersection(Type, Intersection) :-
    type_intersection(Type, Intersection),
    !.
has_intersection(Type, Subtype) :-
    error:must_be(nonvar, Type),
    error:must_be(arbitrary_type, Subtype),
    shared_value(Type, Subtype, _),
    assert(type_intersection(Type, Subtype)).

% Find a value shared by both Type and Subtype
shared_value(Type, Subtype, Value) :-
    between(1,100,_),
    quickcheck:arbitrary(Subtype, Value),
    error:is_of_type(Type, Value),
    !.


%% known_type(?Type:type) is semidet.
%
%  True if Type is a type known to error:has_type/2. Iterates
%  all known types on backtracking. Be aware that some types are
%  polymorphic (like `list(T)`) so Type may be a non-ground term.
%
%  As a convenience, the type named `type` describes the set of all
%  values for which `known_type/1` is true.
known_type(Type) :-
    dif(Type, impossible),  % library(error) implementation detail
    clause(error:has_type(Type, _), _Body, _Ref).


:- multifile error:has_type/2.
error:has_type(type, T) :-
    known_type(T).
