:- module(mavis, [the/2]).

/** <module> Optional type declarations
Declare optional types which are checked during development time.
See pack documentation for more information.
*/

module_wants_mavis(Module) :-
    predicate_property(Module:the(_,_), imported_from(mavis)).

%%	the(+Type, ?Value) is det.
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
    Mode2 =.. [_|RawArgs],
    maplist(normalize_args, RawArgs, Args).

normalize_args(X0, arg(Mode,Name,Type)) :-
    ( X0 =.. [Mode0,Arg] -> true; Mode0='?', Arg=X0 ),
    ( member(Mode0, [+,-,?,:,@,!]) -> Mode=Mode0; Mode='?' ),
    ( Arg = Name:Type -> true; Name=Arg, Type=any).

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
    Module \= mavis,
    mavis:module_wants_mavis(Module),

    % fetch this predicate's structured comment
    functor(Head, Name, Arity),
    Indicator =.. [Slash, Name, Arity],
    pldoc_process:doc_comment(Module:Indicator,_,_,Comment),

    % parse and normalize mode description
    mode_declaration(Comment, ModeText),
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









