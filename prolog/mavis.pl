:- module(mavis, [the/2]).

/** <module> Optional type declarations

The =mavis= module (because she helps with typing ;-) allows one to
give optional type declarations to Prolog code. During *development*,
these declarations throw informative exceptions if values don't match
types. In *production*, the declarations do nothing.

---+ Why?

We love dynamic types. That's one reason we love Prolog. But sometimes
types are a helpful tool.  They can:

  * offer documentation to those reading our code
  * help find errors during development
  * structure our thinking during development
  * provide data for static analysis tools

---+ How?

Mavis types are defined using error:has_type/2. We might define an
=even_integer= type with

==
error:has_type(even_integer, X) :-
    0 is X mod 2.
==

Our code can use that definiton like

==
%% frobnify(+A, -B)
frobnify(A, B) :-
    the(integer, A),
    the(even_integer, B),
    B is 2*A.
==

We can declare types for bound variables, like =A=, and
not-yet-bound variables, like =B=. The type constraints are implemented
with freeze/2 so they apply as soon as a variable is bound.

To disable type checking in production, start Prolog with the
=|-O|= command line argument. A macro eliminates calls to the/2 so they
have no runtime overhead.

@author Michael Hendricks <michael@ndrix.org>
@license BSD
*/

%%	the(+Type, ?Value) is det.
%
%	Declare that Value has the given Type.
%	Succeeds if Value is bound to a value that's compatible
%	with Type.  Throws an informative exception if Value
%	is bound to a value that's not compatible with Type.
%	If Value is not bound, the type check is delayed until
%	Value becomes bound.
%
%	When optimizations are enabled
%	(=|current_prolog_flag(optimise, true)|= a macro removes =the=
%	entirely so that it always succeeds.
:- if(current_prolog_flag(optimise,true)).

the(_,_).  % avoid "Exported procedure mavis:the/2 is not defined"
user:goal_expansion(the(_,_), true).

:- else.

:- use_module(library(pldoc/doc_modes), []).
:- use_module(library(pldoc/doc_wiki), [indented_lines/3]).
:- use_module(library(charsio), [read_term_from_chars/3]).

% extract mode declaration from a structured comment
mode_declaration(Comment, ModeCodes) :-
    string_to_list(Comment, Codes),
    indented_lines(Codes, ["%"], Lines),
    pldoc_modes:mode_lines(Lines, ModeCodes, [], _).

% read a raw mode declaration from character codes
read_mode_declaration(ModeCodes, Mode) :-
    Options = [module(pldoc_modes), variable_names(Vars)],
    read_term_from_chars(ModeCodes, Mode, Options),
    maplist(call,Vars).

% convert mode declarations to a standard form
normalize_mode(Mode0, Module, mode(Module, Indicator, Args, Det)) :-
    (Mode0 = is(Mode1, Det) -> true; Mode1=Mode0, Det=nondet),
    (Mode1 = //(Mode2) -> Slash='//'; Mode2=Mode1, Slash='/' ),
    Mode2 =.. [Functor|RawArgs],
    length(RawArgs, Arity),
    Indicator =.. [Slash, Functor, Arity],
    maplist(normalize_args, RawArgs, Args).

normalize_args(X0, arg(Mode,Name,Type)) :-
    ( X0 =.. [Mode0,Arg] -> true; Mode0='?', Arg=X0 ),
    ( member(Mode0, [+,-,?,:,@,!]) -> Mode=Mode0; Mode='?' ),
    ( Arg = Name:Type -> true; Name=Arg, Type=any).

% Convert PlDoc structured comments into mavis:mode/4 facts.
:- dynamic mode/4.
:- multifile prolog:comment_hook/3.
prolog:comment_hook([_-Comment|_],_,_) :-
    prolog_load_context(module, Module),
    %Module = mavis, % TODO remove me
    mode_declaration(Comment, ModeText),
    read_mode_declaration(ModeText, RawMode),
    normalize_mode(RawMode, Module, Mode),
    assert(Mode).


the(Type, Value) :-
    freeze(Value, must_be(Type, Value)).

%%	foo(+A:integer, -B, C)// is semidet.
%
%	This is a fake structured comment.
foo(_,_).

%%	lazy(X)
lazy(_).

%%	just_type(Y:integer) is det.
just_type(_).

:- endif.









