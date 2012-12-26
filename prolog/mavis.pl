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
the(Type, Value) :-
    freeze(Value, must_be(Type, Value)).
:- endif.

split([], _, []).
split([X], Div, [[X]]) :-
    dif(X, Div),
    !.  % optimization
split([Div|T], Div, [[]|Rest]) :-
    split(T, Div, Rest),
    !.  % optimization
split([H|T], Div, [[H|First]|Rest]) :-
    dif(H, Div),
    split(T, Div, [First|Rest]).  % implies: dif(T, [])


:- begin_tests(split).
test(forward_zero) :-
    split("", 10, []).
test(forward_one) :-
    split("hello", 10, ["hello"]).
test(forward_two) :-
    split("hello\naravis", 10, ["hello", "aravis"]).
test(forward_three) :-
    split("hello\naravis\njericho", 10, ["hello","aravis","jericho"]).

test(backward_zero) :-
    split(Codes, 10, []),
    Codes = [].
test(backward_one) :-
    split(Codes, 10, ["hello"]),
    Codes = "hello".
test(backward_two) :-
    [Comma] = ",",
    split(Codes, Comma, ["alpha", "beta"]),
    Codes = "alpha,beta".
test(backward_three) :-
    [Comma] = ",",
    split(Codes, Comma, ["alpha","beta","gamma"]),
    Codes = "alpha,beta,gamma".

test(find_separator) :-
    split("alpha,beta", Div, ["alpha","beta"]),
    [Div] = ",".

test(forward_trailing_zero) :-
    split("\n", 10, [[]]).
test(forward_trailing_one) :-
    split("hello\n", 10, ["hello"]).
test(forward_trailing_two) :-
    split("hello\ngoodbye\n", 10, ["hello", "goodbye"]).
:- end_tests(split).
