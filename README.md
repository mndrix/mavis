# Synopsis

    :- use_module(library(mavis)).

    %% even(+X:integer) is semidet.
    even(X) :-
        0 is X mod 2.

# Description

The =mavis= module (because she helps with typing ;-) allows one to
use optional type declarations in Prolog code. During *development*,
these declarations throw informative exceptions when values don't match
types.  A typical development environment converts this into a helpful
stack track which assists in locating the error.

In *production*, the declarations are completely removed by macros
and do nothing.  Production time
is defined as any time when optimization is enabled:
`current_prolog_flag(optimise, true)`.

Type declarations can be give manually by calling the/2.  `mavis` also inserts
type declarations for you based on your PlDoc structured comments.  For
example, during development, the definition of `even` above becomes

    even(A) :-
        the(integer, A),
        0 is A mod 2.

## Why?

We love dynamic types. That's one reason we love Prolog. But
sometimes more precise types are a helpful tool.  They can:

  * offer documentation to those reading our code
  * help find errors during development
  * structure our thinking during development
  * provide data for static analysis tools

# Defining new types

Mavis types are defined using error:has_type/2. We might define an
`even_integer` type with

    error:has_type(even_integer, X) :-
        0 is X mod 2.

We can use the definition manually:

    frobnify(A, B) :-
        the(integer, A),
        the(even_integer, B),
        B is 2*A.

or simply add it to our PlDoc comments:

    %% frobnify(+A:integer, -B:even_integer)
    frobnify(A, B) :-
        B is 2*A.

We can declare types for bound variables, like `A`, and
not-yet-bound variables, like `B`. The type constraints are implemented
with when/2 so they apply as soon as a variable is ground.

To disable type checking in production, start Prolog with the
`-O` command line argument. A macro eliminates calls to the/2 so they
have no runtime overhead.

# Changes in this Version

  * Fix packaging error

# Installation

Using SWI-Prolog 6.3.16 or later:

    $ swipl
    1 ?- pack_install(mavis).

Source code available and pull requests accepted on GitHub:
https://github.com/mndrix/mavis

@author Michael Hendricks <michael@ndrix.org>
@license BSD
