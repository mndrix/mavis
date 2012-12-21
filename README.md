The `mavis` module (because she helps with typing ;-) allows one to
give optional type declarations to Prolog code. During *development*,
these declarations throw informative exceptions if values don't match
types. In *production*, the declarations do nothing.

Why?
====

We love dynamic types. That's one reason we love Prolog. But sometimes
types are a helpful tool.  They can:

  * offer documentation to those reading our code
  * help find errors during development
  * structure our thinking during development
  * provide data for static analysis tools

How?
====

Mavis types are defined using `error:has_type/2`. We might define an
`even_integer` type with

```prolog
error:has_type(even_integer, X) :-
    0 is X mod 2.
```

Our code can use that definiton like

```prolog
%% frobnify(+A, -B)
frobnify(A, B) :-
    the(integer, A),
    the(even_integer, B),
    B is 2*A.
```

We can declare types for bound variables, like `A`, and
not-yet-bound variables, like `B`. The type constraints are implemented
with `freeze/2` so they apply as soon as a variable is bound.

To disable type checking in production, start Prolog with the
`-O` command line argument. A macro eliminates calls to `the/2` so they
have no runtime overhead.
