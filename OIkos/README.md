OIkos
=====

OIkos is an IKOS API in OCaml.

Interfaces
----------

OIkos is composed of :
- A small API in `api/ikos.mli`, very close to IKOS
- A higher level wrapper in `ast/ast.mli`, that allows you to write in a language very close to C, and use IKOS
- LustreC, to use IKOS on lustre code

You can try OIkos with the ocaml interactive interpreter `ast/ast.top` in the build directory.

Examples
--------

See `tests/test_api.ml` and `tests/test_ast_example.ml` for examples.
