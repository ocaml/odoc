This test tests the ability to reference constructors, omitting the type they
are coming from.

  $ ocamlc -c -bin-annot a.mli
  $ odoc compile --warn-error -I . a.cmti

Currently, it is only possible to omit the parent type of the constructor in the
toplevel: only [{!M.constructor-Foo}] does not resolve, as it cannot find a type
named [M].

  $ odoc link a.odoc
  File "a.mli", line 15, characters 4-22:
  Warning: Failed to resolve reference unresolvedroot(t).A Couldn't find "t"
  File "a.mli", line 9, characters 4-24:
  Warning: Failed to resolve reference unresolvedroot(M).Foo Couldn't find "M"
