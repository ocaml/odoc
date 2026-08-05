Testing functor applications that go through `module type of`
=============================================================

It is not very common to define types of functors in OCaml, but it is possible
and it is also possible to get the type checker to determine the type via the
`module type of` operator.

Modules can use this type which, in turn, makes them functors.

Setup
-----

In this file we have a simple functor `Make` and create another module, `Named`
which has the same type as `Make` and thus also a functor.

  $ cat test.mli
  module Make (T : sig end) : sig type included end
  module Named : module type of Make
  
  module Applicant : sig end
  
  module Applied : module type of Named(Applicant)

There is also the module which it is applied to (`Applicant`) which is of no
particular importance and the functor application.

Test
----

However, at the moment Odoc can't see through the indirection and emits a
warning that the new functor, `Named`, is not a functor:

  $ compile test.mli
  File "test.odoc":
  Warning: Failed to lookup type identifier(root(Test).Named,false)(identifier(root(Test).Applicant,false)).included Parent_module: Parent_expr: Apply module is not a functor
