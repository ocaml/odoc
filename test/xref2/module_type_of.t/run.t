Testing `module type of` expressions
====================================

In OCaml, `module type of` is evaluated eagerly, such that subsequent modifications to anything containing a `module type of` expression behave as if it had just been an explicit signature. For example, in the
following signature:

```ocaml
module type S = sig
end
```

this should behave as if we had written:

```ocaml
module type S = sig
end
```

Test file
---------

This is the test file we're using. We declare a module type S, which contains a module X. We then further declare two module types that take the strengthened and unstrengthened signatures of X. This test is designed to ensure that the semantics of `module type of` reflect OCaml's - that is, in the following test we want to ensure that the extra type from module `X1` does not 'leak into' module types `Y` and `Z`.

  $ cat m.mli
  module type S = sig
    module X : sig type t end
    module type Y = module type of X
    module type Z = module type of struct include X end
  end
  module X1 : sig type t type u end
  module type T = S with module X := X1

Compile
-------

  $ compile m.mli

Tests
-----

Make sure the expansion of `T` is present, and check that the expansion of `T`
contains only 2 modules (the module `X` should have been removed)

  $ odoc_print m.odocl -r T --short --show-expansions
  module type M.T = M.S with X := M.X1
    (sig :
      module type Y = module type of M.X1 (sig : type t end)
      module type Z = module type of struct include M.X1 end
        (sig : type t = M.X1.t end)
     end)

Check that the expansion of 'T.Y' contains only 1 type

  $ odoc_print m.odocl -r T.Y --short --show-expansions
  module type M.T.Y = module type of M.X1
    (sig : type t end)

Verify that T.Y.t has not been strengthened

  $ odoc_print m.odocl -r T.Y.t --short
  type M.T.Y.t 

But that T.Z.t _has_ been strengthened

  $ odoc_print m.odocl -r T.Z.t --short
  type M.T.Z.t  = M.X1.t
