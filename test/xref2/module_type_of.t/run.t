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

Make sure the expansion of `T` is present

Check that the expansion of `T` contains only 2 modules (the module `X` should have been removed)

  $ odoc_print m.odocl | jq ".content.Module.items[2].ModuleType.expr.Some.With.w_expansion.Some.Signature.items" > T_sig.json
  $ jq "map(map_values(.id))" < T_sig.json
  [
    {
      "ModuleType": {
        "`ModuleType": [
          {
            "`ModuleType": [
              {
                "`Root": [
                  {
                    "Some": {
                      "`Page": [
                        "None",
                        "test"
                      ]
                    }
                  },
                  "M"
                ]
              },
              "T"
            ]
          },
          "Y"
        ]
      }
    },
    {
      "ModuleType": {
        "`ModuleType": [
          {
            "`ModuleType": [
              {
                "`Root": [
                  {
                    "Some": {
                      "`Page": [
                        "None",
                        "test"
                      ]
                    }
                  },
                  "M"
                ]
              },
              "T"
            ]
          },
          "Z"
        ]
      }
    }
  ]

Check that the expansion of 'T.Y' contains only 1 type

  $ jq ".[0].ModuleType.expr.Some.TypeOf.t_expansion.Some.Signature.items" < T_sig.json > T.Y_sig.json
  $ odoc_print m.odocl | jq "map(keys | .[0])" < T.Y_sig.json
  [
    "Type"
  ]

Verify that T.Y.t has not been strengthened

  $ jq ".[0].Type[1].equation.manifest" < T.Y_sig.json
  "None"

But that T.Z.t _has_ been strengthened

  $ jq ".[1].ModuleType.expr.Some.TypeOf.t_expansion.Some.Signature.items" < T_sig.json > T.Z_sig.json
  $ jq ".[0].Type[1].equation.manifest" < T.Z_sig.json
  {
    "Some": {
      "Constr": [
        {
          "`Resolved": {
            "`Type": [
              {
                "`Identifier": {
                  "`Module": [
                    {
                      "`Root": [
                        {
                          "Some": {
                            "`Page": [
                              "None",
                              "test"
                            ]
                          }
                        },
                        "M"
                      ]
                    },
                    "X1"
                  ]
                }
              },
              "t"
            ]
          }
        },
        []
      ]
    }
  }
