In this test we're marking a ModuleType with a canonical path, then referencing
it later in the file. The resulting resolved path should contain a `CanonicalT
constructor where the second element of the tuple is Resolved.

  $ cat test.mli
  (** @canonical Test.Y *)
  module type X = sig
          type t
  end
  
  module type Y = X
  
  module type Z = X
  

  $ ocamlc -c -bin-annot test.mli
  $ odoc compile --package x test.cmti
  $ odoc link test.odoc
  $ odoc_print test.odocl | jq ".content.Module.items[2].ModuleType.expr.Some.Path.p_path"
  {
    "`Resolved": {
      "`CanonicalT": [
        {
          "`Identifier": {
            "`ModuleType": [
              {
                "`Root": [
                  {
                    "`RootPage": "x"
                  },
                  "Test"
                ]
              },
              "X"
            ]
          }
        },
        {
          "`Resolved": {
            "`Identifier": {
              "`ModuleType": [
                {
                  "`Root": [
                    {
                      "`RootPage": "x"
                    },
                    "Test"
                  ]
                },
                "Y"
              ]
            }
          }
        }
      ]
    }
  }

