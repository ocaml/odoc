Test to check that when doing a deep substitution inside a signature,
that items that reference that element are still correctly resolved
even if they're in a different path.

  $ cat m.mli
  module type S = sig
    module M: sig
      type t
    end
    type t = M.t
  end
  module type T = S with type M.t := int
  

In this, we want to check that the type `t` in module type `T` has
its RHS correctly replaced with an `int`

  $ ocamlc -c -bin-annot m.mli
  $ odoc compile m.cmti
  $ odoc link m.odoc
  $ odoc html-generate m.odocl --indent -o .
  $ odoc_print m.odocl -r T.t | jq .
  {
    "id": {
      "`Type": [
        {
          "`ModuleType": [
            {
              "`Root": [
                "None",
                "M"
              ]
            },
            "T"
          ]
        },
        "t"
      ]
    },
    "locs": "None",
    "doc": [],
    "equation": {
      "params": [],
      "private_": "false",
      "manifest": {
        "Some": {
          "Constr": [
            {
              "`Resolved": {
                "`Identifier": {
                  "`CoreType": "int"
                }
              }
            },
            []
          ]
        }
      },
      "constraints": []
    },
    "representation": "None"
  }
 
