This test emulates how dune does wrapping of libraries. We first have the alias module
that defines the short names for the library mdules. This module is compiled first and
using the '-no-alias-deps' command line argument, since the modules it is aliasing haven't
been compiled yet. We also need to disable warning 49 (no cmi file was found in path for
module).

  $ cat foo__.ml
  module Type0 = Foo__type0
  
  (** @canonical Foo.Type *)
  module Type = Foo__type
  
  $ ocamlc -w -49 -bin-annot -no-alias-deps -c foo__.ml

Then we can compile the modules that make up the library. In this test we have two modules,
only one of which will be exposed in the end. Module `Foo__type0` will _not_ be exposed,
but holds the definitions of some types. We mark one of these types with a canonical tag,
indicating that it will be exposed as Foo.Type.Path.t. Note that we also reference within
the type to be exposed a type that will _not_ be exposed.

  $ cat foo__type0.ml
  module Identifier = struct
    type t = [ `boo ]
  end
  
  module Resolved_path = struct
    type module_ = [ `Identifier of Identifier.t | `Hidden of module_ ]
    (** @canonical Foo.Type.Path.t *)
  end

  $ ocamlc -c -bin-annot foo__type0.ml

Here is the module in which the types above will be exposed.

  $ cat foo__type.ml
  module Path = struct
      type t = Type0.Resolved_path.module_
  
      type u = Type0.Resolved_path.module_
  end
  
  type t = Type0.Resolved_path.module_

  $ ocamlc -c -bin-annot -open Foo__ foo__type.ml

And finally the module we expose everything underneath.

  $ cat foo.ml
  module Type = Type
  

  $ ocamlc -c -bin-annot -open Foo__ foo.ml

Now we can run odoc

  $ odoc compile --package x -I . foo__.cmt
  $ odoc compile --package x -I . foo__type0.cmt
  $ odoc compile --package x -I . foo__type.cmt
  $ odoc compile --package x -I . foo.cmt

We only need to link `foo` as all the others are hidden

  $ odoc link -I . foo.odoc
  $ odoc html-generate foo.odocl -o html
  $ odoc support-files -o html

Now we check that any types with 'equations' found in `foo` are equal to
polymorphic variants rather than Constrs

  $ odoc_print foo.odocl -r Type.Path.t | jq '.. | .["equation"]? | select(.) | .manifest.Some.Polymorphic_variant.kind'
  "Fixed"

Canonical paths should be as short as possible. As such, the following ought to be just an Identifier:

  $ odoc_print foo.odocl -r Type.Path.u | jq '.. | .["equation"]? | select(.) | .manifest.Some.Constr[0]["`Resolved"]["`CanonicalType"][1]'
  {
    "`Resolved": {
      "`Identifier": {
        "`Type": [
          {
            "`Module": [
              {
                "`Module": [
                  {
                    "`Root": [
                      {
                        "Some": {
                          "`Page": [
                            "None",
                            "x"
                          ]
                        }
                      },
                      "Foo"
                    ]
                  },
                  "Type"
                ]
              },
              "Path"
            ]
          },
          "t"
        ]
      }
    }
  }

And this one should be `` `Type(`Identifier,t) ``

  $ odoc_print foo.odocl -r Type.t | jq '.. | .["equation"]? | select(.) | .manifest.Some.Constr[0]["`Resolved"]["`CanonicalType"][1]'
  {
    "`Resolved": {
      "`Type": [
        {
          "`Identifier": {
            "`Module": [
              {
                "`Module": [
                  {
                    "`Root": [
                      {
                        "Some": {
                          "`Page": [
                            "None",
                            "x"
                          ]
                        }
                      },
                      "Foo"
                    ]
                  },
                  "Type"
                ]
              },
              "Path"
            ]
          }
        },
        "t"
      ]
    }
  }
