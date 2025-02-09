This is an excerpt from ocamlary, showing an issue resolving hidden modules that
aren't roots.
  $ cat test.mli
  module CanonicalTest : sig
    module Base__List : sig
      type 'a t
  
      val id : 'a t -> 'a t
    end
  
    module Base__ : sig
      module List = Base__List
      (** @canonical Test.CanonicalTest.Base.List *)
    end
  
    module Base : sig
      module List = Base__.List
    end
  
    module Base_Tests : sig
      module C : module type of Base__.List
  
      open Base__
      module L = List
  
      val baz : 'a Base__.List.t -> unit
      (** We can't reference [Base__] because it's hidden.
          {!List.t} ([List.t]) should resolve. *)
    end
  end
  
  val test : 'a CanonicalTest.Base__.List.t -> unit
  
  
  module Enclosing : sig
    (** This is going to contain a hidden item *)
  
    (**/**)
    module Hidden : sig
      module Still_hidden : sig
        type t
      end
    end
    
    (**/**)
  
  end
  
  
  module NonCanonical : sig
  
    module NotHidden = Enclosing.Hidden.Still_hidden
    (** This ought to be expanded *)
  
    type hidden__type = int
  
    val helpful : hidden__type
  end
      


  $ ocamlc -c -bin-annot test.mli
  $ odoc compile test.cmti

This shouldn't cause any warnings:

  $ odoc link test.odoc -I .
  File "test.mli", line 25, characters 8-17:
  Warning: Failed to resolve reference unresolvedroot(List).t Couldn't find "List"

There should be an expansion on `NotHidden`

  $ odoc_print test.odocl -r NonCanonical.NotHidden | jq '.type_.Alias[1]'
  {
    "Some": {
      "Signature": {
        "items": [
          {
            "Type": [
              "Ordinary",
              {
                "id": {
                  "`Type": [
                    {
                      "`Module": [
                        {
                          "`Module": [
                            {
                              "`Root": [
                                "None",
                                "Test"
                              ]
                            },
                            "NonCanonical"
                          ]
                        },
                        "NotHidden"
                      ]
                    },
                    "t"
                  ]
                },
                "source_loc": "None",
                "doc": {
                  "elements": [],
                  "warnings_tag": "None"
                },
                "equation": {
                  "params": [],
                  "private_": "false",
                  "manifest": "None",
                  "constraints": []
                },
                "representation": "None"
              }
            ]
          }
        ],
        "compiled": "true",
        "doc": {
          "elements": [],
          "warnings_tag": "None"
        }
      }
    }
  }

  $ odoc_print test.odocl -r NonCanonical.helpful
  {
    "id": {
      "`Value": [
        { "`Module": [ { "`Root": [ "None", "Test" ] }, "NonCanonical" ] },
        "helpful"
      ]
    },
    "source_loc": "None",
    "doc": { "elements": [], "warnings_tag": "None" },
    "type_": {
      "Constr": [
        {
          "`Resolved": {
            "`Identifier": {
              "`Type": [
                {
                  "`Module": [
                    { "`Root": [ "None", "Test" ] }, "NonCanonical"
                  ]
                },
                "hidden__type"
              ]
            }
          }
        },
        []
      ]
    },
    "value": "Abstract"
  }


