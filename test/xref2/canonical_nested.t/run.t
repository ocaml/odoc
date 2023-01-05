In this test, we have two nested canonical modules. An outer one that
resembles the normal dune namespacing trick, and an inner one where we
have declared a particular module alias to be the canonical module for
another module:

  $ cat main__Container.mli
  module Test : sig
    (** Nested canonical test *)
  
    module A : sig
      (** @canonical Main.Container.Test.B *)
      
      type t
    end
  
    module B = A
  
    type t = A.t
  end
  
  
Here is the inner alias with the canonical tag in `module A` declaring
`B` to be the canonical module, where `B` is an alias of `A`. The
result of this is that:
1. module `B` should be expanded
2. type `t` should link to the definition of `t` in module `B` rather
than in module `A`.

  $ ocamlc -c -bin-annot main__Container.mli
  $ ocamlc -c -bin-annot main__.ml
  $ ocamlc -c -bin-annot main.ml
  $ odoc compile main__Container.cmti
  $ odoc compile -I . main__.cmt
  $ odoc compile -I . main.cmt

At this point none of the canonical references are resolved. They should be
unresolved in the paths though:

  $ odoc_print -r Test.B main__Container.odoc
  {
    "id": {
      "`Module": [
        { "`Module": [ { "`Root": [ "None", "Main__Container" ] }, "Test" ] },
        "B"
      ]
    },
    "locs": {
      "Some": {
        "source_parent": { "`Root": [ "None", "Main__Container" ] },
        "impl": "None",
        "intf": {
          "Some": "File \"main__Container.mli\", line 10, characters 2-14"
        }
      }
    },
    "doc": [],
    "type_": {
      "Alias": [
        {
          "`Resolved": {
            "`Canonical": [
              {
                "`Identifier": {
                  "`Module": [
                    {
                      "`Module": [
                        { "`Root": [ "None", "Main__Container" ] },
                        "Test"
                      ]
                    },
                    "A"
                  ]
                }
              },
              {
                "`Dot": [
                  {
                    "`Dot": [
                      { "`Dot": [ { "`Root": "Main" }, "Container" ] },
                      "Test"
                    ]
                  },
                  "B"
                ]
              }
            ]
          }
        },
        "None"
      ]
    },
    "canonical": "None",
    "hidden": "false"
  }

  $ odoc_print -r Container main.odoc
  {
    "id": { "`Module": [ { "`Root": [ "None", "Main" ] }, "Container" ] },
    "locs": {
      "Some": {
        "source_parent": { "`Root": [ "None", "Main" ] },
        "impl": { "Some": "def-Main0" },
        "intf": "None"
      }
    },
    "doc": [],
    "type_": {
      "Alias": [
        {
          "`Resolved": {
            "`Canonical": [
              {
                "`Module": [
                  {
                    "`Hidden": {
                      "`Identifier": { "`Root": [ "None", "Main__" ] }
                    }
                  },
                  "Container"
                ]
              },
              { "`Dot": [ { "`Root": "Main" }, "Container" ] }
            ]
          }
        },
        "None"
      ]
    },
    "canonical": "None",
    "hidden": "false"
  }

  $ odoc link -I . main.odoc

  $ odoc_print -r Container.Test.B main.odocl
  {
    "id": {
      "`Module": [
        {
          "`Module": [
            { "`Module": [ { "`Root": [ "None", "Main" ] }, "Container" ] },
            "Test"
          ]
        },
        "B"
      ]
    },
    "locs": {
      "Some": {
        "source_parent": {
          "`Module": [ { "`Root": [ "None", "Main" ] }, "Container" ]
        },
        "impl": "None",
        "intf": {
          "Some": "File \"main__Container.mli\", line 10, characters 2-14"
        }
      }
    },
    "doc": [],
    "type_": {
      "Alias": [
        {
          "`Resolved": {
            "`Canonical": [
              {
                "`Identifier": {
                  "`Module": [
                    {
                      "`Module": [
                        {
                          "`Module": [
                            { "`Root": [ "None", "Main" ] },
                            "Container"
                          ]
                        },
                        "Test"
                      ]
                    },
                    "A"
                  ]
                }
              },
              {
                "`Resolved": {
                  "`Identifier": {
                    "`Module": [
                      {
                        "`Module": [
                          {
                            "`Module": [
                              { "`Root": [ "None", "Main" ] },
                              "Container"
                            ]
                          },
                          "Test"
                        ]
                      },
                      "B"
                    ]
                  }
                }
              }
            ]
          }
        },
        {
          "Some": {
            "e_expansion": {
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
                                      "`Module": [
                                        { "`Root": [ "None", "Main" ] },
                                        "Container"
                                      ]
                                    },
                                    "Test"
                                  ]
                                },
                                "B"
                              ]
                            },
                            "t"
                          ]
                        },
                        "locs": {
                          "source_parent": {
                            "`Module": [
                              { "`Root": [ "None", "Main" ] },
                              "Container"
                            ]
                          },
                          "impl": "None",
                          "intf": {
                            "Some":
                              "File \"main__Container.mli\", line 7, characters 4-10"
                          }
                        },
                        "doc": [],
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
                "doc": []
              }
            },
            "e_sources": "None"
          }
        }
      ]
    },
    "canonical": "None",
    "hidden": "false"
  }


