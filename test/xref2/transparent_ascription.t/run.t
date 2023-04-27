Transparent ascription
======================

  $ ocamlc -c -bin-annot test.mli
  $ odoc compile test.cmti

The following modules should be expanded (and be a `"Signature": {}`):

  $ odoc_print test.odoc -r Basic.P.N | jq ".type_ | (.ModuleType.Signature|={})"
  {
    "ModuleType": {
      "Signature": {}
    }
  }

  $ odoc_print test.odoc -r Nested.P1.N1 | jq ".type_ | (.ModuleType.Signature|={})"
  {
    "ModuleType": {
      "Signature": {}
    }
  }

  $ odoc_print test.odoc -r Nested.P1.N2 | jq ".type_ | (.ModuleType.Signature|={})"
  {
    "ModuleType": {
      "Signature": {}
    }
  }

  $ odoc_print test.odoc -r Nested.P2.N1 | jq ".type_ | (.ModuleType.Signature|={})"
  {
    "ModuleType": {
      "Signature": {}
    }
  }

  $ odoc_print test.odoc -r Nested.P2.N2 | jq ".type_ | (.ModuleType.Signature|={})"
  {
    "ModuleType": {
      "Signature": {}
    }
  }

  $ odoc_print test.odoc -r Via_alias.P.N | jq ".type_ | (.ModuleType.Signature|={})"
  {
    "ModuleType": {
      "Signature": {}
    }
  }

The following modules expressions should remain as they are typed:
(the `t_expansion` should be `Some {}`)

  $ odoc_print test.odoc -r Cascade.P.N1 | jq ".type_ | (.ModuleType.TypeOf.t_expansion.Some|={})"
  {
    "ModuleType": {
      "TypeOf": {
        "t_desc": {
          "ModPath": {
            "`Resolved": {
              "`Identifier": {
                "`Module": [
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
                          "Cascade"
                        ]
                      },
                      "P"
                    ]
                  },
                  "O"
                ]
              }
            }
          }
        },
        "t_expansion": {
          "Some": {}
        }
      }
    }
  }

  $ odoc_print test.odoc -r Cascade.P.N2 | jq ".type_ | (.ModuleType.TypeOf.t_expansion.Some|={})"
  {
    "ModuleType": {
      "TypeOf": {
        "t_desc": {
          "ModPath": {
            "`Resolved": {
              "`Module": [
                {
                  "`Identifier": {
                    "`Module": [
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
                              "Cascade"
                            ]
                          },
                          "P"
                        ]
                      },
                      "O"
                    ]
                  }
                },
                "I"
              ]
            }
          }
        },
        "t_expansion": {
          "Some": {}
        }
      }
    }
  }

  $ odoc_print test.odoc -r In_functor_parameter.P.G | jq ".type_ | (.ModuleType.Functor|=[]) | (.ModuleType.TypeOf.t_expansion.Some|={})"
  {
    "ModuleType": {
      "Functor": [],
      "TypeOf": {
        "t_expansion": {
          "Some": {}
        }
      }
    }
  }

  $ odoc link test.odoc
  $ odoc html-generate test.odocl -o html
  $ odoc support-files -o html
  $ cp -a html /tmp/test-html
