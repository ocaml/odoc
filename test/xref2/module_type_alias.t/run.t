Test module type aliases

In the following module:

  $ cat test.mli
  module type A = sig
    type t
    val v : t
  end
  
  module type B = A
  
  (** {!B} {!B.t} *)
  

both the references in the comment ought to contain AliasModuleType constructors
as they are both referencing items that won't be expanded.  

  $ ocamlc -c -bin-annot test.mli
  $ odoc compile test.cmti
  $ odoc link test.odoc
  $ odoc html-generate -o html test.odocl
  $ odoc_print test.odocl | jq ".content.Module.items[2]"
  {
    "Comment": {
      "`Docs": [
        {
          "`Paragraph": [
            {
              "`Reference": [
                {
                  "`Resolved": {
                    "`AliasModuleType": [
                      {
                        "`Identifier": {
                          "`ModuleType": [
                            {
                              "`Root": [
                                "None",
                                "Test"
                              ]
                            },
                            "A"
                          ]
                        }
                      },
                      {
                        "`Identifier": {
                          "`ModuleType": [
                            {
                              "`Root": [
                                "None",
                                "Test"
                              ]
                            },
                            "B"
                          ]
                        }
                      }
                    ]
                  }
                },
                []
              ]
            },
            "`Space",
            {
              "`Reference": [
                {
                  "`Resolved": {
                    "`Type": [
                      {
                        "`AliasModuleType": [
                          {
                            "`Identifier": {
                              "`ModuleType": [
                                {
                                  "`Root": [
                                    "None",
                                    "Test"
                                  ]
                                },
                                "A"
                              ]
                            }
                          },
                          {
                            "`Identifier": {
                              "`ModuleType": [
                                {
                                  "`Root": [
                                    "None",
                                    "Test"
                                  ]
                                },
                                "B"
                              ]
                            }
                          }
                        ]
                      },
                      "t"
                    ]
                  }
                },
                []
              ]
            }
          ]
        }
      ]
    }
  }


