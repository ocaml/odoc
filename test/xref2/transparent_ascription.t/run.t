Transparent ascription
======================

  $ ocamlc -c -bin-annot test.mli
  $ odoc compile test.cmti
  $ odoc_print test.odoc -r Basic.P.N
  {
    "id": {
      "`Module": [
        {
          "`Module": [
            { "`Module": [ { "`Root": [ "None", "Test" ] }, "Basic" ] }, "P"
          ]
        },
        "N"
      ]
    },
    "locs": "None",
    "doc": [],
    "type_": {
      "ModuleType": {
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
                                  { "`Root": [ "None", "Test" ] }, "Basic"
                                ]
                              },
                              "P"
                            ]
                          },
                          "N"
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
      }
    },
    "canonical": "None",
    "hidden": "false"
  }
  $ odoc_print test.odoc -r Nested.P1.N1
  {
    "id": {
      "`Module": [
        {
          "`Module": [
            { "`Module": [ { "`Root": [ "None", "Test" ] }, "Nested" ] }, "P1"
          ]
        },
        "N1"
      ]
    },
    "locs": "None",
    "doc": [],
    "type_": {
      "ModuleType": {
        "Signature": {
          "items": [
            {
              "Module": [
                "Ordinary",
                {
                  "id": {
                    "`Module": [
                      {
                        "`Module": [
                          {
                            "`Module": [
                              {
                                "`Module": [
                                  { "`Root": [ "None", "Test" ] }, "Nested"
                                ]
                              },
                              "P1"
                            ]
                          },
                          "N1"
                        ]
                      },
                      "M"
                    ]
                  },
                  "locs": "None",
                  "doc": [],
                  "type_": {
                    "ModuleType": {
                      "Path": {
                        "p_expansion": {
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
                                                    "`Module": [
                                                      {
                                                        "`Module": [
                                                          {
                                                            "`Root": [
                                                              "None", "Test"
                                                            ]
                                                          },
                                                          "Nested"
                                                        ]
                                                      },
                                                      "P1"
                                                    ]
                                                  },
                                                  "N1"
                                                ]
                                              },
                                              "M"
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
                          }
                        },
                        "p_path": {
                          "`Resolved": {
                            "`Identifier": {
                              "`ModuleType": [
                                { "`Root": [ "None", "Test" ] }, "T"
                              ]
                            }
                          }
                        }
                      }
                    }
                  },
                  "canonical": "None",
                  "hidden": "false"
                }
              ]
            }
          ],
          "compiled": "true",
          "doc": []
        }
      }
    },
    "canonical": "None",
    "hidden": "false"
  }
  $ odoc_print test.odoc -r Nested.P1.N2
  {
    "id": {
      "`Module": [
        {
          "`Module": [
            { "`Module": [ { "`Root": [ "None", "Test" ] }, "Nested" ] }, "P1"
          ]
        },
        "N2"
      ]
    },
    "locs": "None",
    "doc": [],
    "type_": {
      "ModuleType": {
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
                                  { "`Root": [ "None", "Test" ] }, "Nested"
                                ]
                              },
                              "P1"
                            ]
                          },
                          "N2"
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
      }
    },
    "canonical": "None",
    "hidden": "false"
  }
  $ odoc_print test.odoc -r Nested.P2.N1
  {
    "id": {
      "`Module": [
        {
          "`Module": [
            { "`Module": [ { "`Root": [ "None", "Test" ] }, "Nested" ] }, "P2"
          ]
        },
        "N1"
      ]
    },
    "locs": "None",
    "doc": [],
    "type_": {
      "ModuleType": {
        "Signature": {
          "items": [
            {
              "Module": [
                "Ordinary",
                {
                  "id": {
                    "`Module": [
                      {
                        "`Module": [
                          {
                            "`Module": [
                              {
                                "`Module": [
                                  { "`Root": [ "None", "Test" ] }, "Nested"
                                ]
                              },
                              "P2"
                            ]
                          },
                          "N1"
                        ]
                      },
                      "M"
                    ]
                  },
                  "locs": "None",
                  "doc": [],
                  "type_": {
                    "ModuleType": {
                      "Path": {
                        "p_expansion": {
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
                                                    "`Module": [
                                                      {
                                                        "`Module": [
                                                          {
                                                            "`Root": [
                                                              "None", "Test"
                                                            ]
                                                          },
                                                          "Nested"
                                                        ]
                                                      },
                                                      "P2"
                                                    ]
                                                  },
                                                  "N1"
                                                ]
                                              },
                                              "M"
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
                          }
                        },
                        "p_path": {
                          "`Resolved": {
                            "`Identifier": {
                              "`ModuleType": [
                                { "`Root": [ "None", "Test" ] }, "T"
                              ]
                            }
                          }
                        }
                      }
                    }
                  },
                  "canonical": "None",
                  "hidden": "false"
                }
              ]
            }
          ],
          "compiled": "true",
          "doc": []
        }
      }
    },
    "canonical": "None",
    "hidden": "false"
  }
  $ odoc_print test.odoc -r Nested.P2.N2
  {
    "id": {
      "`Module": [
        {
          "`Module": [
            { "`Module": [ { "`Root": [ "None", "Test" ] }, "Nested" ] }, "P2"
          ]
        },
        "N2"
      ]
    },
    "locs": "None",
    "doc": [],
    "type_": {
      "ModuleType": {
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
                                  { "`Root": [ "None", "Test" ] }, "Nested"
                                ]
                              },
                              "P2"
                            ]
                          },
                          "N2"
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
      }
    },
    "canonical": "None",
    "hidden": "false"
  }
  $ odoc_print test.odoc -r Via_alias.P.N
  {
    "id": {
      "`Module": [
        {
          "`Module": [
            { "`Module": [ { "`Root": [ "None", "Test" ] }, "Via_alias" ] },
            "P"
          ]
        },
        "N"
      ]
    },
    "locs": "None",
    "doc": [],
    "type_": {
      "ModuleType": {
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
                                  { "`Root": [ "None", "Test" ] }, "Via_alias"
                                ]
                              },
                              "P"
                            ]
                          },
                          "N"
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
                              "`Type": [
                                {
                                  "`Alias": [
                                    {
                                      "`Identifier": {
                                        "`Module": [
                                          { "`Root": [ "None", "Test" ] },
                                          "Int"
                                        ]
                                      }
                                    },
                                    {
                                      "`Identifier": {
                                        "`Module": [
                                          {
                                            "`Module": [
                                              {
                                                "`Module": [
                                                  {
                                                    "`Root": [ "None", "Test" ]
                                                  },
                                                  "Via_alias"
                                                ]
                                              },
                                              "P"
                                            ]
                                          },
                                          "M"
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
                    },
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
      }
    },
    "canonical": "None",
    "hidden": "false"
  }
  $ odoc_print test.odoc -r Cascade.P.N1
  {
    "id": {
      "`Module": [
        {
          "`Module": [
            { "`Module": [ { "`Root": [ "None", "Test" ] }, "Cascade" ] }, "P"
          ]
        },
        "N1"
      ]
    },
    "locs": "None",
    "doc": [],
    "type_": {
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
                            { "`Root": [ "None", "Test" ] }, "Cascade"
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
            "Some": {
              "Signature": {
                "items": [
                  {
                    "Module": [
                      "Ordinary",
                      {
                        "id": {
                          "`Module": [
                            {
                              "`Module": [
                                {
                                  "`Module": [
                                    {
                                      "`Module": [
                                        { "`Root": [ "None", "Test" ] },
                                        "Cascade"
                                      ]
                                    },
                                    "P"
                                  ]
                                },
                                "N1"
                              ]
                            },
                            "I"
                          ]
                        },
                        "locs": "None",
                        "doc": [],
                        "type_": {
                          "ModuleType": {
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
                                                      {
                                                        "`Module": [
                                                          {
                                                            "`Root": [
                                                              "None", "Test"
                                                            ]
                                                          },
                                                          "Cascade"
                                                        ]
                                                      },
                                                      "P"
                                                    ]
                                                  },
                                                  "N1"
                                                ]
                                              },
                                              "I"
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
                          }
                        },
                        "canonical": "None",
                        "hidden": "false"
                      }
                    ]
                  }
                ],
                "compiled": "true",
                "doc": []
              }
            }
          }
        }
      }
    },
    "canonical": "None",
    "hidden": "false"
  }
  $ odoc_print test.odoc -r Cascade.P.N2
  {
    "id": {
      "`Module": [
        {
          "`Module": [
            { "`Module": [ { "`Root": [ "None", "Test" ] }, "Cascade" ] }, "P"
          ]
        },
        "N2"
      ]
    },
    "locs": "None",
    "doc": [],
    "type_": {
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
                                { "`Root": [ "None", "Test" ] }, "Cascade"
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
                                      "`Module": [
                                        { "`Root": [ "None", "Test" ] },
                                        "Cascade"
                                      ]
                                    },
                                    "P"
                                  ]
                                },
                                "N2"
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
            }
          }
        }
      }
    },
    "canonical": "None",
    "hidden": "false"
  }
  $ odoc_print test.odoc -r In_functor_parameter.P.G
  {
    "id": {
      "`Module": [
        {
          "`Module": [
            {
              "`Module": [
                { "`Root": [ "None", "Test" ] }, "In_functor_parameter"
              ]
            },
            "P"
          ]
        },
        "G"
      ]
    },
    "locs": "None",
    "doc": [],
    "type_": {
      "ModuleType": {
        "Functor": [
          {
            "Named": {
              "id": {
                "`Parameter": [
                  {
                    "`Module": [
                      {
                        "`Module": [
                          {
                            "`Module": [
                              { "`Root": [ "None", "Test" ] },
                              "In_functor_parameter"
                            ]
                          },
                          "P"
                        ]
                      },
                      "G"
                    ]
                  },
                  "X"
                ]
              },
              "expr": {
                "Signature": {
                  "items": [
                    {
                      "Type": [
                        "Ordinary",
                        {
                          "id": {
                            "`Type": [
                              {
                                "`Parameter": [
                                  {
                                    "`Module": [
                                      {
                                        "`Module": [
                                          {
                                            "`Module": [
                                              { "`Root": [ "None", "Test" ] },
                                              "In_functor_parameter"
                                            ]
                                          },
                                          "P"
                                        ]
                                      },
                                      "G"
                                    ]
                                  },
                                  "X"
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
                            "manifest": "None",
                            "constraints": []
                          },
                          "representation": "None"
                        }
                      ]
                    },
                    {
                      "Value": {
                        "id": {
                          "`Value": [
                            {
                              "`Parameter": [
                                {
                                  "`Module": [
                                    {
                                      "`Module": [
                                        {
                                          "`Module": [
                                            { "`Root": [ "None", "Test" ] },
                                            "In_functor_parameter"
                                          ]
                                        },
                                        "P"
                                      ]
                                    },
                                    "G"
                                  ]
                                },
                                "X"
                              ]
                            },
                            "plus"
                          ]
                        },
                        "locs": "None",
                        "doc": [],
                        "type_": {
                          "Arrow": [
                            "None",
                            {
                              "Constr": [
                                {
                                  "`Resolved": {
                                    "`Identifier": {
                                      "`Type": [
                                        {
                                          "`Parameter": [
                                            {
                                              "`Module": [
                                                {
                                                  "`Module": [
                                                    {
                                                      "`Module": [
                                                        {
                                                          "`Root": [
                                                            "None", "Test"
                                                          ]
                                                        },
                                                        "In_functor_parameter"
                                                      ]
                                                    },
                                                    "P"
                                                  ]
                                                },
                                                "G"
                                              ]
                                            },
                                            "X"
                                          ]
                                        },
                                        "t"
                                      ]
                                    }
                                  }
                                },
                                []
                              ]
                            },
                            {
                              "Arrow": [
                                "None",
                                {
                                  "Constr": [
                                    {
                                      "`Resolved": {
                                        "`Identifier": {
                                          "`Type": [
                                            {
                                              "`Parameter": [
                                                {
                                                  "`Module": [
                                                    {
                                                      "`Module": [
                                                        {
                                                          "`Module": [
                                                            {
                                                              "`Root": [
                                                                "None", "Test"
                                                              ]
                                                            },
                                                            "In_functor_parameter"
                                                          ]
                                                        },
                                                        "P"
                                                      ]
                                                    },
                                                    "G"
                                                  ]
                                                },
                                                "X"
                                              ]
                                            },
                                            "t"
                                          ]
                                        }
                                      }
                                    },
                                    []
                                  ]
                                },
                                {
                                  "Constr": [
                                    {
                                      "`Resolved": {
                                        "`Identifier": {
                                          "`Type": [
                                            {
                                              "`Parameter": [
                                                {
                                                  "`Module": [
                                                    {
                                                      "`Module": [
                                                        {
                                                          "`Module": [
                                                            {
                                                              "`Root": [
                                                                "None", "Test"
                                                              ]
                                                            },
                                                            "In_functor_parameter"
                                                          ]
                                                        },
                                                        "P"
                                                      ]
                                                    },
                                                    "G"
                                                  ]
                                                },
                                                "X"
                                              ]
                                            },
                                            "t"
                                          ]
                                        }
                                      }
                                    },
                                    []
                                  ]
                                }
                              ]
                            }
                          ]
                        },
                        "value": "Abstract"
                      }
                    }
                  ],
                  "compiled": "true",
                  "doc": []
                }
              }
            }
          },
          {
            "Signature": {
              "items": [
                {
                  "Type": [
                    "Ordinary",
                    {
                      "id": {
                        "`Type": [
                          {
                            "`Result": {
                              "`Module": [
                                {
                                  "`Module": [
                                    {
                                      "`Module": [
                                        { "`Root": [ "None", "Test" ] },
                                        "In_functor_parameter"
                                      ]
                                    },
                                    "P"
                                  ]
                                },
                                "G"
                              ]
                            }
                          },
                          "t"
                        ]
                      },
                      "locs": "None",
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
          }
        ]
      }
    },
    "canonical": "None",
    "hidden": "false"
  }
  $ odoc link test.odoc
  $ odoc html-generate test.odocl -o html
  $ odoc support-files -o html
  $ cp -a html /tmp/test-html
