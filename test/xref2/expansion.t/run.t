  $ ocamlc -c -bin-annot test.mli
  $ odoc compile test.cmti
  $ odoc_print test.odoc
  {
    "id": { "`Root": [ "None", "Test" ] },
    "root": "<root>",
    "digest": "<digest>",
    "imports": [
      { "Unresolved": [ "CamlinternalFormatBasics", { "Some": "<digest>" } ] },
      { "Unresolved": [ "Stdlib", { "Some": "<digest>" } ] }
    ],
    "source": {
      "Some": {
        "file": "test.mli",
        "build_dir": "$TESTCASE_ROOT",
        "digest": "<digest>"
      }
    },
    "interface": "true",
    "hidden": "false",
    "content": {
      "Module": {
        "items": [
          { "Comment": "`Stop" },
          {
            "Module": [
              "Ordinary",
              {
                "id": { "`Module": [ { "`Root": [ "None", "Test" ] }, "H" ] },
                "source_loc": "None",
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
                                      { "`Root": [ "None", "Test" ] },
                                      "H"
                                    ]
                                  },
                                  "t"
                                ]
                              },
                              "source_loc": "None",
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
                "hidden": "true"
              }
            ]
          },
          {
            "Module": [
              "Ordinary",
              {
                "id": { "`Module": [ { "`Root": [ "None", "Test" ] }, "S" ] },
                "source_loc": "None",
                "doc": [],
                "type_": {
                  "ModuleType": {
                    "Signature": {
                      "items": [
                        {
                          "ModuleType": {
                            "id": {
                              "`ModuleType": [
                                {
                                  "`Module": [
                                    { "`Root": [ "None", "Test" ] },
                                    "S"
                                  ]
                                },
                                "X"
                              ]
                            },
                            "source_loc": "None",
                            "doc": [],
                            "canonical": "None",
                            "expr": {
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
                                                "`ModuleType": [
                                                  {
                                                    "`Module": [
                                                      {
                                                        "`Root": [
                                                          "None", "Test"
                                                        ]
                                                      },
                                                      "S"
                                                    ]
                                                  },
                                                  "X"
                                                ]
                                              },
                                              "M"
                                            ]
                                          },
                                          "source_loc": "None",
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
                                                                  "`ModuleType": [
                                                                    {
                                                                      "`Module": [
                                                                      {
                                                                      "`Root": [
                                                                      "None",
                                                                      "Test"
                                                                      ]
                                                                      },
                                                                      "S"
                                                                      ]
                                                                    },
                                                                    "X"
                                                                  ]
                                                                },
                                                                "M"
                                                              ]
                                                            },
                                                            "t"
                                                          ]
                                                        },
                                                        "source_loc": "None",
                                                        "doc": [],
                                                        "equation": {
                                                          "params": [],
                                                          "private_": "false",
                                                          "manifest": "None",
                                                          "constraints": []
                                                        },
                                                        "representation":
                                                          "None"
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
                        },
                        {
                          "Module": [
                            "Ordinary",
                            {
                              "id": {
                                "`Module": [
                                  {
                                    "`Module": [
                                      { "`Root": [ "None", "Test" ] },
                                      "S"
                                    ]
                                  },
                                  "X"
                                ]
                              },
                              "source_loc": "None",
                              "doc": [],
                              "type_": {
                                "ModuleType": {
                                  "With": {
                                    "w_substitutions": [
                                      {
                                        "TypeEq": [
                                          {
                                            "`Resolved": {
                                              "`Type": [
                                                {
                                                  "`Module": [
                                                    {
                                                      "`Root": {
                                                        "`ModuleType": {
                                                          "`Identifier": {
                                                            "`ModuleType": [
                                                              {
                                                                "`Module": [
                                                                  {
                                                                    "`Root": [
                                                                      "None",
                                                                      "Test"
                                                                    ]
                                                                  },
                                                                  "S"
                                                                ]
                                                              },
                                                              "X"
                                                            ]
                                                          }
                                                        }
                                                      }
                                                    },
                                                    "M"
                                                  ]
                                                },
                                                "t"
                                              ]
                                            }
                                          },
                                          {
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
                                          }
                                        ]
                                      }
                                    ],
                                    "w_expansion": {
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
                                                                "`Root": [
                                                                  "None",
                                                                  "Test"
                                                                ]
                                                              },
                                                              "S"
                                                            ]
                                                          },
                                                          "X"
                                                        ]
                                                      },
                                                      "M"
                                                    ]
                                                  },
                                                  "source_loc": "None",
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
                                                                      "`Root": [
                                                                      "None",
                                                                      "Test"
                                                                      ]
                                                                      },
                                                                      "S"
                                                                      ]
                                                                      },
                                                                      "X"
                                                                      ]
                                                                      },
                                                                      "M"
                                                                      ]
                                                                    },
                                                                    "t"
                                                                  ]
                                                                },
                                                                "source_loc":
                                                                  "None",
                                                                "doc": [],
                                                                "equation": {
                                                                  "params": [],
                                                                  "private_":
                                                                    "false",
                                                                  "manifest": {
                                                                    "Some": {
                                                                      "Constr": [
                                                                      {
                                                                      "`Resolved": {
                                                                      "`Identifier": {
                                                                      "`CoreType":
                                                                      "int"
                                                                      }
                                                                      }
                                                                      },
                                                                      []
                                                                      ]
                                                                    }
                                                                  },
                                                                  "constraints":
                                                                    []
                                                                },
                                                                "representation":
                                                                  "None"
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
                                    },
                                    "w_expr": {
                                      "Path": {
                                        "`Resolved": {
                                          "`Identifier": {
                                            "`ModuleType": [
                                              {
                                                "`Module": [
                                                  {
                                                    "`Root": [ "None", "Test" ]
                                                  },
                                                  "S"
                                                ]
                                              },
                                              "X"
                                            ]
                                          }
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
                        },
                        {
                          "Module": [
                            "Ordinary",
                            {
                              "id": {
                                "`Module": [
                                  {
                                    "`Module": [
                                      { "`Root": [ "None", "Test" ] },
                                      "S"
                                    ]
                                  },
                                  "Y"
                                ]
                              },
                              "source_loc": "None",
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
                                                          "`Root": [
                                                            "None", "Test"
                                                          ]
                                                        },
                                                        "S"
                                                      ]
                                                    },
                                                    "Y"
                                                  ]
                                                },
                                                "t"
                                              ]
                                            },
                                            "source_loc": "None",
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
                        },
                        {
                          "Module": [
                            "Ordinary",
                            {
                              "id": {
                                "`Module": [
                                  {
                                    "`Module": [
                                      { "`Root": [ "None", "Test" ] },
                                      "S"
                                    ]
                                  },
                                  "Z"
                                ]
                              },
                              "source_loc": "None",
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
                                                    "`Root": [ "None", "Test" ]
                                                  },
                                                  "S"
                                                ]
                                              },
                                              "Y"
                                            ]
                                          }
                                        }
                                      }
                                    },
                                    "t_original_path": {
                                      "`Identifier": [
                                        {
                                          "`Module": [
                                            {
                                              "`Module": [
                                                { "`Root": [ "None", "Test" ] },
                                                "S"
                                              ]
                                            },
                                            "Y"
                                          ]
                                        },
                                        "false"
                                      ]
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
                                                                "`Root": [
                                                                  "None",
                                                                  "Test"
                                                                ]
                                                              },
                                                              "S"
                                                            ]
                                                          },
                                                          "Z"
                                                        ]
                                                      },
                                                      "t"
                                                    ]
                                                  },
                                                  "source_loc": "None",
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
                          ]
                        },
                        {
                          "Module": [
                            "Ordinary",
                            {
                              "id": {
                                "`Module": [
                                  {
                                    "`Module": [
                                      { "`Root": [ "None", "Test" ] },
                                      "S"
                                    ]
                                  },
                                  "A"
                                ]
                              },
                              "source_loc": "None",
                              "doc": [],
                              "type_": {
                                "ModuleType": {
                                  "Path": {
                                    "p_expansion": {
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
                                                                "`Root": [
                                                                  "None",
                                                                  "Test"
                                                                ]
                                                              },
                                                              "S"
                                                            ]
                                                          },
                                                          "A"
                                                        ]
                                                      },
                                                      "M"
                                                    ]
                                                  },
                                                  "source_loc": "None",
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
                                                                      "`Root": [
                                                                      "None",
                                                                      "Test"
                                                                      ]
                                                                      },
                                                                      "S"
                                                                      ]
                                                                      },
                                                                      "A"
                                                                      ]
                                                                      },
                                                                      "M"
                                                                      ]
                                                                    },
                                                                    "t"
                                                                  ]
                                                                },
                                                                "source_loc":
                                                                  "None",
                                                                "doc": [],
                                                                "equation": {
                                                                  "params": [],
                                                                  "private_":
                                                                    "false",
                                                                  "manifest":
                                                                    "None",
                                                                  "constraints":
                                                                    []
                                                                },
                                                                "representation":
                                                                  "None"
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
                                    },
                                    "p_path": {
                                      "`Resolved": {
                                        "`Identifier": {
                                          "`ModuleType": [
                                            {
                                              "`Module": [
                                                { "`Root": [ "None", "Test" ] },
                                                "S"
                                              ]
                                            },
                                            "X"
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
                        },
                        {
                          "Module": [
                            "Ordinary",
                            {
                              "id": {
                                "`Module": [
                                  {
                                    "`Module": [
                                      { "`Root": [ "None", "Test" ] },
                                      "S"
                                    ]
                                  },
                                  "B"
                                ]
                              },
                              "source_loc": "None",
                              "doc": [],
                              "type_": {
                                "Alias": [
                                  {
                                    "`Resolved": {
                                      "`Hidden": {
                                        "`Hidden": {
                                          "`Identifier": {
                                            "`Module": [
                                              { "`Root": [ "None", "Test" ] },
                                              "H"
                                            ]
                                          }
                                        }
                                      }
                                    }
                                  },
                                  "None"
                                ]
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
                "hidden": "true"
              }
            ]
          },
          { "Comment": "`Stop" },
          {
            "Module": [
              "Ordinary",
              {
                "id": {
                  "`Module": [ { "`Root": [ "None", "Test" ] }, "Test" ]
                },
                "source_loc": "None",
                "doc": [],
                "type_": {
                  "Alias": [
                    {
                      "`Resolved": {
                        "`Hidden": {
                          "`Hidden": {
                            "`Identifier": {
                              "`Module": [
                                { "`Root": [ "None", "Test" ] },
                                "S"
                              ]
                            }
                          }
                        }
                      }
                    },
                    "None"
                  ]
                },
                "canonical": "None",
                "hidden": "false"
              }
            ]
          }
        ],
        "compiled": "true",
        "doc": [ { "`Paragraph": [ { "`Word": "testing" } ] } ]
      }
    },
    "expansion": "None",
    "canonical": "None"
  }
  $ odoc link test.odoc
  $ odoc html-generate -o /tmp/html test.odocl
  $ odoc support-files -o /tmp/html
