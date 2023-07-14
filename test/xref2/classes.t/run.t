Check that types referring to class types (with `#` in them)
resolve correctly. All of the 'Class' json objects should contain
'Resolved'

  $ ocamlc -c -bin-annot b.ml
  $ ocamlc -c -bin-annot c.ml -dtypedtree
  [
    structure_item (c.ml[1,0+0]..c.ml[4,65+3])
      Tstr_class_type
      [
        class_type_declaration (c.ml[1,0+0]..c.ml[4,65+3])
          pci_virt = Concrete
          pci_params =
            []
          pci_name = "d"
          pci_expr =
            class_type (c.ml[1,0+15]..c.ml[4,65+3])
              Tcty_signature
              class_signature
                core_type (c.ml[1,0+21]..c.ml[1,0+21])
                  Ttyp_any
                [
                  class_type_field (c.ml[2,22+2]..c.ml[2,22+16])
                    Tctf_method "e" Public Concrete
                      core_type (c.ml[2,22+2]..c.ml[2,22+16])
                        Ttyp_poly
                        core_type (c.ml[2,22+13]..c.ml[2,22+16])
                          Ttyp_constr "B!.t"
                          []
                  class_type_field (c.ml[3,39+2]..c.ml[3,39+25])
                    Tctf_method "f" Public Concrete
                      core_type (c.ml[3,39+2]..c.ml[3,39+25])
                        Ttyp_poly
                        core_type (c.ml[3,39+13]..c.ml[3,39+25])
                          Ttyp_arrow
                          Nolabel
                          core_type (c.ml[3,39+13]..c.ml[3,39+17])
                            Ttyp_class "B!.#u"
                            []
                          core_type (c.ml[3,39+21]..c.ml[3,39+25])
                            Ttyp_constr "unit/6!"
                            []
                ]
      ]
    structure_item (c.ml[5,69+0]..c.ml[5,69+26])
      Tstr_value Nonrec
      [
        <def>
          pattern (c.ml[5,69+4]..c.ml[5,69+5])
            extra
              Tpat_extra_constraint
              core_type (c.ml[5,69+8]..c.ml[5,69+11]) ghost
                Ttyp_poly
                core_type (c.ml[5,69+8]..c.ml[5,69+11])
                  Ttyp_constr "B!.t"
                  []
            Tpat_var "f/279"
          expression (c.ml[5,69+14]..c.ml[5,69+26])
            extra
              Texp_constraint
              core_type (c.ml[5,69+8]..c.ml[5,69+11])
                Ttyp_constr "B!.t"
                []
            Texp_assert          expression (c.ml[5,69+21]..c.ml[5,69+26])
              Texp_construct "false"
              []
      ]
    structure_item (c.ml[6,96+0]..c.ml[6,96+34])
      Tstr_value Nonrec
      [
        <def>
          pattern (c.ml[6,96+4]..c.ml[6,96+5])
            extra
              Tpat_extra_constraint
              core_type (c.ml[6,96+8]..c.ml[6,96+20]) ghost
                Ttyp_poly
                core_type (c.ml[6,96+8]..c.ml[6,96+20])
                  Ttyp_arrow
                  Nolabel
                  core_type (c.ml[6,96+8]..c.ml[6,96+12])
                    Ttyp_class "B!.#u"
                    []
                  core_type (c.ml[6,96+16]..c.ml[6,96+20])
                    Ttyp_constr "unit/6!"
                    []
            Tpat_var "g/280"
          expression (c.ml[6,96+23]..c.ml[6,96+34])
            extra
              Texp_constraint
              core_type (c.ml[6,96+8]..c.ml[6,96+20])
                Ttyp_arrow
                Nolabel
                core_type (c.ml[6,96+8]..c.ml[6,96+12])
                  Ttyp_class "B!.#u"
                  []
                core_type (c.ml[6,96+16]..c.ml[6,96+20])
                  Ttyp_constr "unit/6!"
                  []
            Texp_function
            Nolabel
            [
              <case>
                pattern (c.ml[6,96+27]..c.ml[6,96+28])
                  Tpat_any
                expression (c.ml[6,96+32]..c.ml[6,96+34])
                  Texp_construct "()"
                  []
            ]
      ]
    structure_item (c.ml[7,131+0]..c.ml[7,131+33])
      Tstr_value Nonrec
      [
        <def>
          pattern (c.ml[7,131+4]..c.ml[7,131+5])
            extra
              Tpat_extra_constraint
              core_type (c.ml[7,131+8]..c.ml[7,131+19]) ghost
                Ttyp_poly
                core_type (c.ml[7,131+8]..c.ml[7,131+19])
                  Ttyp_arrow
                  Nolabel
                  core_type (c.ml[7,131+8]..c.ml[7,131+11])
                    Ttyp_constr "B!.u"
                    []
                  core_type (c.ml[7,131+15]..c.ml[7,131+19])
                    Ttyp_constr "unit/6!"
                    []
            Tpat_var "h/282"
          expression (c.ml[7,131+22]..c.ml[7,131+33])
            extra
              Texp_constraint
              core_type (c.ml[7,131+8]..c.ml[7,131+19])
                Ttyp_arrow
                Nolabel
                core_type (c.ml[7,131+8]..c.ml[7,131+11])
                  Ttyp_constr "B!.u"
                  []
                core_type (c.ml[7,131+15]..c.ml[7,131+19])
                  Ttyp_constr "unit/6!"
                  []
            Texp_function
            Nolabel
            [
              <case>
                pattern (c.ml[7,131+26]..c.ml[7,131+27])
                  Tpat_any
                expression (c.ml[7,131+31]..c.ml[7,131+33])
                  Texp_construct "()"
                  []
            ]
      ]
    structure_item (c.ml[8,165+0]..c.ml[8,165+41])
      Tstr_value Nonrec
      [
        <def>
          pattern (c.ml[8,165+4]..c.ml[8,165+5])
            extra
              Tpat_extra_constraint
              core_type (c.ml[8,165+8]..c.ml[8,165+27]) ghost
                Ttyp_poly
                core_type (c.ml[8,165+8]..c.ml[8,165+27])
                  Ttyp_arrow
                  Nolabel
                  core_type (c.ml[8,165+8]..c.ml[8,165+19])
                    Ttyp_object Closed
                      method m
                        core_type (c.ml[8,165+14]..c.ml[8,165+17])
                          Ttyp_poly
                          core_type (c.ml[8,165+14]..c.ml[8,165+17])
                            Ttyp_constr "B!.u"
                            []
                  core_type (c.ml[8,165+23]..c.ml[8,165+27])
                    Ttyp_constr "unit/6!"
                    []
            Tpat_var "i/284"
          expression (c.ml[8,165+30]..c.ml[8,165+41])
            extra
              Texp_constraint
              core_type (c.ml[8,165+8]..c.ml[8,165+27])
                Ttyp_arrow
                Nolabel
                core_type (c.ml[8,165+8]..c.ml[8,165+19])
                  Ttyp_object Closed
                    method m
                      core_type (c.ml[8,165+14]..c.ml[8,165+17])
                        Ttyp_poly
                        core_type (c.ml[8,165+14]..c.ml[8,165+17])
                          Ttyp_constr "B!.u"
                          []
                core_type (c.ml[8,165+23]..c.ml[8,165+27])
                  Ttyp_constr "unit/6!"
                  []
            Texp_function
            Nolabel
            [
              <case>
                pattern (c.ml[8,165+34]..c.ml[8,165+35])
                  Tpat_any
                expression (c.ml[8,165+39]..c.ml[8,165+41])
                  Texp_construct "()"
                  []
            ]
      ]
  ]
  
  $ ocamlc -c -bin-annot d.mli
  $ ocamlc -c -bin-annot e.mli
  $ ocamlc -c -bin-annot f.mli

  $ odoc compile b.cmt
  $ odoc compile c.cmt -I .
  $ odoc compile d.cmti -I .
  $ odoc compile e.cmti -I .
  $ odoc compile f.cmti -I .
  $ odoc_print -r f f.odoc | jq .
  {
    "id": {
      "`Value": [
        {
          "`Root": [
            "None",
            "F"
          ]
        },
        "f"
      ]
    },
    "locs": "None",
    "doc": [],
    "type_": {
      "Class": [
        {
          "`Resolved": {
            "`Identifier": {
              "`ClassType": [
                {
                  "`Root": [
                    "None",
                    "F"
                  ]
                },
                "u"
              ]
            }
          }
        },
        []
      ]
    },
    "value": "Abstract"
  }
  $ odoc_print e.odoc -r g | jq .
  {
    "id": {
      "`Value": [
        {
          "`Root": [
            "None",
            "E"
          ]
        },
        "g"
      ]
    },
    "locs": "None",
    "doc": [],
    "type_": {
      "Class": [
        {
          "`Resolved": {
            "`ClassType": [
              {
                "`Identifier": {
                  "`Root": [
                    "None",
                    "B"
                  ]
                }
              },
              "u"
            ]
          }
        },
        []
      ]
    },
    "value": "Abstract"
  }
  $ odoc_print e.odoc -r d | jq '.expr.Signature.items[1].Method.type_'
  {
    "Class": [
      {
        "`Resolved": {
          "`ClassType": [
            {
              "`Identifier": {
                "`Root": [
                  "None",
                  "B"
                ]
              }
            },
            "u"
          ]
        }
      },
      []
    ]
  }

  $ odoc_print c.odoc -r g | jq '.type_'
  {
    "Arrow": [
      "None",
      {
        "Class": [
          {
            "`Resolved": {
              "`ClassType": [
                {
                  "`Identifier": {
                    "`Root": [
                      "None",
                      "B"
                    ]
                  }
                },
                "u"
              ]
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
                "`CoreType": "unit"
              }
            }
          },
          []
        ]
      }
    ]
  }

