This test checks that when we have expressions of the form `sig ... end with ...` that we replace
them with simple `sig ... end` - any types or modules referred to in the `with` clauses will not
be able to link anywhere because we don't render the inner signature anywhere.

  $ cat test.ml
  module type S = sig
    type t
    type u
  end with type t = int with type u = int
  

  $ cat compile.sh
  #!/bin/sh
  
  ocamlc -c -bin-annot -dtypedtree test.ml
  odoc compile page.mld --child module-test
  odoc compile -I . --parent page test.cmt
  odoc link -I . test.odoc
  odoc html-generate --indent -o html test.odocl
  odoc support-files -o html
  

  $ ./compile.sh
  [
    structure_item (test.ml[1,0+0]..test.ml[4,38+39])
      Tstr_modtype "S/276"
        module_type (test.ml[1,0+16]..test.ml[4,38+39])
          Tmty_with
          module_type (test.ml[1,0+16]..test.ml[4,38+21])
            Tmty_with
            module_type (test.ml[1,0+16]..test.ml[4,38+3])
              Tmty_signature
              [
                signature_item (test.ml[2,20+2]..test.ml[2,20+8])
                  Tsig_type Rec
                  [
                    type_declaration t/268 (test.ml[2,20+2]..test.ml[2,20+8])
                      ptype_params =
                        []
                      ptype_cstrs =
                        []
                      ptype_kind =
                        Ttype_abstract
                      ptype_private = Public
                      ptype_manifest =
                        None
                  ]
                signature_item (test.ml[3,29+2]..test.ml[3,29+8])
                  Tsig_type Rec
                  [
                    type_declaration u/269 (test.ml[3,29+2]..test.ml[3,29+8])
                      ptype_params =
                        []
                      ptype_cstrs =
                        []
                      ptype_kind =
                        Ttype_abstract
                      ptype_private = Public
                      ptype_manifest =
                        None
                  ]
              ]
            [
              "t/268"
                Twith_type
                  type_declaration t/268 (test.ml[4,38+9]..test.ml[4,38+21])
                    ptype_params =
                      []
                    ptype_cstrs =
                      []
                    ptype_kind =
                      Ttype_abstract
                    ptype_private = Public
                    ptype_manifest =
                      Some
                        core_type (test.ml[4,38+18]..test.ml[4,38+21])
                          Ttyp_constr "int/1!"
                          []
            ]
          [
            "u/272"
              Twith_type
                type_declaration u/272 (test.ml[4,38+27]..test.ml[4,38+39])
                  ptype_params =
                    []
                  ptype_cstrs =
                    []
                  ptype_kind =
                    Ttype_abstract
                  ptype_private = Public
                  ptype_manifest =
                    Some
                      core_type (test.ml[4,38+36]..test.ml[4,38+39])
                        Ttyp_constr "int/1!"
                        []
          ]
  ]
  

Let's check which module type `.content.Module.items[0].ModuleType` refers to:

  $ odoc_print test.odocl | jq '.content.Module.items[0].ModuleType.id' 
  {
    "`ModuleType": [
      {
        "`Root": [
          {
            "Some": {
              "`Page": [
                "None",
                "page"
              ]
            }
          },
          "Test"
        ]
      },
      "S"
    ]
  }

And it ought to still be a With after compiling and linking.

  $ odoc_print test.odocl | jq '.content.Module.items[0].ModuleType.expr.Some | keys'
  [
    "With"
  ]

