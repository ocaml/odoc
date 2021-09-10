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
  
  ocamlc -c -bin-annot test.ml
  odoc compile page.mld --child module-test
  odoc compile -I . --parent page test.cmt
  odoc link -I . test.odoc
  odoc html-generate --indent -o html test.odocl
  odoc support-files -o html
  

  $ ./compile.sh

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

