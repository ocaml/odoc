  $ ocamlc -c -bin-annot test.ml
  $ odoc compile test.cmt
  $ odoc link test.odoc
  $ odoc html-generate -o html test.odocl
  $ odoc support-files -o html

We should have an 'Optional' argument (as opposed to a 'RawOptional' one)

  $ odoc_print -r f test.odocl
  {
    "id": { "`Value": [ { "`Root": [ "None", "Test" ] }, "f" ] },
    "source_loc": "None",
    "doc": { "elements": [], "suppress_warnings": "false" },
    "type_": {
      "Arrow": [
        { "Some": { "Optional": "optional" } },
        { "Constr": [ { "`Resolved": { "`CoreType": "int" } }, [] ] },
        {
          "Arrow": [
            "None",
            { "Constr": [ { "`Resolved": { "`CoreType": "unit" } }, [] ] },
            { "Var": "a" }
          ]
        }
      ]
    },
    "value": "Abstract"
  }
