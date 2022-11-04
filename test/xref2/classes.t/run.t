Check that types referring to class types (with `#` in them)
resolve correctly. All of the 'Class' json objects should contain
'Resolved'

  $ ocamlc -c -bin-annot b.ml
  $ ocamlc -c -bin-annot c.ml
  $ ocamlc -c -bin-annot d.mli
  $ ocamlc -c -bin-annot e.mli
  $ ocamlc -c -bin-annot f.mli

  $ odoc compile b.cmt
  $ odoc compile c.cmt -I .
  $ odoc compile d.cmti -I .
  File "d.cmti":
  Warning: No implementation file found for the given interface
  $ odoc compile e.cmti -I .
  File "e.cmti":
  Warning: No implementation file found for the given interface
  $ odoc compile f.cmti -I .
  File "f.cmti":
  Warning: No implementation file found for the given interface
  $ odoc_print -r f f.odoc 
  {
    "id": { "`Value": [ { "`Root": [ "None", "F" ] }, "f" ] },
    "locs": {
      "impl": "None",
      "intf": { "Some": "File \"f.mli\", line 3, characters 0-10" }
    },
    "doc": [],
    "type_": {
      "Class": [
        {
          "`Resolved": {
            "`Identifier": {
              "`ClassType": [ { "`Root": [ "None", "F" ] }, "u" ]
            }
          }
        },
        []
      ]
    },
    "value": "Abstract"
  }
  $ odoc_print e.odoc -r g
  {
    "id": { "`Value": [ { "`Root": [ "None", "E" ] }, "g" ] },
    "locs": {
      "impl": "None",
      "intf": { "Some": "File \"e.mli\", line 6, characters 0-12" }
    },
    "doc": [],
    "type_": {
      "Class": [
        {
          "`Resolved": {
            "`ClassType": [
              { "`Identifier": { "`Root": [ "None", "B" ] } },
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

