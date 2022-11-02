All of the paths should be resolved after just the compile step

  $ ocamlc -c -bin-annot m.mli
  $ odoc compile m.cmti
  File "m.cmti":
  Warning: No implementation file found for the given interface
  $ odoc_print m.odoc | jq '.. | .["Constr"]? | select(.)'
  [
    {
      "`Resolved": {
        "`Identifier": {
          "`Type": [
            {
              "`Root": [
                "None",
                "M"
              ]
            },
            "t"
          ]
        }
      }
    },
    []
  ]
  [
    {
      "`Resolved": {
        "`Identifier": {
          "`Type": [
            {
              "`Root": [
                "None",
                "M"
              ]
            },
            "y"
          ]
        }
      }
    },
    []
  ]
  [
    {
      "`Resolved": {
        "`Identifier": {
          "`Type": [
            {
              "`Root": [
                "None",
                "M"
              ]
            },
            "x"
          ]
        }
      }
    },
    []
  ]
