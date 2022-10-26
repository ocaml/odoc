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
              "`Module": [
                {
                  "`Root": [
                    "None",
                    "M"
                  ]
                },
                "A"
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
        "`Type": [
          {
            "`Identifier": {
              "`Module": [
                {
                  "`Root": [
                    "None",
                    "M"
                  ]
                },
                "B"
              ]
            }
          },
          "t"
        ]
      }
    },
    []
  ]
  [
    {
      "`Resolved": {
        "`Type": [
          {
            "`Identifier": {
              "`Module": [
                {
                  "`Root": [
                    "None",
                    "M"
                  ]
                },
                "A"
              ]
            }
          },
          "t"
        ]
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
              "`Module": [
                {
                  "`Root": [
                    "None",
                    "M"
                  ]
                },
                "B"
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
        "`Type": [
          {
            "`Identifier": {
              "`Module": [
                {
                  "`Root": [
                    "None",
                    "M"
                  ]
                },
                "A"
              ]
            }
          },
          "t"
        ]
      }
    },
    []
  ]
  [
    {
      "`Resolved": {
        "`Type": [
          {
            "`Identifier": {
              "`Module": [
                {
                  "`Root": [
                    "None",
                    "M"
                  ]
                },
                "B"
              ]
            }
          },
          "t"
        ]
      }
    },
    []
  ]
