  $ ocamlc -c -bin-annot test.mli
  $ odoc compile test.cmti
  $ odoc link test.odoc
  $ odoc html-generate test.odocl -o html
  $ odoc support-files -o html

The following should be resolved as identifier Test.A
  $ odoc_print -r test test.odocl | jq '.equation.manifest.Some.Constr[0]["`Resolved"]["`Type"][0]["`Canonical"][1]'
  {
    "`Resolved": {
      "`Identifier": {
        "`Module": [
          {
            "`Root": [
              "None",
              "Test"
            ]
          },
          "A"
        ]
      }
    }
  }

The following should be resolved as Test.Wrapper.X

  $ odoc_print -r test2 test.odocl | jq '.equation.manifest.Some.Constr[0]["`Resolved"]["`Type"][0]["`Canonical"][1]'
  {
    "`Resolved": {
      "`Module": [
        {
          "`Identifier": {
            "`Module": [
              {
                "`Root": [
                  "None",
                  "Test"
                ]
              },
              "Wrapper"
            ]
          }
        },
        "X"
      ]
    }
  }

The following should be resolved as Test.Wrapper2.X


  $ odoc_print -r test3 test.odocl | jq '.equation.manifest.Some.Constr[0]["`Resolved"]["`Type"][0]["`Canonical"][1]'
  {
    "`Resolved": {
      "`Module": [
        {
          "`Identifier": {
            "`Module": [
              {
                "`Root": [
                  "None",
                  "Test"
                ]
              },
              "Wrapper2"
            ]
          }
        },
        "X"
      ]
    }
  }

This should probably not resolve at all, but that's a problem for another day. currently it resolves as Test.Wrapper3.X

  $ odoc_print -r test3a test.odocl | jq '.type_.Constr[0]["`Resolved"]["`Type"][0]["`Canonical"][1]'
  {
    "`Resolved": {
      "`Module": [
        {
          "`Identifier": {
            "`Module": [
              {
                "`Root": [
                  "None",
                  "Test"
                ]
              },
              "Wrapper3"
            ]
          }
        },
        "X"
      ]
    }
  }

Should resolve as identifier Test.B
  $ odoc_print -r test4 test.odocl | jq '.equation.manifest.Some.Constr[0]["`Resolved"]["`Type"][0]["`Canonical"][1]'
  {
    "`Resolved": {
      "`Identifier": {
        "`Module": [
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
  }

Should resove to be an alias!
  $ odoc_print -r test5 test.odocl | jq '.equation.manifest.Some.Constr[0]["`Resolved"]["`Type"][0]["`Canonical"][1]'
  {
    "`Resolved": {
      "`Alias": [
        {
          "`Identifier": {
            "`Module": [
              {
                "`Root": [
                  "None",
                  "Test"
                ]
              },
              "C_"
            ]
          }
        },
        {
          "`Identifier": [
            {
              "`Module": [
                {
                  "`Root": [
                    "None",
                    "Test"
                  ]
                },
                "C"
              ]
            },
            "false"
          ]
        }
      ]
    }
  }

