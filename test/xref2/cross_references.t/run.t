Two modules that reference each other:

  $ cat a.mli
  type t
  (** {!B} *)
  $ cat b.mli
  type t
  (** {!A} *)

  $ compile a.mli b.mli
  File "a.cmti":
  Warning: No implementation file found for the given interface
  File "b.cmti":
  Warning: No implementation file found for the given interface

Check that references are resolved:

  $ odoc_print a.odocl | jq '.content.Module.items[0].Type[1].doc[0]'
  {
    "`Paragraph": [
      {
        "`Reference": [
          {
            "`Resolved": {
              "`Identifier": {
                "`Root": [
                  {
                    "Some": {
                      "`Page": [
                        "None",
                        "test"
                      ]
                    }
                  },
                  "B"
                ]
              }
            }
          },
          []
        ]
      }
    ]
  }
  $ odoc_print b.odocl | jq '.content.Module.items[0].Type[1].doc[0]'
  {
    "`Paragraph": [
      {
        "`Reference": [
          {
            "`Resolved": {
              "`Identifier": {
                "`Root": [
                  {
                    "Some": {
                      "`Page": [
                        "None",
                        "test"
                      ]
                    }
                  },
                  "A"
                ]
              }
            }
          },
          []
        ]
      }
    ]
  }
