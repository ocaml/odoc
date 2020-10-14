Two modules that reference each other:

  $ cat a.mli
  (** {!B} *)
  $ cat b.mli
  (** {!A} *)

  $ compile a.mli b.mli
  Starting link
  Starting link

Check that references are resolved:

  $ odoc_print a.odocl | jq '.doc[0][1]."`Paragraph"[0][1]'
  {
    "`Reference": [
      {
        "`Resolved": {
          "`Identifier": {
            "`Root": [
              "<root>",
              "B"
            ]
          }
        }
      },
      []
    ]
  }
  $ odoc_print b.odocl | jq '.doc[0][1]."`Paragraph"[0][1]'
  {
    "`Reference": [
      {
        "`Resolved": {
          "`Identifier": {
            "`Root": [
              "<root>",
              "A"
            ]
          }
        }
      },
      []
    ]
  }
