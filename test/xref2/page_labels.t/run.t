  $ odoc compile page.mld
  $ odoc link page-page.odoc
  $ odoc_print page-page.odocl | jq '.content[1]["`Paragraph"][0]["`Reference"][0]'
  {
    "`Resolved": {
      "`Identifier": {
        "`Label": [
          {
            "`LeafPage": [
              "None",
              "page"
            ]
          },
          "test"
        ]
      }
    }
  }


