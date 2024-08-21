  $ echo "{0 Test}" > file.mld

It is possible to have pages with no parent, even with the odoc 3 "parent-id"
argument. In this case, an empty string is passed as argument.

  $ odoc compile --parent-id "" --output-dir _odoc file.mld
  $ ls _odoc
  page-file.odoc
  $ odoc_print _odoc/page-file.odoc | jq ".name"
  {
    "`LeafPage": [
      "None",
      "file"
    ]
  }
