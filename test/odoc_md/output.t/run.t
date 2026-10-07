[odoc-md -o] should write the page where [-o] says. The page's identifier
should still come from [--parent-id] and the input's name: a page [readme]
below [pkg]. An identifier taken from the [-o] path would be wrong.

  $ cat readme.md
  # Readme
  
  Hello.
  $ odoc-md --parent-id pkg -o custom/page-readme.odoc readme.md
  $ ls custom
  page-readme.odoc
  $ odoc_print custom/page-readme.odoc | jq -c ".name"
  {"`LeafPage":[{"Some":{"`Page":["None","pkg"]}},"readme"]}

With neither [-o] nor [--output-dir], [odoc-md] should fail, saying that one
of them is needed.

  $ odoc-md --parent-id pkg readme.md
  Either -o or --output-dir must be given.
  [1]
