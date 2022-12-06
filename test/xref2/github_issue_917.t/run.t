# References to pages and items when page and module names are the same.

  $ compile foo.mld foo.mli
  File "foo.mld", line 5, characters 7-20:
  Warning: Failed to resolve reference unresolvedroot(foo) Parent_module: Lookup failure (root module): foo

  $ jq_scan_references() { jq -c '.. | .["`Reference"]? | select(.) | .[0]'; }

Every references in `page-foo.odocl` should resolve:

  $ odoc_print page-foo.odocl | jq_scan_references
  {"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":["None","test"]}},"foo"]}}}
  {"`Root":["foo","`TModule"]}
  {"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":["None","test"]}},"foo"]}}}

Every references in `foo.odocl` should resolve:

  $ odoc_print foo.odocl | jq_scan_references
  {"`Resolved":{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Foo"]}}}
  {"`Resolved":{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Foo"]}}}
  {"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":["None","test"]}},"foo"]}}}
