  $ ocamlc -bin-annot test.ml

  $ mkdir h
  $ odoc compile --output-dir h --parent-id pkg doc/foo.mld
  $ odoc compile --output-dir h --parent-id pkg doc/dup.mld
  $ odoc compile --output-dir h --parent-id pkg/subdir doc/subdir/bar.mld
  $ odoc compile --output-dir h --parent-id pkg/subdir doc/subdir/dup.mld
  $ odoc compile --output-dir h --parent-id pkg/libname test.cmt
  $ odoc compile-asset --output-dir h --parent-id pkg --name img.png

  $ odoc link -P pkg:h/pkg -L libname:h/pkg/libname h/pkg/subdir/page-dup.odoc
  $ odoc link -P pkg:h/pkg -L libname:h/pkg/libname h/pkg/subdir/page-bar.odoc
  File "doc/subdir/bar.mld", line 12, characters 39-48:
  Warning: Failed to resolve reference ./Test Path 'Test' not found
  File "doc/subdir/bar.mld", line 12, characters 18-38:
  Warning: Failed to resolve reference /pkg/libname/Test Path '/pkg/libname/Test' not found
  File "doc/subdir/bar.mld", line 12, characters 0-17:
  Warning: Failed to resolve reference //libname/Test Path '//libname/Test' not found
  $ odoc link -P pkg:h/pkg -L libname:h/pkg/libname h/pkg/page-dup.odoc
  $ odoc link -P pkg:h/pkg -L libname:h/pkg/libname h/pkg/page-foo.odoc
  File "doc/foo.mld", line 12, characters 27-36:
  Warning: Failed to resolve reference ./Test Path 'Test' not found
  File "doc/foo.mld", line 12, characters 0-9:
  Warning: Failed to resolve reference //Test Path '//Test' not found
  File "doc/foo.mld", line 6, characters 35-41:
  Warning: Failed to resolve reference unresolvedroot(bar) Couldn't find "bar"
  $ odoc link -P pkg:h/pkg -L libname:h/pkg/libname h/pkg/libname/test.odoc
  File "test.ml", line 6, characters 38-44:
  Warning: Failed to resolve reference unresolvedroot(bar) Couldn't find "bar"

Helper that extracts references in a compact way. Headings help to interpret the result.

  $ jq_references() { jq -c '.. | objects | if has("`Reference") then . elif has("`Heading") then [ .. | .["`Word"]? | select(.) ] else empty end'; }

  $ odoc_print ./h/pkg/page-foo.odocl | jq_references
  ["Title","for","foo"]
  ["Page","foo"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":["None","pkg"]}},"foo"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":["None","pkg"]}},"foo"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":["None","pkg"]}},"foo"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":["None","pkg"]}},"foo"]}}},[]]}
  ["Page","subdir/bar"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"subdir"]}},"bar"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"subdir"]}},"bar"]}}},[]]}
  {"`Reference":[{"`Root":["bar","`TUnknown"]},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"subdir"]}},"bar"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"subdir"]}},"bar"]}}},[]]}
  ["Page","dup"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":["None","pkg"]}},"dup"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":["None","pkg"]}},"dup"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":["None","pkg"]}},"dup"]}}},[]]}
  ["Page","subdir/dup"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"subdir"]}},"dup"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"subdir"]}},"dup"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"subdir"]}},"dup"]}}},[]]}
  ["Module","Test"]
  {"`Reference":[{"`Any_path":["`TCurrentPackage",["Test"]]},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`Root":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"libname"]}},"Test"]}}},[]]}
  {"`Reference":[{"`Any_path":["`TRelativePath",["Test"]]},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`Root":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"libname"]}},"Test"]}}},[]]}
  ["Asset"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`AssetFile":[{"`Page":["None","pkg"]},"img.png"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`AssetFile":[{"`Page":["None","pkg"]},"img.png"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`AssetFile":[{"`Page":["None","pkg"]},"img.png"]}}},[]]}

  $ odoc_print ./h/pkg/subdir/page-bar.odocl | jq_references
  ["Title","for","subdir/bar"]
  ["Page","foo"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":["None","pkg"]}},"foo"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":["None","pkg"]}},"foo"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":["None","pkg"]}},"foo"]}}},[]]}
  ["Page","subdir/bar"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"subdir"]}},"bar"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"subdir"]}},"bar"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"subdir"]}},"bar"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"subdir"]}},"bar"]}}},[]]}
  ["Page","dup"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":["None","pkg"]}},"dup"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":["None","pkg"]}},"dup"]}}},[]]}
  ["Page","subdir/dup"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"subdir"]}},"dup"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"subdir"]}},"dup"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"subdir"]}},"dup"]}}},[]]}
  ["Module","Test"]
  {"`Reference":[{"`Any_path":["`TCurrentPackage",["libname","Test"]]},[]]}
  {"`Reference":[{"`Any_path":["`TAbsolutePath",["pkg","libname","Test"]]},[]]}
  {"`Reference":[{"`Any_path":["`TRelativePath",["Test"]]},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`Root":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"libname"]}},"Test"]}}},[]]}

  $ odoc_print ./h/pkg/libname/test.odocl | jq_references
  ["Page","foo"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":["None","pkg"]}},"foo"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":["None","pkg"]}},"foo"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":["None","pkg"]}},"foo"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":["None","pkg"]}},"foo"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":["None","pkg"]}},"foo"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":["None","pkg"]}},"foo"]}}},[]]}
  ["Page","subdir/bar"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"subdir"]}},"bar"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"subdir"]}},"bar"]}}},[]]}
  {"`Reference":[{"`Root":["bar","`TUnknown"]},[]]}
  ["Page","dup"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":["None","pkg"]}},"dup"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":["None","pkg"]}},"dup"]}}},[]]}
  ["Page","subdir/dup"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"subdir"]}},"dup"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"subdir"]}},"dup"]}}},[]]}
  ["Module","Test"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`Root":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"libname"]}},"Test"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`Root":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"libname"]}},"Test"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`Root":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"libname"]}},"Test"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`Root":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"libname"]}},"Test"]}}},[]]}
