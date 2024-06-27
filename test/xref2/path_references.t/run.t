  $ ocamlc -bin-annot test.ml

  $ mkdir h
  $ odoc compile --output-dir h --parent-id pkg/doc doc/foo.mld
  $ odoc compile --output-dir h --parent-id pkg/doc doc/dup.mld
  $ odoc compile --output-dir h --parent-id pkg/doc/subdir doc/subdir/bar.mld
  $ odoc compile --output-dir h --parent-id pkg/doc/subdir doc/subdir/dup.mld
  $ odoc compile --output-dir h --parent-id pkg/lib/libname test.cmt

  $ odoc link -P pkg:h/pkg/doc -L libname:h/pkg/lib/libname h/pkg/doc/subdir/page-dup.odoc
  $ odoc link -P pkg:h/pkg/doc -L libname:h/pkg/lib/libname h/pkg/doc/subdir/page-bar.odoc
  File "h/pkg/doc/subdir/page-Test.odoc":
  File does not exist
  File "doc/subdir/bar.mld", line 12, characters 49-56:
  Warning: Failed to resolve reference unresolvedroot(Test) Couldn't find "Test"
  File "doc/subdir/bar.mld", line 12, characters 39-48:
  Warning: Failed to resolve reference ./Test Path 'Test' not found
  File "doc/subdir/bar.mld", line 12, characters 18-38:
  Warning: Failed to resolve reference /pkg/libname/Test Path '/pkg/libname/Test' not found
  File "doc/subdir/bar.mld", line 12, characters 0-17:
  Warning: Failed to resolve reference //libname/Test Path '//libname/Test' not found
  File "doc/subdir/bar.mld", line 4, characters 21-27:
  Warning: Failed to resolve reference unresolvedroot(foo) Couldn't find "foo"
  $ odoc link -P pkg:h/pkg/doc -L libname:h/pkg/lib/libname h/pkg/doc/page-dup.odoc
  $ odoc link -P pkg:h/pkg/doc -L libname:h/pkg/lib/libname h/pkg/doc/page-foo.odoc
  File "h/pkg/doc/page-Test.odoc":
  File does not exist
  File "doc/foo.mld", line 12, characters 49-56:
  Warning: Failed to resolve reference unresolvedroot(Test) Couldn't find "Test"
  File "doc/foo.mld", line 12, characters 39-48:
  Warning: Failed to resolve reference ./Test Path 'Test' not found
  File "doc/foo.mld", line 12, characters 18-38:
  Warning: Failed to resolve reference /pkg/libname/Test Path '/pkg/libname/Test' not found
  File "doc/foo.mld", line 12, characters 0-17:
  Warning: Failed to resolve reference //libname/Test Path '//libname/Test' not found
  File "doc/foo.mld", line 6, characters 35-41:
  Warning: Failed to resolve reference unresolvedroot(bar) Couldn't find "bar"
  $ odoc link --current-package pkg -P pkg:h/pkg/doc -L libname:h/pkg/lib/libname h/pkg/lib/libname/test.odoc
  File "test.ml", line 12, characters 42-51:
  Warning: Failed to resolve reference ./Test Path 'Test' not found
  File "test.ml", line 12, characters 21-41:
  Warning: Failed to resolve reference /pkg/libname/Test Path '/pkg/libname/Test' not found
  File "test.ml", line 12, characters 3-20:
  Warning: Failed to resolve reference //libname/Test Path '//libname/Test' not found
  File "test.ml", line 6, characters 38-44:
  Warning: Failed to resolve reference unresolvedroot(bar) Couldn't find "bar"
  File "test.ml", line 4, characters 34-45:
  Warning: Failed to resolve reference unresolvedroot(foo) Couldn't find page "foo"
  File "test.ml", line 3, characters 24-30:
  Warning: Failed to resolve reference unresolvedroot(foo) Couldn't find "foo"

Helper that extracts references in a compact way. Headings help to interpret the result.

  $ jq_references() { jq -c '.. | objects | if has("`Reference") then . elif has("`Heading") then [ .. | .["`Word"]? | select(.) ] else empty end'; }

  $ odoc_print ./h/pkg/doc/page-foo.odocl | jq_references
  ["Title","for","foo"]
  ["Page","foo"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"foo"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"foo"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"foo"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"foo"]}}},[]]}
  ["Page","subdir/bar"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"subdir"]}},"bar"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"subdir"]}},"bar"]}}},[]]}
  {"`Reference":[{"`Root":["bar","`TUnknown"]},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"subdir"]}},"bar"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"subdir"]}},"bar"]}}},[]]}
  ["Page","dup"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"dup"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"dup"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"dup"]}}},[]]}
  ["Page","subdir/dup"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"subdir"]}},"dup"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"subdir"]}},"dup"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"subdir"]}},"dup"]}}},[]]}
  ["Module","Test"]
  {"`Reference":[{"`Any_path":["`TCurrentPackage",["libname","Test"]]},[]]}
  {"`Reference":[{"`Any_path":["`TAbsolutePath",["pkg","libname","Test"]]},[]]}
  {"`Reference":[{"`Any_path":["`TRelativePath",["Test"]]},[]]}
  {"`Reference":[{"`Root":["Test","`TUnknown"]},[]]}

  $ odoc_print ./h/pkg/doc/subdir/page-bar.odocl | jq_references
  ["Title","for","subdir/bar"]
  ["Page","foo"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"foo"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"foo"]}}},[]]}
  {"`Reference":[{"`Root":["foo","`TUnknown"]},[]]}
  ["Page","subdir/bar"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"subdir"]}},"bar"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"subdir"]}},"bar"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"subdir"]}},"bar"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"subdir"]}},"bar"]}}},[]]}
  ["Page","dup"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"dup"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"dup"]}}},[]]}
  ["Page","subdir/dup"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"subdir"]}},"dup"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"subdir"]}},"dup"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"subdir"]}},"dup"]}}},[]]}
  ["Module","Test"]
  {"`Reference":[{"`Any_path":["`TCurrentPackage",["libname","Test"]]},[]]}
  {"`Reference":[{"`Any_path":["`TAbsolutePath",["pkg","libname","Test"]]},[]]}
  {"`Reference":[{"`Any_path":["`TRelativePath",["Test"]]},[]]}
  {"`Reference":[{"`Root":["Test","`TUnknown"]},[]]}

  $ odoc_print ./h/pkg/lib/libname/test.odocl | jq_references
  ["Page","foo"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"foo"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"foo"]}}},[]]}
  {"`Reference":[{"`Root":["foo","`TUnknown"]},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"foo"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"foo"]}}},[]]}
  {"`Reference":[{"`Root":["foo","`TPage"]},[]]}
  ["Page","subdir/bar"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"subdir"]}},"bar"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"subdir"]}},"bar"]}}},[]]}
  {"`Reference":[{"`Root":["bar","`TUnknown"]},[]]}
  ["Page","dup"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"dup"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"dup"]}}},[]]}
  ["Page","subdir/dup"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"subdir"]}},"dup"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"subdir"]}},"dup"]}}},[]]}
  ["Module","Test"]
  {"`Reference":[{"`Any_path":["`TCurrentPackage",["libname","Test"]]},[]]}
  {"`Reference":[{"`Any_path":["`TAbsolutePath",["pkg","libname","Test"]]},[]]}
  {"`Reference":[{"`Any_path":["`TRelativePath",["Test"]]},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`Root":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"lib"]}},"libname"]}},"Test"]}}},[]]}
