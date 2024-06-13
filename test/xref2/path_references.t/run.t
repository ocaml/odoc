  $ ocamlc -bin-annot test.ml

  $ mkdir h
  $ odoc compile --output-dir h --parent-id pkg/doc doc/foo.mld
  $ odoc compile --output-dir h --parent-id pkg/doc doc/dup.mld
  $ odoc compile --output-dir h --parent-id pkg/doc/subdir doc/subdir/bar.mld
  $ odoc compile --output-dir h --parent-id pkg/doc/subdir doc/subdir/dup.mld
  $ odoc compile --output-dir h --parent-id pkg/lib/libname test.cmt

  $ odoc link -P pkg:h/pkg/doc -L libname:h/pkg/lib/libname h/pkg/doc/subdir/page-dup.odoc
  $ odoc link -P pkg:h/pkg/doc -L libname:h/pkg/lib/libname h/pkg/doc/subdir/page-bar.odoc
  File "doc/subdir/bar.mld", line 12, characters 49-56:
  Warning: Failed to resolve reference unresolvedroot(Test) Couldn't find "Test"
  File "doc/subdir/bar.mld", line 12, characters 39-48:
  Warning: Failed to resolve reference ./Test Path 'Test' not found
  File "doc/subdir/bar.mld", line 12, characters 18-38:
  Warning: Failed to resolve reference /pkg/libname/Test Path '/pkg/libname/Test' not found
  File "doc/subdir/bar.mld", line 12, characters 0-17:
  Warning: Failed to resolve reference //libname/Test Path '//libname/Test' not found
  File "doc/subdir/bar.mld", line 10, characters 35-43:
  Warning: Failed to resolve reference ./dup Path 'dup' not found
  File "doc/subdir/bar.mld", line 6, characters 42-50:
  Warning: Failed to resolve reference ./bar Path 'bar' not found
  File "doc/subdir/bar.mld", line 4, characters 21-27:
  Warning: Failed to resolve reference unresolvedroot(foo) Couldn't find "foo"
  $ odoc link -P pkg:h/pkg/doc -L libname:h/pkg/lib/libname h/pkg/doc/page-dup.odoc
  $ odoc link -P pkg:h/pkg/doc -L libname:h/pkg/lib/libname h/pkg/doc/page-foo.odoc
  File "doc/foo.mld", line 12, characters 49-56:
  Warning: Failed to resolve reference unresolvedroot(Test) Couldn't find "Test"
  File "doc/foo.mld", line 12, characters 39-48:
  Warning: Failed to resolve reference ./Test Path 'Test' not found
  File "doc/foo.mld", line 12, characters 18-38:
  Warning: Failed to resolve reference /pkg/libname/Test Path '/pkg/libname/Test' not found
  File "doc/foo.mld", line 12, characters 0-17:
  Warning: Failed to resolve reference //libname/Test Path '//libname/Test' not found
  File "doc/foo.mld", line 10, characters 35-48:
  Warning: Failed to resolve reference ./subdir/dup Path 'subdir/dup' not found
  File "doc/foo.mld", line 8, characters 21-29:
  Warning: Failed to resolve reference ./dup Path 'dup' not found
  File "doc/foo.mld", line 6, characters 56-71:
  Warning: Failed to resolve reference ./subdir/bar Path 'subdir/bar' not found
  File "doc/foo.mld", line 6, characters 42-55:
  Warning: Failed to resolve reference ./subdir/bar Path 'subdir/bar' not found
  File "doc/foo.mld", line 6, characters 35-41:
  Warning: Failed to resolve reference unresolvedroot(bar) Couldn't find "bar"
  File "doc/foo.mld", line 4, characters 28-36:
  Warning: Failed to resolve reference ./foo Path 'foo' not found
  $ odoc link -P pkg:h/pkg/doc -L libname:h/pkg/lib/libname h/pkg/lib/libname/test.odoc
  File "test.ml", line 12, characters 42-51:
  Warning: Failed to resolve reference ./Test Path 'Test' not found
  File "test.ml", line 12, characters 21-41:
  Warning: Failed to resolve reference /pkg/libname/Test Path '/pkg/libname/Test' not found
  File "test.ml", line 12, characters 3-20:
  Warning: Failed to resolve reference //libname/Test Path '//libname/Test' not found
  File "test.ml", line 10, characters 3-18:
  Warning: Failed to resolve reference //subdir/dup Path '//subdir/dup' not found
  File "test.ml", line 8, characters 3-11:
  Warning: Failed to resolve reference //dup Path '//dup' not found
  File "test.ml", line 6, characters 38-44:
  Warning: Failed to resolve reference unresolvedroot(bar) Couldn't find "bar"
  File "test.ml", line 6, characters 3-18:
  Warning: Failed to resolve reference //subdir/bar Path '//subdir/bar' not found
  File "test.ml", line 4, characters 34-45:
  Warning: Failed to resolve reference unresolvedroot(foo) Couldn't find page "foo"
  File "test.ml", line 4, characters 3-16:
  Warning: Failed to resolve reference //foo Path '//foo' not found
  File "test.ml", line 3, characters 24-30:
  Warning: Failed to resolve reference unresolvedroot(foo) Couldn't find "foo"
  File "test.ml", line 3, characters 3-11:
  Warning: Failed to resolve reference //foo Path '//foo' not found

Helper that extracts references in a compact way. Headings help to interpret the result.

  $ jq_references() { jq -c '.. | objects | if has("`Reference") then . elif has("`Heading") then [ .. | .["`Word"]? | select(.) ] else empty end'; }

  $ odoc_print ./h/pkg/doc/page-foo.odocl | jq_references
  ["Title","for","foo"]
  ["Page","foo"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"foo"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"foo"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"foo"]}}},[]]}
  {"`Reference":[{"`Any_path":["`TRelativePath",["foo"]]},[]]}
  ["Page","subdir/bar"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"subdir"]}},"bar"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"subdir"]}},"bar"]}}},[]]}
  {"`Reference":[{"`Root":["bar","`TUnknown"]},[]]}
  {"`Reference":[{"`Any_path":["`TRelativePath",["subdir","bar"]]},[]]}
  {"`Reference":[{"`Any_path":["`TRelativePath",["subdir","bar"]]},[]]}
  ["Page","dup"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"dup"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"dup"]}}},[]]}
  {"`Reference":[{"`Any_path":["`TRelativePath",["dup"]]},[]]}
  ["Page","subdir/dup"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"subdir"]}},"dup"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"subdir"]}},"dup"]}}},[]]}
  {"`Reference":[{"`Any_path":["`TRelativePath",["subdir","dup"]]},[]]}
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
  {"`Reference":[{"`Any_path":["`TRelativePath",["bar"]]},[]]}
  ["Page","dup"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"dup"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"dup"]}}},[]]}
  ["Page","subdir/dup"]
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"subdir"]}},"dup"]}}},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"subdir"]}},"dup"]}}},[]]}
  {"`Reference":[{"`Any_path":["`TRelativePath",["dup"]]},[]]}
  ["Module","Test"]
  {"`Reference":[{"`Any_path":["`TCurrentPackage",["libname","Test"]]},[]]}
  {"`Reference":[{"`Any_path":["`TAbsolutePath",["pkg","libname","Test"]]},[]]}
  {"`Reference":[{"`Any_path":["`TRelativePath",["Test"]]},[]]}
  {"`Reference":[{"`Root":["Test","`TUnknown"]},[]]}

  $ odoc_print ./h/pkg/lib/libname/test.odocl | jq_references
  ["Page","foo"]
  {"`Reference":[{"`Any_path":["`TCurrentPackage",["foo"]]},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"foo"]}}},[]]}
  {"`Reference":[{"`Root":["foo","`TUnknown"]},[]]}
  {"`Reference":[{"`Page_path":["`TCurrentPackage",["foo"]]},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"foo"]}}},[]]}
  {"`Reference":[{"`Root":["foo","`TPage"]},[]]}
  ["Page","subdir/bar"]
  {"`Reference":[{"`Any_path":["`TCurrentPackage",["subdir","bar"]]},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"subdir"]}},"bar"]}}},[]]}
  {"`Reference":[{"`Root":["bar","`TUnknown"]},[]]}
  ["Page","dup"]
  {"`Reference":[{"`Any_path":["`TCurrentPackage",["dup"]]},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"dup"]}}},[]]}
  ["Page","subdir/dup"]
  {"`Reference":[{"`Any_path":["`TCurrentPackage",["subdir","dup"]]},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`LeafPage":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"doc"]}},"subdir"]}},"dup"]}}},[]]}
  ["Module","Test"]
  {"`Reference":[{"`Any_path":["`TCurrentPackage",["libname","Test"]]},[]]}
  {"`Reference":[{"`Any_path":["`TAbsolutePath",["pkg","libname","Test"]]},[]]}
  {"`Reference":[{"`Any_path":["`TRelativePath",["Test"]]},[]]}
  {"`Reference":[{"`Resolved":{"`Identifier":{"`Root":[{"Some":{"`Page":[{"Some":{"`Page":[{"Some":{"`Page":["None","pkg"]}},"lib"]}},"libname"]}},"Test"]}}},[]]}
