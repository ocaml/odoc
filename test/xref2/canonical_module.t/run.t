Test that @canonical tags work on compilation units when it is placed in the
top-comment.

The module Test_x is expected to be referenced as Test.X.

  $ compile test_x.mli test_y.ml test.ml

Test_x and Test_y have a 'canonical' field:

  $ odoc_print test_x.odocl | jq -c ".canonical"
  {"Some":{"`Dot":[{"`Root":"Test"},"X"]}}
  $ odoc_print test_y.odocl | jq -c ".canonical"
  {"Some":{"`Dot":[{"`Root":"Test"},"Y"]}}

Test_x is defined as a .mli file and Test_y as a .ml file in order to test the
loader code handling canonical tags.

Submodules *_out have the canonical tag in the declarations and submodules *_in
have it in the top-comment.

Every references should be marked as canonical:

  $ odoc_print test.odocl | jq -c ".content.Module.items | .[] | .Module[1].type_.Alias[0] | select(.)"
  {"`Resolved":{"`Canonical":[{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_x"]}},{"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"X"]}}}]}}
  {"`Resolved":{"`Canonical":[{"`Module":[{"`Canonical":[{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_x"]}},{"`Dot":[{"`Root":"Test"},"X"]}]},"Out"]},{"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"X_out"]}}}]}}
  {"`Resolved":{"`Canonical":[{"`Module":[{"`Canonical":[{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_x"]}},{"`Dot":[{"`Root":"Test"},"X"]}]},"In"]},{"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"X_in"]}}}]}}
  {"`Resolved":{"`Canonical":[{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_y"]}},{"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"Y"]}}}]}}
  {"`Resolved":{"`Canonical":[{"`Module":[{"`Canonical":[{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_y"]}},{"`Dot":[{"`Root":"Test"},"Y"]}]},"Out"]},{"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"Y_out"]}}}]}}
  {"`Resolved":{"`Canonical":[{"`Module":[{"`Canonical":[{"`Identifier":{"`Root":[{"Some":{"`Page":["None","test"]}},"Test_y"]}},{"`Dot":[{"`Root":"Test"},"Y"]}]},"In"]},{"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"Y_in"]}}}]}}
