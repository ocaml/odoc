Test that @canonical tags work on compilation units when it is placed in the
top-comment.

The module Test_x is expected to be referenced as Test.X.
Test_w is similar but defined as a .ml file.

  $ compile test_x.mli test_w.ml test.ml
  File "test_x.mli", line 13, characters 6-26:
  Unexpected tag '@canonical' at this location.
  File "test_w.ml", line 13, characters 6-26:
  Unexpected tag '@canonical' at this location.
  File "test.ml", line 18, characters 6-24:
  Unexpected tag '@canonical' at this location.

Test_x has a 'canonical' field:

  $ odoc_print test_x.odocl | jq -c ".canonical"
  {"Some":{"`Dot":[{"`Root":"Test"},"X"]}}

The first two type declarations should have resolved canonical constructors, the third should not

  $ odoc_print test.odocl | jq -c ".content.Module.items | .[] | .Type[1] | select(.) | .equation.manifest.Some.Constr"
  [{"`Resolved":{"`Type":[{"`Canonical":[{"`Identifier":{"`Root":[{"`RootPage":"test"},"Test_x"]}},{"`Resolved":{"`Alias":[{"`Identifier":{"`Root":[{"`RootPage":"test"},"Test_x"]}},{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"X"]}}]}}]},"t"]}},[]]
  [{"`Resolved":{"`Type":[{"`Canonical":[{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"Test_y"]}},{"`Resolved":{"`Alias":[{"`Canonical":[{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"Test_y"]}},{"`Dot":[{"`Root":"Test"},"Y"]}]},{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"Y"]}}]}}]},"t"]}},[]]
  [{"`Resolved":{"`Type":[{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"Test_z"]}},"t"]}},[]]

  $ odoc_print test.odocl | jq -c ".content.Module.items | .[] | .Module[1].type_.Alias[0] | select(.)"
  {"`Resolved":{"`Canonical":[{"`Identifier":{"`Root":[{"`RootPage":"test"},"Test_x"]}},{"`Resolved":{"`Alias":[{"`Identifier":{"`Root":[{"`RootPage":"test"},"Test_x"]}},{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"X"]}}]}}]}}
  {"`Resolved":{"`Canonical":[{"`Module":[{"`Canonical":[{"`Identifier":{"`Root":[{"`RootPage":"test"},"Test_x"]}},{"`Resolved":{"`Alias":[{"`Identifier":{"`Root":[{"`RootPage":"test"},"Test_x"]}},{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"X"]}}]}}]},"M"]},{"`Resolved":{"`Alias":[{"`Module":[{"`Identifier":{"`Root":[{"`RootPage":"test"},"Test_x"]}},"M"]},{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"X_m"]}}]}}]}}
  {"`Resolved":{"`Module":[{"`Canonical":[{"`Identifier":{"`Root":[{"`RootPage":"test"},"Test_x"]}},{"`Resolved":{"`Alias":[{"`Identifier":{"`Root":[{"`RootPage":"test"},"Test_x"]}},{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"X"]}}]}}]},"N"]}}
  {"`Resolved":{"`Canonical":[{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"Test_y"]}},{"`Resolved":{"`Alias":[{"`Canonical":[{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"Test_y"]}},{"`Dot":[{"`Root":"Test"},"Y"]}]},{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"Y"]}}]}}]}}
  {"`Resolved":{"`Canonical":[{"`Identifier":{"`Root":[{"`RootPage":"test"},"Test_w"]}},{"`Resolved":{"`Alias":[{"`Identifier":{"`Root":[{"`RootPage":"test"},"Test_w"]}},{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"W"]}}]}}]}}
  {"`Resolved":{"`Canonical":[{"`Module":[{"`Canonical":[{"`Identifier":{"`Root":[{"`RootPage":"test"},"Test_w"]}},{"`Resolved":{"`Alias":[{"`Identifier":{"`Root":[{"`RootPage":"test"},"Test_w"]}},{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"W"]}}]}}]},"M"]},{"`Resolved":{"`Alias":[{"`Module":[{"`Identifier":{"`Root":[{"`RootPage":"test"},"Test_w"]}},"M"]},{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"W_m"]}}]}}]}}
  {"`Resolved":{"`Module":[{"`Canonical":[{"`Identifier":{"`Root":[{"`RootPage":"test"},"Test_w"]}},{"`Resolved":{"`Alias":[{"`Identifier":{"`Root":[{"`RootPage":"test"},"Test_w"]}},{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"W"]}}]}}]},"N"]}}
  {"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"Test_z"]}}}
