Test that @canonical tags work on compilation units when it is placed in the
top-comment.

The module Test_X is expected to be referenced through Test.X.

  $ compile test_x.mli test.ml
  File "test.ml", line 15, characters 6-24:
  Unexpected tag '@canonical' at this location.

Test_x has a 'canonical' field:

  $ odoc_print test_x.odocl | jq -c ".canonical"
  {"Some":{"`Dot":[{"`Root":"Test"},"X"]}}

The first two type declarations should have resolved canonical constructors, the third should not

  $ odoc_print test.odocl | jq -c ".content.Module.items | .[] | .Type[1] | select(.) | .equation.manifest.Some.Constr"
  [{"`Resolved":{"`Type":[{"`Canonical":[{"`Identifier":{"`Root":[{"`RootPage":"test"},"Test_x"]}},{"`Resolved":{"`Alias":[{"`Identifier":{"`Root":[{"`RootPage":"test"},"Test_x"]}},{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"X"]}}]}}]},"t"]}},[]]
  [{"`Resolved":{"`Type":[{"`Canonical":[{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"Test_y"]}},{"`Resolved":{"`Alias":[{"`Canonical":[{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"Test_y"]}},{"`Dot":[{"`Root":"Test"},"Y"]}]},{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"Y"]}}]}}]},"t"]}},[]]
  [{"`Resolved":{"`Type":[{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"Test_z"]}},"t"]}},[]]

