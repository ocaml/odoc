Test that @canonical tags work on compilation units when it is placed in the
top-comment.

The module Test__X is expected to be referenced through Test.X.

  $ compile test__x.mli test.ml

Test__x has a 'canonical' field:

  $ odoc_print test__x.odocl | jq -c ".canonical"
  {"Some":{"`Dot":[{"`Root":"Test"},"X"]}}

The alias Test.X should be marked as canonical:

  $ odoc_print test.odocl | jq -c ".content.Module.items | .[] | .Module[1].type_.Alias[0] | select(.)"
  {"`Resolved":{"`Canonical":[{"`Hidden":{"`Identifier":{"`Root":[{"`RootPage":"test"},"Test__x"]}}},{"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"X"]}}}]}}
  {"`Resolved":{"`Canonical":[{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"Test__y"]}},{"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"Y"]}}}]}}
  {"`Resolved":{"`Hidden":{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"Test__z"]}}}}
