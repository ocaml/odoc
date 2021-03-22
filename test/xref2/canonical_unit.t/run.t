Test that @canonical tags work on compilation units when it is placed in the
top-comment.

The module Test__X is expected to be referenced through Test.X.

  $ compile test__x.mli test.ml

The alias Test.X should be marked as canonical:

  $ odoc_print test.odocl | jq -c ".content.Module.items | .[] | .Module[1].type_.Alias[0] | select(.)"
  {"`Resolved":{"`Hidden":{"`Identifier":{"`Root":[{"`RootPage":"test"},"Test__x"]}}}}
  {"`Resolved":{"`Canonical":[{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"Test__y"]}},{"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"Y"]}}}]}}
  {"`Resolved":{"`Hidden":{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Test"]},"Test__z"]}}}}
