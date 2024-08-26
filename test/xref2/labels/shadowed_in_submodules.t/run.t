Label in a submodule clashing with a previously defined label 

There should be no ambiguous labels in this example.

  $ compile test.mli

All the references should resolve and point to what's written in the text.

  $ odoc_print test.odocl | jq -c '.. | .["`Reference"]? | select(.)'
  [{"`Resolved":{"`Identifier":{"`Label":[{"`Module":[{"`Root":[{"Some":{"`Library":["None","test","test"]}},"Test"]},"X"]},"foo"]}}},[{"`Word":"Expecting"},"`Space",{"`Word":"H2"}]]
  [{"`Resolved":{"`Identifier":{"`Label":[{"`Root":[{"Some":{"`Library":["None","test","test"]}},"Test"]},"foo"]}}},[{"`Word":"Expecting"},"`Space",{"`Word":"H1"}]]
  [{"`Resolved":{"`Label":[{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Library":["None","test","test"]}},"Test"]},"X"]}},"foo"]}},[{"`Word":"Expecting"},"`Space",{"`Word":"H2"}]]
