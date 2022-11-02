Compile the modules:

  $ ocamlc -c a.mli a.ml -bin-annot
  $ odoc compile a.cmti

Show the locations:

  $ odoc_print a.odoc | jq '.. | .locs? | select(.)'
  {
    "impl": {
      "Some": "File \"a.ml\", line 1, characters 0-21"
    },
    "intf": "None"
  }
  {
    "impl": {
      "Some": "File \"a.ml\", line 3, character 0 to line 7, character 3"
    },
    "intf": "None"
  }
  {
    "impl": {
      "Some": "File \"a.ml\", line 4, character 2 to line 6, character 5"
    },
    "intf": "None"
  }
