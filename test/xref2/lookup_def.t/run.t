Compile the modules:

  $ ocamlc -c a.mli a.ml -bin-annot
  $ odoc compile a.cmti

Show the locations:

  $ odoc_print a.odoc | jq -c '.. | select(.locs?) | [ .id, .locs ]'
  [{"`Module":[{"`Root":["None","A"]},"M"]},{"impl":{"Some":"File \"a.ml\", line 1, characters 0-21"},"intf":{"Some":"File \"a.mli\", line 1, characters 0-18"}}]
  [{"`Module":[{"`Root":["None","A"]},"N"]},{"impl":{"Some":"File \"a.ml\", line 3, character 0 to line 11, character 3"},"intf":{"Some":"File \"a.mli\", line 3, character 0 to line 9, character 3"}}]
  [{"`ModuleType":[{"`Module":[{"`Root":["None","A"]},"N"]},"S"]},{"impl":"None","intf":{"Some":"File \"a.mli\", line 4, character 2 to line 6, character 5"}}]
  [{"`Value":[{"`ModuleType":[{"`Module":[{"`Root":["None","A"]},"N"]},"S"]},"x"]},{"impl":{"Some":"File \"a.ml\", line 4, character 2 to line 6, character 5"},"intf":{"Some":"File \"a.mli\", line 5, characters 4-15"}}]
  [{"`Module":[{"`Module":[{"`Root":["None","A"]},"N"]},"T"]},{"impl":{"Some":"File \"a.ml\", line 8, character 2 to line 10, character 5"},"intf":{"Some":"File \"a.mli\", line 8, characters 2-14"}}]
  [{"`Value":[{"`Module":[{"`Module":[{"`Root":["None","A"]},"N"]},"T"]},"x"]},{"impl":{"Some":"File \"a.ml\", line 4, character 2 to line 6, character 5"},"intf":{"Some":"File \"a.mli\", line 5, characters 4-15"}}]
  [{"`Type":[{"`Root":["None","A"]},"t"]},{"impl":"None","intf":{"Some":"File \"a.mli\", line 11, characters 0-6"}}]
  [{"`Value":[{"`Root":["None","A"]},"a"]},{"impl":{"Some":"File \"a.ml\", line 15, characters 4-5"},"intf":{"Some":"File \"a.mli\", line 13, characters 0-11"}}]
  [{"`Exception":[{"`Root":["None","A"]},"Exn"]},{"impl":"None","intf":{"Some":"File \"a.mli\", line 15, characters 0-13"}}]
  [{"`Type":[{"`Root":["None","A"]},"ext"]},{"impl":"None","intf":{"Some":"File \"a.mli\", line 17, characters 0-13"}}]
  [{"`Extension":[{"`Root":["None","A"]},"Ext"]},{"impl":"None","intf":{"Some":"File \"a.mli\", line 19, characters 12-15"}}]
  [{"`Class":[{"`Root":["None","A"]},"cls"]},{"impl":"None","intf":{"Some":"File \"a.mli\", line 21, characters 0-22"}}]
  [{"`ClassType":[{"`Root":["None","A"]},"clst"]},{"impl":"None","intf":{"Some":"File \"a.mli\", line 23, characters 0-28"}}]
