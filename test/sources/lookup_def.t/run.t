Compile the modules:

  $ ocamlc -c a.mli a.ml -bin-annot
  $ odoc compile --impl a.ml a.cmti
  $ odoc link a.odoc

Show the locations:

  $ odoc_print a.odocl | jq -c '.. | select(.locs?) | [ .id, .locs ]'
  [{"`Module":[{"`Root":["None","A"]},"M"]},{"Some":{"source_parent":{"`Root":["None","A"]},"anchor":{"Some":"def-A0"}}}]
  [{"`Module":[{"`Root":["None","A"]},"N"]},{"Some":{"source_parent":{"`Root":["None","A"]},"anchor":{"Some":"def-A5"}}}]
  [{"`ModuleType":[{"`Module":[{"`Root":["None","A"]},"N"]},"S"]},{"Some":{"source_parent":{"`Root":["None","A"]},"anchor":{"Some":"def-A2"}}}]
  [{"`Value":[{"`ModuleType":[{"`Module":[{"`Root":["None","A"]},"N"]},"S"]},"x"]},{"source_parent":{"`Root":["None","A"]},"anchor":{"Some":"def-A2"}}]
  [{"`Module":[{"`Module":[{"`Root":["None","A"]},"N"]},"T"]},{"Some":{"source_parent":{"`Root":["None","A"]},"anchor":{"Some":"def-A4"}}}]
  [{"`Value":[{"`Module":[{"`Module":[{"`Root":["None","A"]},"N"]},"T"]},"x"]},{"source_parent":{"`Root":["None","A"]},"anchor":{"Some":"def-A3"}}]
  [{"`Type":[{"`Root":["None","A"]},"t"]},{"source_parent":{"`Root":["None","A"]},"anchor":{"Some":"def-A6"}}]
  [{"`Value":[{"`Root":["None","A"]},"a"]},{"source_parent":{"`Root":["None","A"]},"anchor":{"Some":"def-A7"}}]
  [{"`Exception":[{"`Root":["None","A"]},"Exn"]},{"source_parent":{"`Root":["None","A"]},"anchor":{"Some":"def-A9"}}]
  [{"`Type":[{"`Root":["None","A"]},"ext"]},{"source_parent":{"`Root":["None","A"]},"anchor":{"Some":"def-A10"}}]
  [{"`Extension":[{"`Root":["None","A"]},"Ext"]},{"source_parent":{"`Root":["None","A"]},"anchor":{"Some":"def-A11"}}]
  [{"`Class":[{"`Root":["None","A"]},"cls"]},{"source_parent":{"`Root":["None","A"]},"anchor":{"Some":"def-A12"}}]
  [{"`ClassType":[{"`Root":["None","A"]},"clst"]},{"source_parent":{"`Root":["None","A"]},"anchor":{"Some":"def-A14"}}]
