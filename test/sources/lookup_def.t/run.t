Compile the modules:

  $ odoc compile --child module-a root.mld
  $ ocamlc -c a.mli a.ml -bin-annot
  $ odoc compile --source a.ml --source-parent page-root -I . a.cmti
  $ odoc link a.odoc

Show the locations:

  $ odoc_print a.odocl | jq -c '.. | select(.locs?) | [ .id, .locs ]'
  [{"`Module":[{"`Root":["None","A"]},"M"]},{"Some":{"source_parent":[{"`Page":["None","root"]},"a.ml"],"anchor":{"Some":"def-0"}}}]
  [{"`Module":[{"`Root":["None","A"]},"N"]},{"Some":{"source_parent":[{"`Page":["None","root"]},"a.ml"],"anchor":{"Some":"def-5"}}}]
  [{"`ModuleType":[{"`Module":[{"`Root":["None","A"]},"N"]},"S"]},{"Some":{"source_parent":[{"`Page":["None","root"]},"a.ml"],"anchor":{"Some":"def-2"}}}]
  [{"`Value":[{"`ModuleType":[{"`Module":[{"`Root":["None","A"]},"N"]},"S"]},"x"]},{"Some":{"source_parent":[{"`Page":["None","root"]},"a.ml"],"anchor":{"Some":"def-2"}}}]
  [{"`Module":[{"`Module":[{"`Root":["None","A"]},"N"]},"T"]},{"Some":{"source_parent":[{"`Page":["None","root"]},"a.ml"],"anchor":{"Some":"def-4"}}}]
  [{"`Value":[{"`Module":[{"`Module":[{"`Root":["None","A"]},"N"]},"T"]},"x"]},{"Some":{"source_parent":[{"`Page":["None","root"]},"a.ml"],"anchor":{"Some":"def-3"}}}]
  [{"`Type":[{"`Root":["None","A"]},"t"]},{"Some":{"source_parent":[{"`Page":["None","root"]},"a.ml"],"anchor":{"Some":"def-6"}}}]
  [{"`Value":[{"`Root":["None","A"]},"a"]},{"Some":{"source_parent":[{"`Page":["None","root"]},"a.ml"],"anchor":{"Some":"def-7"}}}]
  [{"`Exception":[{"`Root":["None","A"]},"Exn"]},{"Some":{"source_parent":[{"`Page":["None","root"]},"a.ml"],"anchor":{"Some":"def-9"}}}]
  [{"`Type":[{"`Root":["None","A"]},"ext"]},{"Some":{"source_parent":[{"`Page":["None","root"]},"a.ml"],"anchor":{"Some":"def-10"}}}]
  [{"`Extension":[{"`Root":["None","A"]},"Ext"]},{"Some":{"source_parent":[{"`Page":["None","root"]},"a.ml"],"anchor":{"Some":"def-11"}}}]
  [{"`Class":[{"`Root":["None","A"]},"cls"]},{"Some":{"source_parent":[{"`Page":["None","root"]},"a.ml"],"anchor":{"Some":"def-12"}}}]
  [{"`ClassType":[{"`Root":["None","A"]},"clst"]},{"Some":{"source_parent":[{"`Page":["None","root"]},"a.ml"],"anchor":{"Some":"def-14"}}}]
