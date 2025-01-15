Compile the modules:

  $ ocamlc -c a.mli a.ml -bin-annot

  $ odoc compile-impl --source-id src/a.ml -I . a.cmt
  $ odoc compile -I . a.cmti

  $ odoc link -I . src-a.odoc
  odoc: FILE.odoc argument: no 'src-a.odoc' file or directory
  Usage: odoc link [--custom-layout] [--open=MODULE] [OPTION]… FILE.odoc
  Try 'odoc link --help' or 'odoc --help' for more information.
  [2]
  $ odoc link -I . a.odoc

Show the locations:

  $ odoc_print a.odocl | jq -c '.. | select(.source_loc?) | [ .id, .source_loc ]'
  [{"`Module":[{"`Root":["None","A"]},"M"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":["None","src"]},"a.ml"]},"module-M"]}}]
  [{"`Module":[{"`Root":["None","A"]},"N"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":["None","src"]},"a.ml"]},"module-N"]}}]
  [{"`ModuleType":[{"`Module":[{"`Root":["None","A"]},"N"]},"S"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":["None","src"]},"a.ml"]},"module-N.module-type-S"]}}]
  [{"`Value":[{"`ModuleType":[{"`Module":[{"`Root":["None","A"]},"N"]},"S"]},"x"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":["None","src"]},"a.ml"]},"module-N.module-type-S"]}}]
  [{"`Module":[{"`Module":[{"`Root":["None","A"]},"N"]},"T"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":["None","src"]},"a.ml"]},"module-N.module-T"]}}]
  [{"`Value":[{"`Module":[{"`Module":[{"`Root":["None","A"]},"N"]},"T"]},"x"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":["None","src"]},"a.ml"]},"module-N.module-T.val-x"]}}]
  [{"`Type":[{"`Root":["None","A"]},"t"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":["None","src"]},"a.ml"]},"type-t"]}}]
  [{"`Value":[{"`Root":["None","A"]},"a"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":["None","src"]},"a.ml"]},"val-a"]}}]
  [{"`Exception":[{"`Root":["None","A"]},"Exn"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":["None","src"]},"a.ml"]},"exception-Exn"]}}]
  [{"`Type":[{"`Root":["None","A"]},"ext"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":["None","src"]},"a.ml"]},"type-ext"]}}]
  [{"`Extension":[{"`Root":["None","A"]},"Ext"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":["None","src"]},"a.ml"]},"extension-Ext"]}}]
  [{"`Class":[{"`Root":["None","A"]},"cls"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":["None","src"]},"a.ml"]},"class-cls"]}}]
  [{"`ClassType":[{"`Root":["None","A"]},"clst"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":["None","src"]},"a.ml"]},"class-type-clst"]}}]
