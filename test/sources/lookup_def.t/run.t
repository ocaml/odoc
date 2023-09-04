Compile the modules:

  $ odoc compile -c module-a -c src-source root.mld

  $ printf "a.ml\n" > source_tree.map
  $ odoc source-tree -I . --parent page-root -o src-source.odoc source_tree.map

  $ ocamlc -c a.mli a.ml -bin-annot
  $ odoc compile --cmt a.cmt --source-name a.ml --source-parent-file src-source.odoc -I . a.cmti
  $ odoc link a.odoc

Show the locations:

  $ odoc_print a.odocl | jq -c '.. | select(.locs?) | [ .id, .locs ]'
  [{"`Module":[{"`Root":["None","A"]},"M"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"module-M"]}}]
  [{"`Module":[{"`Root":["None","A"]},"N"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"module-N"]}}]
  [{"`ModuleType":[{"`Module":[{"`Root":["None","A"]},"N"]},"S"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"module-N.module-type-S"]}}]
  [{"`Value":[{"`ModuleType":[{"`Module":[{"`Root":["None","A"]},"N"]},"S"]},"x"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"module-N.module-type-S"]}}]
  [{"`Module":[{"`Module":[{"`Root":["None","A"]},"N"]},"T"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"module-N.module-T"]}}]
  [{"`Value":[{"`Module":[{"`Module":[{"`Root":["None","A"]},"N"]},"T"]},"x"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"module-N.module-T.val-x"]}}]
  [{"`Type":[{"`Root":["None","A"]},"t"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"type-t"]}}]
  [{"`Value":[{"`Root":["None","A"]},"a"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"val-a"]}}]
  [{"`Exception":[{"`Root":["None","A"]},"Exn"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"exception-Exn"]}}]
  [{"`Type":[{"`Root":["None","A"]},"ext"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"type-ext"]}}]
  [{"`Extension":[{"`Root":["None","A"]},"Ext"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"extension-Ext"]}}]
  [{"`Class":[{"`Root":["None","A"]},"cls"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"class-cls"]}}]
  [{"`ClassType":[{"`Root":["None","A"]},"clst"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"class-type-clst"]}}]
