Compile the modules:

  $ odoc compile -c module-a -c src-source root.mld

  $ printf "a.ml\n" > source_tree.map
  $ odoc source-tree -I . --parent page-root -o src-source.odoc source_tree.map

  $ ocamlc -c a.mli a.ml -bin-annot
  $ odoc compile --source-name a.ml --source-parent-file src-source.odoc -I . a.cmti
  $ odoc link a.odoc

Show the locations:

  $ odoc_print a.odocl | jq -c '.. | select(.locs?) | [ .id, .locs ]'
  [{"`Module":[{"`Root":["None","A"]},"M"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"def-0"]}}]
  [{"`Module":[{"`Root":["None","A"]},"N"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"def-5"]}}]
  [{"`ModuleType":[{"`Module":[{"`Root":["None","A"]},"N"]},"S"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"def-2"]}}]
  [{"`Value":[{"`ModuleType":[{"`Module":[{"`Root":["None","A"]},"N"]},"S"]},"x"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"def-2"]}}]
  [{"`Module":[{"`Module":[{"`Root":["None","A"]},"N"]},"T"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"def-4"]}}]
  [{"`Value":[{"`Module":[{"`Module":[{"`Root":["None","A"]},"N"]},"T"]},"x"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"def-3"]}}]
  [{"`Type":[{"`Root":["None","A"]},"t"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"def-6"]}}]
  [{"`Value":[{"`Root":["None","A"]},"a"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"def-7"]}}]
  [{"`Exception":[{"`Root":["None","A"]},"Exn"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"def-9"]}}]
  [{"`Type":[{"`Root":["None","A"]},"ext"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"def-10"]}}]
  [{"`Extension":[{"`Root":["None","A"]},"Ext"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"def-11"]}}]
  [{"`Class":[{"`Root":["None","A"]},"cls"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"def-12"]}}]
  [{"`ClassType":[{"`Root":["None","A"]},"clst"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":[{"Some":{"`Page":["None","root"]}},"source"]},"a.ml"]},"def-14"]}}]
