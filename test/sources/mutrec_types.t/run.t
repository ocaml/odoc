Mutually recursive type declarations should get a source location, like
non-recursive ones. On OxCaml 5.4, shape reduction does not return a uid for
them and odoc has to look inside the [Mutrec] shape.

  $ ocamlc -c a.mli a.ml -bin-annot

  $ odoc compile-impl --source-id src/a.ml -I . a.cmt
  $ odoc compile -I . a.cmti

  $ ! odoc link -I . src-a.odoc > /dev/null 2>&1
  $ odoc link -I . a.odoc

  $ odoc_print a.odocl | jq -c '.. | select(.source_loc?) | [ .id, .source_loc ]'
  [{"`Type":[{"`Root":["None","A"]},"t"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":["None","src"]},"a.ml"]},"type-t"]}}]
  [{"`Type":[{"`Root":["None","A"]},"a"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":["None","src"]},"a.ml"]},"type-a"]}}]
  [{"`Type":[{"`Root":["None","A"]},"b"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":["None","src"]},"a.ml"]},"type-b"]}}]
  [{"`Type":[{"`Root":["None","A"]},"c"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":["None","src"]},"a.ml"]},"type-c"]}}]
  [{"`Type":[{"`Root":["None","A"]},"d"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":["None","src"]},"a.ml"]},"type-d"]}}]
