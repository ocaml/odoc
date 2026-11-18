Mutually recursive type declarations should get a source location, like
non-recursive ones. On OxCaml 5.4, shape reduction does not return a uid for
them and odoc has to look inside the [Mutrec] shape.

  $ ocamlc -c a.mli a.ml -bin-annot

  $ odoc compile-impl --source-id src/a.ml -I . a.cmt
  $ odoc compile -I . a.cmti

  $ odoc link -I . impl-a.odoc
  $ odoc link -I . a.odoc

  $ odoc_print a.odocl | jq -c '.. | select(.source_loc?) | [ .id, .source_loc ]'
  [{"`Type":[{"`Root":["None","A"]},"t"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":["None","src"]},"a.ml"]},"type-t"]}}]
  [{"`Type":[{"`Root":["None","A"]},"a"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":["None","src"]},"a.ml"]},"type-a"]}}]
  [{"`Type":[{"`Root":["None","A"]},"b"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":["None","src"]},"a.ml"]},"type-b"]}}]
  [{"`Type":[{"`Root":["None","A"]},"c"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":["None","src"]},"a.ml"]},"type-c"]}}]
  [{"`Type":[{"`Root":["None","A"]},"d"]},{"Some":{"`SourceLocation":[{"`SourcePage":[{"`Page":["None","src"]},"a.ml"]},"type-d"]}}]

The definitions in the implementation carry the location of each type:

  $ odoc_print impl-a.odocl | jq -c '[.source_info[] | select(.[0].Definition?) | [.[0].Definition."`SourceLocation"[1], .[1]] | select(.[0] | test("^type-[a-z]+$"))] | sort_by(.[1].loc_start.pos_cnum | tonumber) | .[]'
  ["type-t",{"loc_start":{"pos_lnum":"1","pos_cnum":"0"},"loc_end":{"pos_lnum":"1","pos_cnum":"12"}}]
  ["type-a",{"loc_start":{"pos_lnum":"3","pos_cnum":"14"},"loc_end":{"pos_lnum":"3","pos_cnum":"35"}}]
  ["type-b",{"loc_start":{"pos_lnum":"4","pos_cnum":"36"},"loc_end":{"pos_lnum":"4","pos_cnum":"50"}}]
  ["type-c",{"loc_start":{"pos_lnum":"6","pos_cnum":"52"},"loc_end":{"pos_lnum":"6","pos_cnum":"70"}}]
  ["type-d",{"loc_start":{"pos_lnum":"7","pos_cnum":"71"},"loc_end":{"pos_lnum":"7","pos_cnum":"95"}}]
