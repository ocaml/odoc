Source links for kind abbreviations.

  $ ocamlc -c k.ml -bin-annot -I .

  $ odoc compile-impl --source-id src/k.ml -I . k.cmt
  $ odoc compile -I . k.cmt
  $ odoc link -I . impl-k.odoc
  $ odoc link -I . k.odoc
  $ odoc html-generate-source --impl impl-k.odocl --indent -o html k.ml
  $ odoc html-generate --indent -o html k.odocl

The kind abbreviation links to its own definition, not to the whole file:

  $ grep -o 'href="[^"]*src/k.ml.html[^"]*"' html/K/index.html
  href="../src/k.ml.html"
  href="../src/k.ml.html#kind-my_kind"
  href="../src/k.ml.html#type-t"

  $ grep -c 'id="kind-my_kind"' html/src/k.ml.html
  1
