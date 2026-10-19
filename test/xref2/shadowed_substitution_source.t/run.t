A local substitution shadowing a real item mustn't make their anchors in
source pages collide: each substitution has an anchor of its own.

  $ ocamlc -c -bin-annot source.ml
  $ odoc compile-impl --source-id src/source.ml -I . source.cmt --output-dir .
  $ odoc compile -I . source.cmt
  $ odoc link -I . impl-source.odoc
  $ odoc html-generate-source --impl impl-source.odocl -o html source.ml
  $ grep -o 'id="module-type-S[^"]*"' html/src/source.ml.html
  id="module-type-S"
  id="module-type-S.type-{t}1/shadowed/(src-source.ml)"
  id="module-type-S.type-t"
  id="module-type-S.module-type-{T}2/shadowed/(src-source.ml)"
  id="module-type-S.module-type-T"
  id="module-type-S.module-type-T.type-q"
  id="module-type-S.module-{M}3/shadowed/(src-source.ml)"
  id="module-type-S.module-M"
