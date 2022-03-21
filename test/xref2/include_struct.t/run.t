This was simply failing in a previous incarnation of the code 
  $ for i in a.mli b.ml c.ml d.ml e.ml f.ml; do ocamlc -c -bin-annot $i; done
  $ odoc compile -I . a.cmti
  $ odoc compile -I . b.cmt
  $ odoc compile -I . c.cmt
  $ odoc compile -I . d.cmt
  $ odoc compile -I . e.cmt
  $ odoc compile -I . f.cmt
