Regression test for ocaml/odoc#1456 (spurious source links on derived code).

Occurrences under a [@merlin.hide] attribute must not be rendered as source
links. This makes odoc ignore generated code in a way that is consistent
with Merlin and ppxlib.

[visible.ml] and [hidden.ml] hold the same definitions (a cross-module
reference ([Mylib.truc]) and local ones) but [hidden.ml] wraps them in
[include struct ... end [@@merlin.hide]]. odoc must link the references of the
former and none of the latter.

  $ ocamlc -c mylib.ml visible.ml hidden.ml -bin-annot

Render [Mylib] too, so the cross-module occurrence has a source anchor to point
at:

  $ odoc compile-impl --source-id src/mylib.ml -I . mylib.cmt
  $ odoc compile -I . mylib.cmt
  $ odoc link -I . mylib.odoc
  $ odoc link -I . impl-mylib.odoc
  $ odoc html-generate-source --impl impl-mylib.odocl --indent -o html mylib.ml

  $ for m in visible hidden; do
  >   odoc compile-impl --source-id src/$m.ml -I . $m.cmt
  >   odoc compile -I . $m.cmt
  >   odoc link -I . $m.odoc
  >   odoc link -I . impl-$m.odoc
  >   odoc html-generate-source --impl impl-$m.odocl --indent -o html $m.ml
  > done

Without [@merlin.hide], both the cross-module and the local references are
linked:

  $ grep -c 'href="mylib.ml.html#val-truc"' html/src/visible.ml.html
  1
  $ grep -c 'href="#local' html/src/visible.ml.html
  2

Under [@merlin.hide], neither is -- yet the source itself is still rendered
(grep exits 1 when there is no match):

  $ grep -c 'href="mylib.ml.html#val-truc"' html/src/hidden.ml.html
  0
  [1]
  $ grep -c 'href="#local' html/src/hidden.ml.html
  0
  [1]
  $ grep -c 'class="LET"' html/src/hidden.ml.html
  2
