Source links (doc/scope_model.mld, section 7, the row "Find the
implementation of a unit"). The definition behind a name in an
implementation can be in another library.
Library [b] calls [A.f] from library [lib]. A library [other] also has a module
[A], which [b] was not compiled against. The source link for [A.f] should go
to [lib]'s [A], not to [other]'s.

  $ cat lib/a.ml
  let f () = ()
  $ cat other/a.ml
  let f () = ()
  let unrelated = ()
  $ cat b/b.ml
  let g = A.f
  $ (cd lib && ocamlc -c -bin-annot a.ml)
  $ (cd other && ocamlc -c -bin-annot a.ml)
  $ (cd b && ocamlc -c -bin-annot -I ../lib b.ml)
  $ odoc compile-impl lib/a.cmt --output-dir h --parent-id lib --source-id lib/a.ml -L lib:h/lib
  $ odoc compile-impl other/a.cmt --output-dir h --parent-id other --source-id other/a.ml -L other:h/other
  $ odoc compile-impl b/b.cmt --output-dir h --parent-id b --source-id b/b.ml -L b:h/b -L lib:h/lib

Linked with [-L] alone, as a driver gives the libraries of the run. The
source page should have a link to [lib]'s [a.ml].

  $ odoc link h/b/impl-b.odoc -L other:h/other -L b:h/b -L lib:h/lib
  $ odoc html-generate-source --impl h/b/impl-b.odocl -o html b/b.ml
  $ grep -o 'href="[^"#]*/a.ml.html[^"]*"' html/b/b.ml.html
  href="../lib/a.ml.html#val-f"
