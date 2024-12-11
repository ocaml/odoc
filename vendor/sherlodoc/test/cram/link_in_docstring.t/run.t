  $ ocamlc -c a.mli -bin-annot -I .
  $ odoc compile -I . a.cmti
  $ odoc link -I . a.odoc
  $ export SHERLODOC_DB=db.bin
  $ export SHERLODOC_FORMAT=marshal
  $ sherlodoc index $(find . -name '*.odocl')
  $ sherlodoc search --print-docstring "foo"
  val A.foo : int
  <div><p>This is a docstring with a <span>link</span></p></div>
  $ sherlodoc search --print-docstring "bar"
  val A.bar : int
  <div><p>This is a docstring with a ref to <span><code>foo</code></span></p></div>
