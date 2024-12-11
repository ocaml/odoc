This test checkes that project that is empty despite not looking empty does not
crash sherlodoc.
  $ export PATH=.:$PATH
  $ export OCAMLRUNPARAM=b
  $ dune build @doc
  $ sherlodoc index ./_build/default/_doc/_odocls/foo/page-index.odocl --format=marshal --db=db.marshal
  $ sherlodoc search --db=db.marshal lorem
  [No results]

