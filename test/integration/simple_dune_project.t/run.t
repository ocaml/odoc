Build the documentation of a simple Dune library.

  $ dune build @install @doc
          odoc _doc/_html/dune_odoc_test/Dune_odoc_test/.dune-keep,_doc/_html/dune_odoc_test/Dune_odoc_test/index.html
  Starting link
          odoc _doc/_html/dune_odoc_test/Dune_odoc_test__Foo/.dune-keep,_doc/_html/dune_odoc_test/Dune_odoc_test__Foo/index.html
  Starting link
          odoc _doc/_html/dune_odoc_test/Dune_odoc_test__Bar/.dune-keep,_doc/_html/dune_odoc_test/Dune_odoc_test__Bar/index.html
  Starting link

  $ find _build/default/_doc/_html -name '*.html' | sort
  _build/default/_doc/_html/dune_odoc_test/Dune_odoc_test/Bar/index.html
  _build/default/_doc/_html/dune_odoc_test/Dune_odoc_test/Foo/index.html
  _build/default/_doc/_html/dune_odoc_test/Dune_odoc_test/index.html
  _build/default/_doc/_html/dune_odoc_test/Dune_odoc_test__Bar/index.html
  _build/default/_doc/_html/dune_odoc_test/Dune_odoc_test__Foo/index.html
  _build/default/_doc/_html/dune_odoc_test/index.html
  _build/default/_doc/_html/index.html
