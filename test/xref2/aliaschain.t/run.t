  $ ocamlc -c -bin-annot chain.mli
  $ odoc compile chain.cmti
  $ odoc link chain.odoc -I .
  $ du -h chain.odocl
   36M	chain.odocl
