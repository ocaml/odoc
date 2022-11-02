A long chain of aliases should produce an odocl file that's a reasonable
size.

  $ ocamlc -c -bin-annot chain.mli
  $ odoc compile chain.cmti
  File "chain.cmti":
  Warning: No implementation file found for the given interface
  $ odoc link chain.odoc -I .
  $ find . -name chain.odocl -size +100000c 
  ./chain.odocl
