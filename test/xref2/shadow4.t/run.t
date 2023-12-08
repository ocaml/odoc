  $ ocamlc -c -bin-annot test.mli
  $ odoc compile test.cmti
  $ odoc link test.odoc
  File "test.odoc":
  Warning: Failed to lookup type identifier(root(Test).T.{t}1,false) Lookup failure (type): root(Test).T.{t}1

