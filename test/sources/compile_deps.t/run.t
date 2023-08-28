Source code rendering needs the same compilation order as cmts.

As a consequence, the dependencies should be taken from the cmt, when source
code rendering is enabled. This must be specified using the --has-src flag for
compile-deps

  $ ocamlc -c b.ml -bin-annot
  $ ocamlc -c a.mli -I . -bin-annot
  $ ocamlc -c a.ml -I . -bin-annot

[a.cmti] does not depend on B, while its implementation [a.cmt] depends on B.

  $ odoc compile-deps a.cmti
  CamlinternalFormatBasics 8f8f634558798ee408df3c50a5539b15
  Stdlib 79b0e9d3b6f7fed07eb3cc2abb961b91
  A 21e6137bd9b3aaa3c66960387b5f32c0
  $ odoc compile-deps a.cmt
  Stdlib 79b0e9d3b6f7fed07eb3cc2abb961b91
  CamlinternalFormatBasics 8f8f634558798ee408df3c50a5539b15
  B 903ddd9b7c0fa4ee6d34b4af6358d1e1
  A 21e6137bd9b3aaa3c66960387b5f32c0

Must contain B:
  $ odoc compile-deps --has-src a.cmti
  Stdlib 79b0e9d3b6f7fed07eb3cc2abb961b91
  CamlinternalFormatBasics 8f8f634558798ee408df3c50a5539b15
  B 903ddd9b7c0fa4ee6d34b4af6358d1e1
  A 21e6137bd9b3aaa3c66960387b5f32c0
