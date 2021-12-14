This test checks for handling of type substitutions.
Specifically, there was an issue where the code in this test
caused an exception during compilation. The absence of the
exception shows this working correctly.

  $ ocamlc -c irmin_layers_intf.ml -bin-annot -I .
  $ odoc compile -I . irmin_layers_intf.cmt


