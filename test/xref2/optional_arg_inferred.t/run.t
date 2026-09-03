Optional arguments of values whose signature is inferred from the
implementation (a module with no mli). From OCaml 5.5 the compiler wraps the
argument types of inferred arrows in a trivial [Tpoly] node, which must not
stop odoc from recognising the [option] beneath it.

  $ cat test.ml
  let f ?force x = ignore force; x
  let exit = f

  $ ocamlc -c -bin-annot test.ml
  $ odoc compile test.cmt
  $ odoc link test.odoc

The labels should be [Optional]; [RawOptional] renders as [?force:???]:

  $ odoc_print test.odocl | jq -c '.. | .["Arrow"]? | select(.) | .[0]'
  {"Some":{"Optional":"force"}}
  "None"
  {"Some":{"Optional":"force"}}
  "None"
