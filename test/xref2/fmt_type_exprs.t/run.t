Short output prints type expressions with the brackets that OCaml needs to
read them back in the same way. A tuple prints its own brackets.

  $ cat t.mli
  type tuple_arg = (int * int) list
  type arrow_arg = (int -> int) list
  type arrows = int -> int -> int
  type arrow_left = (int -> int) -> int
  type labelled = l:(int -> int) -> ?o:int -> int
  type two_args = (int * int, string) result
  type tuple_left = int * int -> int
  type arrow_in_tuple = int * (int -> int)
  type tuple_in_tuple = (int * int) * int
  $ ocamlc -bin-annot -c t.mli
  $ odoc compile t.cmti
  $ odoc link t.odoc
  $ for t in tuple_arg arrow_arg arrows arrow_left labelled two_args \
  >   tuple_left arrow_in_tuple tuple_in_tuple; do
  >   odoc_print --short -r $t t.odocl; echo
  > done
  type T.tuple_arg  = (int * int) list
  type T.arrow_arg  = (int -> int) list
  type T.arrows  = int -> int -> int
  type T.arrow_left  = (int -> int) -> int
  type T.labelled  = l:(int -> int) -> ?o:int -> int
  type T.two_args  = ((int * int), string) Stdlib.result
  type T.tuple_left  = (int * int) -> int
  type T.arrow_in_tuple  = (int * (int -> int))
  type T.tuple_in_tuple  = ((int * int) * int)
