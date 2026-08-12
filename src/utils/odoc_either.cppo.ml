#if OCAML_VERSION < (4, 12, 0)
type ('a, 'b) t =
  | Left of 'a
  | Right of 'b
#else
include Either
#endif
