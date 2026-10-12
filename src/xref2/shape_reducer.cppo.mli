(* Shape reduction, built once per resolver and stored in it. The reducer
   owns the memoisation tables of [Shape_reduce.Make], so queries made
   through the same resolver share work. *)

type t

val make :
  lookup_impl:(string -> Odoc_model.Lang.Implementation.t option) -> t

#if OCAML_VERSION >= (4, 14, 0)
val reduce_for_uid : t -> Shape.t -> Shape.Uid.t option
#endif
