(** [Typename.string tn] is a string representing the type name of [tn] as a string.
    Such a function could be provided by Odoc but we do two things differently :
    - Core types like [int] and [string] are represented as [Stdlib.int] or [Stdlib.string]
    - We do not use any parenthesis on functors. *)
val to_string :
   Odoc_model.Paths.Path.Type.t -> string
