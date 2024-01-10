type t =
  { name : string list
  ; typ : [ `typ of Db.Typexpr.t | `no_typ | `parse_error ]
  }

val of_string : string -> t
val to_string : t -> string
