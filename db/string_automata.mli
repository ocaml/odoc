(* A string automata, constructed from a suffix tree and optimized
   for fast queries and small serialization. *)

type node =
  { start : int
  ; len : int
  ; terminals : Entry.Array.t
  ; children : node array option
  }

type t =
  { str : string
  ; t : node
  }

val empty : t
val find : t -> string -> t option
val find_star : t -> string -> t list
val minimum : t -> Entry.t
