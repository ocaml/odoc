(* A string automata, constructed from a suffix tree and optimized
   for fast queries and small serialization. *)

type terminals =
  | Empty
  | Terminals of Entry.t array
  | Summary of Entry.t array

type node =
  { start : int
  ; len : int
  ; size : int
  ; terminals : terminals
  ; children : node array option
  }

type t =
  { str : string
  ; t : node
  }

val find : t -> string -> t option
val find_star : t -> string -> t list
val minimum : t -> Entry.t
val size : t -> int
