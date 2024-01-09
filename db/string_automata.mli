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

val find : t -> string -> t option

val sets_tree
  :  union:('a -> 'a -> 'a)
  -> terminal:(Entry.Array.t -> 'a)
  -> union_of_array:('a array -> 'a)
  -> t
  -> 'a
