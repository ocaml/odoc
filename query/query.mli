type t =
  { query : string
  ; packages : string list
  ; limit : int
  }

val api : shards:Db.t list -> ?dynamic_sort:bool -> t -> string * Db.Elt.t list

(** For testing *)
module Private : sig
  module Array_succ = Array_succ
  module Succ = Succ
end
