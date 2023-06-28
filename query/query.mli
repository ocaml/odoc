module Parser = Query_parser
module Succ = Succ
module Dynamic_cost = Dynamic_cost

type t =
  { query : string
  ; packages : string list
  ; limit : int
  }

val api : shards:Db.t list -> ?dynamic_sort:bool -> t -> string * Db.Elt.t list

(** For testing *)
module Private : sig
  module Array_succ = Array_succ
end
