module Parser = Query_parser
module Succ = Succ
module Sort = Sort

type t =
  { query : string
  ; packages : string list
  ; limit : int
  }

val api : shards:Db.t list -> t -> string * Db.Elt.t list

(** For testing *)
module Private : sig
  module Array_succ = Array_succ
end
