module Parser = Query_parser
module Succ = Succ
module Sort = Sort

type t =
  { query : string
  ; packages : string list
  ; limit : int
  }

val api : shards:Db.Elt.t array Db.t list -> t -> string * Db.Elt.t list
