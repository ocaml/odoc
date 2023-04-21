module Parser = Query_parser
module Succ = Succ
module Sort = Sort

val find_inter :
  shards:Db.Storage.t list -> Db.String_list_map.key list -> Succ.t Lwt.t

val find_names : shards:Db.Storage.t list -> string list -> Succ.t Lwt.t

type t =
  { query : string
  ; packages : string list
  ; limit : int
  }

val api : shards:Db.Storage.t list -> t -> (string * Db.Elt.t list) Lwt.t
(* TODO : drop the Lwt thing *)