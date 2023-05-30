type t =
  { db_types : Types.db
  ; db_names : Elt.Set.t Trie.t
  }

module type S = sig
  type writer

  val open_out : string -> writer
  val save : db:writer -> t -> unit
  val close_out : writer -> unit
  val load : string -> t list
end
