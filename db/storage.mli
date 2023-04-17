type t =
{ db : Types.db
; db_names : Types.Elt_set.t Types.Tchar.t
}


module type S = sig
  type writer

  val open_out : string -> writer
  val save : db:writer -> t -> unit
  val close_out : writer -> unit
  val load : string -> t list
end

module Ancient : S 

module Marshal : S