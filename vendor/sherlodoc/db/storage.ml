module Occurences = Map.Make (Int)

type db =
  { db_names : String_automata.t
  ; db_pos_types : String_automata.t Occurences.t
  ; db_neg_types : String_automata.t Occurences.t
  }

module type S = sig
  type writer

  val open_out : string -> writer
  val save : db:writer -> db -> unit
  val close_out : writer -> unit
  val load : string -> db list
end
