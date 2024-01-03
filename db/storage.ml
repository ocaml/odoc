module Occurences = Map.Make (Int)

type db =
  { db_names : Suffix_tree.With_elts.reader
  ; db_pos_types : Suffix_tree.With_elts.reader Occurences.t
  ; db_neg_types : Suffix_tree.With_elts.reader Occurences.t
  }

module type S = sig
  type writer

  val open_out : string -> writer
  val save : db:writer -> db -> unit
  val close_out : writer -> unit
  val load : string -> db list
end
