(* This is defined in a standalone file to avoid dependency cycles*)

type t =
  { db_names : Suffix_tree.With_elts.reader
  ; db_types : Suffix_tree.With_occ.reader
  }
