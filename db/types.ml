module String_list_map = Map.Make (struct
  type t = string list

  let compare = List.compare String.compare
end)

type t =
  { db_names : Suffix_tree.With_elts.reader
  ; db_types : Suffix_tree.With_occ.reader
  }
