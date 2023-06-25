module String_list_map = Map.Make (struct
  type t = string list

  let compare = List.compare String.compare
end)

let regroup lst =
  String_list_map.bindings
  @@ List.fold_left
       (fun acc s ->
         let count = try String_list_map.find s acc with Not_found -> 0 in
         String_list_map.add s (count + 1) acc)
       String_list_map.empty lst

type sgn =
  | Pos
  | Neg
  | Unknown

let string_of_sgn = function
  | Pos -> "+"
  | Neg -> "-"
  | Unknown -> "+"

let sgn_not = function
  | Pos -> Neg
  | Neg -> Pos
  | Unknown -> Unknown

type t =
  { db_names : Suffix_tree.With_elts.reader
  ; db_types : Suffix_tree.With_occ.reader
  }
