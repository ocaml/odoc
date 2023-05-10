open Common

let regroup lst =
  String_list_map.bindings
  @@ List.fold_left
       (fun acc s ->
         let count = try String_list_map.find s acc with Not_found -> 0 in
         String_list_map.add s (count + 1) acc)
       String_list_map.empty lst

let regroup_chars lst =
  Char_list_map.bindings
  @@ List.fold_left
       (fun acc s ->
         let count = try Char_list_map.find s acc with Not_found -> 0 in
         Char_list_map.add s (count + 1) acc)
       Char_list_map.empty lst

module Occ = Int.Map

type candidates = Elt.Set.t Occ.t
type db = candidates Trie.t

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
