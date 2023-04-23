module Elt = struct
  type t =
    { cost : int
    ; name : string
    ; str_type : string
    ; type_paths : string list list
    ; doc : string option
    ; pkg : string * string
    }

  let compare_pkg (a_name, _) (b_name, _) = String.compare a_name b_name

  let compare a b =
    match Int.compare a.cost b.cost with
    | 0 -> begin
        match String.compare a.name b.name with
        | 0 -> begin
            match compare_pkg a.pkg b.pkg with
            | 0 -> String.compare a.str_type b.str_type
            | c -> c
          end
        | c -> c
      end
    | c -> c

  let compare a b = if a == b then 0 else compare a b

  let pkg_link { pkg = pkg, v; _ } =
    Printf.sprintf "https://ocaml.org/p/%s/%s" pkg v

  let link t =
    let name, path =
      match List.rev (String.split_on_char '.' t.name) with
      | name :: path -> name, String.concat "/" (List.rev path)
      | _ -> "", ""
    in
    pkg_link t ^ "/doc/" ^ path ^ "/index.html#val-" ^ name
end

module String_list_map = Map.Make (struct
  type t = string list

  let compare = Stdlib.compare
end)

let regroup lst =
  String_list_map.bindings
  @@ List.fold_left
       (fun acc s ->
         let count = try String_list_map.find s acc with Not_found -> 0 in
         String_list_map.add s (count + 1) acc)
       String_list_map.empty lst

module Int_map = Map.Make (Int)
module Elt_set = Set.Make (Elt)
module Occ = Int_map

type candidates = Elt_set.t Occ.t
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
