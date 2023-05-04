open Common

module Elt = struct
  type kind =
    | Doc
    | TypeDecl of { html : string }
    | Module
    | Exception
    | Class_type
    | Method
    | Class
    | TypeExtension
    | ExtensionConstructor
    | ModuleType
    | Constructor
    | Field
    | FunctorParameter
    | ModuleSubstitution
    | ModuleTypeSubstitution
    | InstanceVariable
    | Val of
        { str_type : string
              (** A type can viewed as a tree.
            [a -> b -> c * d] is the following tree :
            {[ ->
              |- a
              |- ->
                 |- b
                 |- *
                    |- c
                    |- d 
            ]} 
            [type_paths] is the list of paths from root to leaf in the tree of 
            the type. There is an annotation to indicate the child's position.
            Here it would be :
            [ [["->";"0"; "a"];["->"; "1"; "->"; "0"; "b"]; ...] ]
            
            It is used to sort results. *)
        ; type_paths : string list list
        }

  type package =
    { name : string
    ; version : string
    }

  type t =
    { cost : int
    ; name : string
    ; kind : kind
    ; doc : string option
    ; pkg : package option
    }

  let compare_pkg { name; version = _ } (b : package) =
    String.compare name b.name

  let compare a b =
    match Int.compare a.cost b.cost with
    | 0 -> begin
        match String.compare a.name b.name with
        | 0 -> begin
            match Option.compare compare_pkg a.pkg b.pkg with
            | 0 -> Stdlib.compare a.kind b.kind
            | c -> c
          end
        | c -> c
      end
    | c -> c

  let compare a b = if a == b then 0 else compare a b

  let pkg_link { pkg; _ } =
    let open Option.O in
    let+ { name; version } = pkg in
    Printf.sprintf "https://ocaml.org/p/%s/%s" name version

  let link t =
    let open Option.O in
    let name, path =
      match List.rev (String.split_on_char '.' t.name) with
      | name :: path -> name, String.concat "/" (List.rev path)
      | _ -> "", ""
    in
    let+ pkg_link = pkg_link t in
    pkg_link ^ "/doc/" ^ path ^ "/index.html#val-" ^ name
end

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

module Char_list_map = Map.Make (struct
  type t = char list

  let compare = List.compare Char.compare
end)

let regroup_chars lst =
  Char_list_map.bindings
  @@ List.fold_left
       (fun acc s ->
         let count = try Char_list_map.find s acc with Not_found -> 0 in
         Char_list_map.add s (count + 1) acc)
       Char_list_map.empty lst

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
