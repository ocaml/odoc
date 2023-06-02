open Common

let clears = ref []
let clear () = Common.List.iter (fun f -> f ()) !clears

module type Elt = sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
  val sub : memo:(t -> t) -> t -> t
end

module type Memo = sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
  val memo : t -> t
end

module Make (Element : Elt) : Memo with type t = Element.t = struct
  type t = Element.t

  let equal = Element.equal
  let hash = Element.hash

  module H = Hashtbl.Make (struct
    type t = Element.t

    let equal = Element.equal
    let hash = Element.hash
  end)

  let cache = H.create 16
  let () = clears := (fun () -> H.clear cache) :: !clears

  let rec memo str =
    try H.find cache str
    with Not_found ->
      let str = Element.sub ~memo str in
      H.add cache str str ;
      str
end

module Make_sub_only (Element : Elt) : Memo with type t = Element.t = struct
  type t = Element.t

  let equal = ( = )
  let hash = Hashtbl.hash
  let rec memo str = Element.sub ~memo str
end

(** This module does not use {!Make} because it does not actually cache anything, 
    its just here for composition. *)
module Char = struct
  type t = char

  let equal = Char.equal
  let hash = Hashtbl.hash
  let memo c = c

  module Map = Char.Map
  module Array_map = Char.Array_map
end

module String = Make (struct
  type t = string

  let hash = Hashtbl.hash
  let equal = String.equal
  let sub ~memo:_ str = String.init (String.length str) (String.get str)
end)

module List (A : Memo) = Make (struct
  type t = A.t list

  let hash li = li |> List.map A.hash |> Hashtbl.hash
  let equal = List.equal A.equal

  let rec sub ~memo lst =
    match lst with
    | [] -> []
    | x :: xs -> A.memo x :: memo (sub ~memo xs)
end)

module Array (A : Memo) = Make (struct
  type t = A.t array

  let equal = Array.equal A.equal
  let hash = Array.hash A.hash
  let sub ~memo:_ arr = arr
end)

module Char_list = List (Char)
module String_list = List (String)
module String_list_list = List (String_list)

module Elt = struct
  include Make (struct
    include Elt

    let sub ~memo:_ { name; kind; has_doc; pkg; json_display } =
      let name = String.memo name in
      let json_display = String.memo json_display in
      { name; kind; has_doc; pkg; json_display }
  end)

  module Set = Elt.Set
end

module Elt_array = Array (Elt)

module Set (A : Memo) (S : Set.S with type elt = A.t) = Make (struct
  type t = S.t

  let equal = S.equal

  let hash m =
    m |> S.elements |> Common.List.map (fun v -> A.hash v) |> Hashtbl.hash

  let sub ~memo:_ = S.map A.memo
end)

module Map (A : Memo) (M : Map.S) = Make (struct
  type t = A.t M.t

  let equal = M.equal A.equal

  let hash m =
    m |> M.bindings
    |> Common.List.map (fun (k, v) -> k, A.hash v)
    |> Hashtbl.hash

  let sub ~memo m = match m with
      M.Empty -> M.Empty
    | M.Node  {l; v; d; r; h} ->
        let l = memo l in
        let r = memo r in
        let d = A.memo d in
        M.Node  {l; v; d; r; h}
end)

module Array_map (A : Memo) (M : Array_map.S) = Make (struct
  type t = A.t M.t

  let equal = M.equal A.equal

  let hash m =
    m |> M.to_array
    |> Common.Array.map (fun (k, v) -> k, A.hash v)
    |> Hashtbl.hash

  let sub ~memo:_ m = M.map ~f:A.memo m
end)

module Elt_set = Set (Elt) (Elt.Set)

module Option (A : Memo) = Make (struct
  type t = A.t option

  let equal = Option.equal A.equal

  let hash opt =
    match opt with
    | None -> Hashtbl.hash None
    | Some a -> Hashtbl.hash (Some (A.hash a))

  let sub ~memo:_ opt =
    match opt with
    | Some a -> Some (A.memo a)
    | None -> None
end)

module Elt_set_option = Option (Elt_set)
module Char_map (A : Memo) = Map (A) (Char.Map)
module Int_map (A : Memo) = Map (A) (Int.Map)
module Char_array_map (A : Memo) = Array_map (A) (Char.Array_map)
module Elt_array_occ = Int_map(Elt_array)
module Elt_set_occ = Int_map (Elt_set)
module Elt_set_char_map = Char_map (Elt_set)

module Trie_gen (A : Memo) : Memo with type t = A.t Trie_gen.t = struct
  module rec M : (Memo with type t = A.t Trie_gen.t) = Make_sub_only (struct
    module Map = Char_map (A)
    module Option = Option (A)

    type t = A.t Trie_gen.t

    let equal = ( = )

    let hash trie =
      let open Trie_gen in
      match trie with
      | Leaf _ -> Hashtbl.hash trie
      | Node { leaf; children } ->
          Hashtbl.hash (Hashtbl.hash leaf, Children.hash children)

    let sub ~memo:_ trie =
      let open Trie_gen in
      match trie with
      | Leaf (chars, elts) -> Leaf (Char_list.memo chars, A.memo elts)
      | Node { leaf; children } ->
          let leaf = Option.memo leaf in
          let children = Children.memo children in
          Node { leaf; children }
  end)

  and Children : (Memo with type t = A.t Trie_gen.t Char.Map.t) = Char_map (M)

  include M
end

module Trie_compact (A : Memo) : Memo with type t = A.t Trie_compact.t = struct
  module rec M : (Memo with type t = A.t Trie_compact.t) = Make_sub_only (struct
    module Map = Char_map (A)
    module Option = Option (A)

    type t = A.t Trie_compact.t

    let equal = ( = )

    let hash trie =
      let open Trie_compact in
      match trie with
      | Leaf _ -> Hashtbl.hash trie
      | Node { leaf; children } ->
          Hashtbl.hash (Hashtbl.hash leaf, Children.hash children)

    let sub ~memo:_ trie =
      let open Trie_compact in
      match trie with
      | Leaf (chars, elts) -> Leaf (String.memo chars, A.memo elts)
      | Node { leaf; children } ->
          let leaf = Option.memo leaf in
          let children = Children.memo children in
          Node { leaf; children }
  end)

  and Children : (Memo with type t = A.t Trie_compact.t Char.Array_map.t) =
    Char_array_map (M)

  include M
end

module Elt_set_trie_gen = Trie_gen (Elt_set)
module Elt_set_occ_trie_gen = Trie_gen (Elt_set_occ)

module Elt_array_trie_gen = Trie_gen (Elt_array)
module Elt_array_occ_trie_gen = Trie_gen (Elt_array_occ)