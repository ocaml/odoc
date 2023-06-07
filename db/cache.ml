open Common

let clears = ref []
let clear () = Common.List.iter (fun f -> f ()) !clears

module type Cached = sig
  type t

  val memo : t -> t
end

(** The result of the [Make] functor. [equal] and [hash] are reexported for 
    composability with other functors. *)
module type Memo = sig
  include Cached

  val equal : t -> t -> bool
  val hash : t -> int
end

(** This module specifies what is need to construct a cache. *)
module type Cachable = sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int

  val sub : memo:(t -> t) -> t -> t
  (** [sub ~memo (v : t)] should replace subvalues [v'] of type [t] by [memo v'],
      and subvalues [a] of type [A.t] by [A.memo a]. *)
end

(** Builds a cache from an cachable type.*)
module Make (Elt : Cachable) : Memo with type t = Elt.t = struct
  type t = Elt.t

  let equal = Elt.equal
  let hash = Elt.hash

  module H = Hashtbl.Make (struct
    type t = Elt.t

    let equal = Elt.equal
    let hash = Elt.hash
  end)

  let cache = H.create 16
  let () = clears := (fun () -> H.clear cache) :: !clears

  let rec memo str =
    try H.find cache str
    with Not_found ->
      let str = Elt.sub ~memo str in
      H.add cache str str ;
      str
end

(** Does not build a cache, but exposes functions that caches that subvalues of 
    a given cache. This is useful for big value with a lot of subvalues, an 
    expansive [hash] and [equal] function, and not a lot of opportunities for 
    sharing. *)
module Make_sub_only (Elt : Cachable) : Memo with type t = Elt.t = struct
  type t = Elt.t

  let equal = ( = )
  let hash = Hashtbl.hash
  let rec memo str = Elt.sub ~memo str
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

  let sub ~memo:_ str =
    (* not returning [str] here is required by [Ancient]. *)
    String.init (String.length str) (String.get str)
end)

module Option (A : Memo) = Make (struct
  type t = A.t option

  let equal = Option.equal A.equal
  let hash = Option.hash A.hash

  let sub ~memo:_ opt =
    match opt with
    | Some a -> Some (A.memo a)
    | None -> None
end)

module List (A : Memo) = Make (struct
  type t = A.t list

  let hash = List.hash A.hash
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
  let sub ~memo:_ arr = Array.map A.memo arr
end)

module Char_list = List (Char)
module String_list = List (String)
module String_list_list = List (String_list)

module Type = Make (struct
  type t = Elt.type_path

  let equal = ( = )
  let hash = Hashtbl.hash

  let sub ~memo:_ { Elt.str; paths } =
    { Elt.str = String.memo str; paths = String_list_list.memo paths }
end)

module Kind = Make (struct
  include Elt.Kind

  let sub ~memo:_ k =
    match k with
    | Constructor type_path -> Constructor (Type.memo type_path)
    | Field type_path -> Field (Type.memo type_path)
    | Val type_path -> Val (Type.memo type_path)
    | _ -> k
end)

module Package = Make (struct
  include Elt.Package

  let sub ~memo:_ { name; version } =
    { name = String.memo name; version = String.memo version }
end)

module Package_option = Option (Package)

module Elt = struct
  include Make (struct
    module Kind_memo = Kind
    include Elt

    let sub ~memo:_ Elt.{ name; kind; doc_html; pkg; json_display } =
      let name = String.memo name in
      let json_display = String.memo json_display in
      let doc_html = String.memo doc_html in
      (* For unknown reasons, this causes a terrible performance drop. *)
      (* let kind = Kind_memo.memo kind in *)
      Elt.{ name; kind; doc_html; pkg; json_display }
  end)

  module Set = Elt.Set
end

module Elt_array = Array (Elt)

module Set (A : Memo) (S : Set.S with type elt = A.t) = Make (struct
  type t = S.t

  let equal = S.equal
  let hash m = m |> S.elements |> Common.List.hash A.hash

  let sub ~memo set =
    match set with
    | S.Empty -> S.Empty
    | S.Node { l; v; r; h } ->
        (* This shares subset. Not actually very useful on tested exemples. *)
        let l = memo l in
        let v = A.memo v in
        let r = memo r in
        S.Node { l; v; r; h }
end)

module Map (A : Memo) (M : Map.S) = Make (struct
  type t = A.t M.t

  let equal = M.equal A.equal

  let hash m =
    m |> M.bindings
    |> Common.List.map (fun (k, v) -> k, A.hash v)
    |> Hashtbl.hash

  let sub ~memo m =
    match m with
    | M.Empty -> M.Empty
    | M.Node { l; v; d; r; h } ->
        (* This shares submaps ! *)
        let l = memo l in
        let r = memo r in
        let d = A.memo d in
        M.Node { l; v; d; r; h }
end)

module Elt_set = Set (Elt) (Elt.Set)
module Elt_set_option = Option (Elt_set)
module Char_map (A : Memo) = Map (A) (Char.Map)
module Int_map (A : Memo) = Map (A) (Int.Map)
module Elt_array_occ = Int_map (Elt_array)
module Elt_set_occ = Int_map (Elt_set)
module Elt_set_char_map = Char_map (Elt_set)

module Trie (A : Memo) : Memo with type t = A.t Trie.t = struct
  module A_option = Option (A)

  (* Here [Make_sub_only] is good enough. Using [Make] instead slows down the
     [Base] test by 50s for a 20ko gain. *)
  module rec M : (Memo with type t = A.t Trie.t) = Make_sub_only (struct
    type t = A.t Trie.t

    let equal = Trie.equal A.equal

    let hash trie =
      let open Trie in
      match trie with
      | Leaf (chars, a) ->
          Hashtbl.hash
            (Leaf (Obj.magic @@ Char_list.hash chars, Obj.magic @@ A.hash a))
      | Node { leaf; children } ->
          Hashtbl.hash
            (Node
               { leaf = Obj.magic @@ A_option.hash leaf
               ; children = Obj.magic @@ Children.hash children
               })

    let sub ~memo:_ trie =
      let open Trie in
      match trie with
      | Leaf (chars, elts) -> Leaf (Char_list.memo chars, A.memo elts)
      | Node { leaf; children } ->
          let leaf = A_option.memo leaf in
          let children = Children.memo children in
          Node { leaf; children }
  end)

  and Children : (Memo with type t = A.t Trie.t Char.Map.t) = Char_map (M)

  include M
end

module Elt_set_trie = Trie (Elt_set)
module Elt_set_occ_trie = Trie (Elt_set_occ)
module Elt_array_trie = Trie (Elt_array)
module Elt_array_occ_trie = Trie (Elt_array_occ)
