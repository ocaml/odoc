open Common

type uid = int

let clears = ref []
let clear () = Common.List.iter (fun f -> f ()) !clears

module type Cached = sig
  type t

  val memo : t -> t
end

(** The result of the [Make] functor. [equal] and [hash] are reexported for 
    composability with other functors. *)
module type Memo = sig
  type t

  val memo : t -> uid * t
end

(** This module specifies what is need to construct a cache. *)
module type Cachable = sig
  type t
  type key

  val sub : memo:(t -> uid * t) -> t -> key * t
  (** [sub ~memo (v : t)] is [(k, v')]. [v'] should be equal to [v], and [k] a
      hashable shallow copy of [v].
      For every subvalue [vs] of type [t] we have [ks, vs' = memo vs]. 
      In [k], [vs] is replaced by [ks].
      In [v], [vs] is replaced by [vs'].

      and subvalues [a] of type [A.t] by [A.memo a]. *)
end

(** Builds a cache from an cachable type.*)
module Make (Elt : Cachable) : Memo with type t = Elt.t = struct
  type t = Elt.t

  let equal = ( = )
  let hash = Hashtbl.hash

  let new_uid =
    let i = ref 0 in
    fun () ->
      let r = !i in
      i := r + 1 ;
      r

  module H = Hashtbl.Make (struct
    type t = Elt.key

    let equal = equal
    let hash = hash
  end)

  let cache : (int * t) H.t = H.create 16
  let () = clears := (fun () -> H.clear cache) :: !clears

  let rec memo elt : int * t =
    let key, elt = Elt.sub ~memo elt in
    match H.find cache key with
    | uid, elt -> uid, elt
    | exception Not_found ->
        let uid = new_uid () in

        H.add cache key (uid, elt) ;
        uid, elt
end

(** Does not build a cache, but exposes functions that caches that subvalues of 
    a given cache. This is useful for big value with a lot of subvalues, an 
    expansive [hash] and [equal] function, and not a lot of opportunities for 
    sharing. *)
(*module Make_sub_only (Elt : Cachable) : Memo with type t = Elt.t = struct
    type t = Elt.t
    type key = Elt.key

    let equal = Elt.equal
    let hash = Elt.hash
    let rec memo str = Elt.sub ~memo str
  end
*)

module Strip (Memo : Memo) : Cached with type t = Memo.t = struct
  type t = Memo.t

  let memo elt =
    let _, elt = Memo.memo elt in
    elt
end

(** This module does not use {!Make} because it does not actually cache anything, 
    its just here for composition. *)
module Char = struct
  type t = char

  let memo c = Char.code c, c

  module Map = Char.Map
  module Array_map = Char.Array_map
end

module String = Make (struct
  type t = string
  type key = string

  let sub ~memo:_ str = str, str
end)

module Option (A : Memo) = Make (struct
  type t = A.t option
  type key = uid option

  let sub ~memo:_ opt =
    match opt with
    | Some a ->
        let uid, a = A.memo a in
        Some uid, Some a
    | None -> None, None
end)

module List (A : Memo) = Make (struct
  type t = A.t list

  type key =
    | Empty
    | Cons of uid * uid

  let sub ~memo lst =
    match lst with
    | [] -> Empty, []
    | elt :: li ->
        let uid_elt, elt = A.memo elt in
        let uid_li, li = memo li in
        Cons (uid_elt, uid_li), elt :: li
end)

module Array (A : Memo) = Make (struct
  type t = A.t array
  type key = uid array

  let sub ~memo:_ arr =
    let arr = Array.map A.memo arr in
    let key = Array.map (fun (key, _) -> key) arr in
    let arr = Array.map (fun (_, elt) -> elt) arr in
    key, arr
end)

module Char_list = List (Char)
module String_list = List (String)
module String_list_list = List (String_list)
module String_option = Option (String)

module Kind = Make (struct
  type t = Elt.Kind.t
  type key = uid Elt.Kind.abstract

  let sub ~memo:_ k =
    let open Elt.Kind in
    match k with
    | Constructor type_ ->
        let uid, type_ = String_list_list.memo type_ in
        Constructor uid, Constructor type_
    | ExtensionConstructor type_ ->
        let uid, type_ = String_list_list.memo type_ in
        ExtensionConstructor uid, ExtensionConstructor type_
    | Field type_ ->
        let uid, type_ = String_list_list.memo type_ in
        Field uid, Field type_
    | Val type_ ->
        let uid, type_ = String_list_list.memo type_ in
        Val uid, Val type_
    (* the below looks like it could be [k -> (k, k) but it does not because of typing issues] *)
    | Doc -> Doc, Doc
    | TypeDecl -> TypeDecl, TypeDecl
    | Module -> Module, Module
    | Exception -> Exception, Exception
    | Class_type -> Class_type, Class_type
    | Method -> Method, Method
    | Class -> Class, Class
    | TypeExtension -> TypeExtension, TypeExtension
    | ModuleType -> ModuleType, ModuleType
end)

module Elt = struct
  include Make (struct
    type t = Elt.t

    type key =
      { name : uid
      ; kind : int
      }

    let int_of_kind =
      let open Elt.Kind in
      function
      | Constructor _ -> 0
      | Field _ -> 1
      | Val _ -> 2
      (* the below looks like it could be [k -> (k, k) but it does not because of typing issues] *)
      | Doc -> 3
      | TypeDecl -> 4
      | Module -> 5
      | Exception -> 6
      | Class_type -> 7
      | Method -> 8
      | Class -> 9
      | TypeExtension -> 10
      | ExtensionConstructor _ -> 11
      | ModuleType -> 12

    let sub ~memo:_ Elt.{ name; kind; doc_html; score; pkg; rhs; url } =
      let uid_name, name = String.memo name in
      let _uid_rhs, rhs = String_option.memo rhs in
      (*let _uid_kind, kind = Kind.memo kind in*)
      ( { name = uid_name; kind = int_of_kind kind }
      , Elt.{ name; kind; doc_html; pkg; rhs; score; url } )
  end)

  module Set = Elt.Set
end

module Elt_array = Array (Elt)

module Set (A : Memo) (S : Set.S with type elt = A.t) = Make (struct
  type t = S.t

  type key =
    | Empty
    | Node of
        { l : uid
        ; v : uid
        ; r : uid
        ; h : int
        }

  let sub ~memo set =
    match set with
    | S.Empty -> Empty, S.Empty
    | S.Node { l; v; r; h } ->
        (* This shares subset. Not actually very useful on tested exemples. *)
        let uid_l, l = memo l in
        let uid_v, v = A.memo v in
        let uid_r, r = memo r in
        Node { l = uid_l; v = uid_v; r = uid_r; h }, S.Node { l; v; r; h }
end)

module Map (A : Memo) (M : Map.S) = Make (struct
  type t = A.t M.t

  type key =
    | Empty
    | Node of
        { l : uid
        ; v : M.key
        ; d : uid
        ; r : uid
        ; h : int
        }

  let sub ~memo m =
    match m with
    | M.Empty -> Empty, M.Empty
    | M.Node { l; v; d; r; h } ->
        (* This shares submaps ! *)
        let uid_l, l = memo l in
        let uid_d, d = A.memo d in
        let uid_r, r = memo r in
        Node { l = uid_l; v; d = uid_d; r = uid_r; h }, M.Node { l; v; d; r; h }
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
  module rec M : (Memo with type t = A.t Trie.t) = Make (struct
    type t = A.t Trie.t

    type key =
      | Leaf of uid * uid
      | Node of
          { leaf : uid
          ; children : uid
          }

    let sub ~memo:_ trie : key * _ =
      let open Trie in
      match trie with
      | Leaf (chars, elts) ->
          let uid_chars, chars = Char_list.memo chars in
          let uid_elts, elts = A.memo elts in
          Leaf (uid_chars, uid_elts), Trie.Leaf (chars, elts)
      | Node { leaf; children } ->
          let uid_leaf, leaf = A_option.memo leaf in
          let uid_children, children = Children.memo children in
          ( Node { leaf = uid_leaf; children = uid_children }
          , Trie.Node { leaf; children } )
  end)

  and Children : (Memo with type t = A.t Trie.t Char.Map.t) = Char_map (M)

  include M
end

module Elt_set_trie = Trie (Elt_set)
module Elt_set_occ_trie = Trie (Elt_set_occ)
module Elt_array_trie = Trie (Elt_array)
module Elt_array_occ_trie = Trie (Elt_array_occ)

(* Hiding the uids *)
module String_ = Strip (String)
module Char_list_ = Strip (Char_list)
module String_list_ = Strip (String_list)
module String_list_list_ = Strip (String_list_list)
module Kind_ = Strip (Kind)
module Elt_array_ = Strip (Elt_array)
module Elt_set_trie_ = Strip (Elt_set_trie)
module Elt_set_occ_trie_ = Strip (Elt_set_occ_trie)
module Elt_array_trie_ = Strip (Elt_array_trie)
module Elt_array_occ_trie_ = Strip (Elt_array_occ_trie)
