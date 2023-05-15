module Elt = Elt
module Types = Types
module Storage_toplevel = Storage
module Trie = Trie
module Caches = Caches
include Types
open Caches

let list_of_string s = List.init (String.length s) (String.get s)

module type S = sig
  type writer

  val optimize : unit -> unit
  val export : writer -> unit
  val store_type : Elt.t -> char list list -> unit
  val store_word : string -> Elt.t -> unit
  val load_counter : int ref
end

module Make (Storage : Storage.S) : S with type writer = Storage.writer = struct
  type writer = Storage.writer

  let load_counter = ref 0
  let db = ref (Trie.empty ())
  let db_names = ref (Trie.empty ())

  module Hset2 = Hashtbl.Make (struct
    type t = Elt.Set.t * Elt.Set.t

    let hash = Hashtbl.hash
    let equal (a, b) (a', b') = a == a' && b == b'
  end)

  module Hocc2 = Hashtbl.Make (struct
    type t = Elt.Set.t Occ.t * Elt.Set.t Occ.t

    let hash = Hashtbl.hash
    let equal (a, b) (a', b') = a == a' && b == b'
  end)

  let elt_set_union ~hs a b =
    try Hset2.find hs (a, b)
    with Not_found ->
      let r = Elt.Set.union a b in
      Hset2.add hs (a, b) r ;
      Hset2.add hs (b, a) r ;
      r

  let occ_merge ~hs a b =
    if a == b
    then a
    else
      Occ.merge
        (fun _ ox oy ->
          match ox, oy with
          | Some x, Some y -> Some (elt_set_union ~hs x y)
          | opt, None | None, opt -> opt)
        a b

  let occ_merge ~ho ~hs a b =
    try Hocc2.find ho (a, b)
    with Not_found ->
      let r = occ_merge ~hs a b in
      Hocc2.add ho (a, b) r ;
      Hocc2.add ho (b, a) r ;
      r

  let optimize () =
    let ho = Hocc2.create 16 in
    let hs = Hset2.create 16 in
    let (_ : Elt.Set.t Occ.t option) = Trie.summarize (occ_merge ~ho ~hs) !db in
    let (_ : Elt.Set.t option) = Trie.summarize (elt_set_union ~hs) !db_names in
    ()

  let export h =
    load_counter := 0 ;
    let t = { Storage_toplevel.db = !db; db_names = !db_names } in
    Storage.save ~db:h t ;
    db := Trie.empty () ;
    db_names := Trie.empty ()

  module Hset = Hashtbl.Make (struct
    type t = Elt.Set.t option

    let hash = Hashtbl.hash
    let equal x y = Option.equal (fun x y -> x == y) x y
  end)

  module Hocc = Hashtbl.Make (struct
    type t = Elt.Set.t Occ.t option

    let hash = Hashtbl.hash
    let equal x y = Option.equal (fun x y -> x == y) x y
  end)

  let set_add elt = function
    | None -> Elt.Set.singleton elt
    | Some s -> Elt.Set.add elt s

  let set_add ~hs elt opt =
    try Hset.find hs opt
    with Not_found ->
      let r = set_add elt opt in
      Hset.add hs opt r ;
      r

  let candidates_add ~hs elt ~count = function
    | None -> Occ.singleton count (set_add ~hs elt None)
    | Some m ->
        let s = Occ.find_opt count m in
        let s = set_add ~hs elt s in
        Occ.add count s m

  let candidates_add ~ho ~hs elt ~count opt =
    try Hocc.find ho opt
    with Not_found ->
      let r = candidates_add ~hs ~count elt opt in
      Hocc.add ho opt r ;
      r

  let store ~ho ~hs name elt ~count =
    let rec go db name =
      match name with
      | [] -> db
      | _ :: next ->
          incr load_counter ;
          let db = Trie.add name (candidates_add ~ho ~hs elt ~count) db in
          go db next
    in
    db := go !db name

  let store_type elt paths =
    let ho = Hocc.create 16 in
    let hs = Hset.create 16 in
    List.iter
      (fun (path, count) -> store ~ho ~hs ~count path elt)
      (regroup_chars paths)

  let store_chars name elt =
    let hs = Hset.create 16 in
    let rec go db = function
      | [] -> db
      | _ :: next as name ->
          incr load_counter ;
          let db = Trie.add name (set_add ~hs elt) db in
          go db next
    in
    db_names := go !db_names name

  let store_word word elt =
    (word |> list_of_string |> List.rev |> Cache_list.memo |> store_chars) elt
end

module Storage = Storage
