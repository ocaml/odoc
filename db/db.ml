open Common
module Elt = Elt
module Types = Types
module Storage_toplevel = Storage
module Trie = Trie
module Cache = Cache
include Types
module Occ = Int.Map

let trie_with_array trie =
  Trie.map_leaf ~f:(fun set -> set |> Elt.Set.to_seq |> Array.of_seq) trie

let trie_with_set trie =
  Trie.map_leaf ~f:(fun arr -> arr |> Array.to_seq |> Elt.Set.of_seq) trie

let trie_with_array_occ trie =
  Trie.map_leaf
    ~f:(fun occs ->
      occs |> Int.Map.map (fun set -> set |> Elt.Set.to_seq |> Array.of_seq))
    trie

let trie_with_set_occ trie =
  Trie.map_leaf
    ~f:(fun occs ->
      occs |> Int.Map.map (fun arr -> arr |> Array.to_seq |> Elt.Set.of_seq))
    trie

let compact db =
  let open Types in
  let { db_types; db_names } = db in
  let t0 = Unix.gettimeofday () in
  let db_types = trie_with_array_occ db_types in
  let t1 = Unix.gettimeofday () in
  let db_names = trie_with_array db_names in
  let t2 = Unix.gettimeofday () in
  let db_types = Cache.Elt_array_occ_trie_.memo db_types in
  let t3 = Unix.gettimeofday () in
  let db_names = Cache.Elt_array_trie_.memo db_names in
  let t4 = Unix.gettimeofday () in
  Printf.printf
    "trie_with_array_occ:%.2fs\n\
     trie_with_array:%.2fs\n\
     Cache.Elt_array_occ_trie.memo:%.2fs\n\
     Cache.Elt_array_trie.memo:%.2fs\n\
     %!"
    (t1 -. t0) (t2 -. t1) (t3 -. t2) (t4 -. t3) ;
  { db_types; db_names }

let list_of_string s = List.init (String.length s) (String.get s)

let list_of_string_rev s =
  let len = String.length s in
  List.init len (fun i -> String.get s (len - i - 1))

module type S = sig
  type writer

  val export : writer -> unit
  val store_type_paths : Elt.t -> string list list -> unit
  val store_word : string -> Elt.t -> unit
  val load_counter : int ref
end

module Make (Storage : Storage.S) : S with type writer = Storage.writer = struct
  type writer = Storage.writer

  let load_counter = ref 0
  let db_types = ref Trie.empty
  let db_names = ref Trie.empty

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

  let export h =
    load_counter := 0 ;
    let db = { db_types = !db_types; db_names = !db_names } in
    let db = compact db in
    Storage.save ~db:h db ;
    db_types := Trie.empty ;
    db_names := Trie.empty

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
    db_types := go !db_types name

  let store_type_paths elt paths =
    let ho = Hocc.create 16 in
    let hs = Hset.create 16 in
    List.iter
      (fun (path, count) -> store ~ho ~hs ~count path elt)
      (regroup_chars paths)

  let store_type_paths elt paths =
    store_type_paths elt
      (List.map
         (fun xs ->
           let xs = List.concat_map list_of_string xs in
           xs)
         paths)

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

  let store_word word elt = (word |> list_of_string_rev |> store_chars) elt
end

module Storage = Storage
