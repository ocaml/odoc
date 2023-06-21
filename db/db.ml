module Elt = Elt
module Types = Types
module Storage_toplevel = Storage
module Suffix_tree = Suffix_tree
include Types
module Occ = Int.Map

let list_of_string s = List.init (String.length s) (String.get s)

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
  let db_types = Suffix_tree.With_occ.make ()
  let db_names = Suffix_tree.With_elts.make ()

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
    let db =
      { db_types = Suffix_tree.With_occ.export db_types
      ; db_names = Suffix_tree.With_elts.export db_names
      }
    in
    PPrint.ToChannel.pretty 0.8 120 stdout
      (Suffix_tree.With_elts.pprint db.db_names) ;
    Storage.save ~db:h db

  let store name elt ~count =
    Suffix_tree.With_occ.add_suffixes db_types name (count, elt)

  let store_type_paths elt paths =
    List.iter
      (fun (path, count) ->
        let word = String.concat "" path in
        store ~count word elt)
      (regroup paths)

  let store_word word elt =
    let word = word |> String.lowercase_ascii in
    Suffix_tree.With_elts.add_suffixes db_names word elt
end

module Storage = Storage
