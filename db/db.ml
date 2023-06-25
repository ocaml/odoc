module Elt = Elt
module Types = Types
module Storage_toplevel = Storage
module Suffix_tree = Suffix_tree
module Occ = Occ
include Types

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

  let export h =
    load_counter := 0 ;
    let t0 = Unix.gettimeofday () in
    let db =
      { db_types = Suffix_tree.With_occ.export db_types
      ; db_names = Suffix_tree.With_elts.export db_names
      }
    in
    let t1 = Unix.gettimeofday () in
    Format.printf "Export in %fms@." (1000.0 *. (t1 -. t0)) ;
    Storage.save ~db:h db

  let store name elt ~count =
    Suffix_tree.With_occ.add_suffixes db_types name (count, elt)

  let store_type_paths elt paths =
    List.iter
      (fun (path, count) ->
        let word = String.concat "" path in
        store ~count word elt)
      (regroup paths)

  let store_word word elt = Suffix_tree.With_elts.add_suffixes db_names word elt
end

module Storage = Storage
