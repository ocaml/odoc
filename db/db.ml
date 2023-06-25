module Elt = Elt
module Types = Types
module Suffix_tree = Suffix_tree
module Occ = Occ
module Storage = Storage
include Types

type writer =
  { writer_names : Suffix_tree.With_elts.writer
  ; writer_types : Suffix_tree.With_occ.writer
  }

let make () =
  { writer_names = Suffix_tree.With_elts.make ()
  ; writer_types = Suffix_tree.With_occ.make ()
  }

let export db =
  let t0 = Unix.gettimeofday () in
  let db =
    { db_names = Suffix_tree.With_elts.export db.writer_names
    ; db_types = Suffix_tree.With_occ.export db.writer_types
    }
  in
  let t1 = Unix.gettimeofday () in
  Format.printf "Export in %fms@." (1000.0 *. (t1 -. t0)) ;
  db

let store db name elt ~count =
  Suffix_tree.With_occ.add_suffixes db.writer_types name (count, elt)

let store_type_paths db elt paths =
  List.iter
    (fun (path, count) ->
      let word = String.concat "" path in
      store db ~count word elt)
    (regroup paths)

let store_word db word elt =
  Suffix_tree.With_elts.add_suffixes db.writer_names word elt
