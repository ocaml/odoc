module Entry = Entry
module Suffix_tree = Suffix_tree
module Occ = Occ
module Storage = Storage
module Type_polarity = Type_polarity
module Typexpr = Typexpr
include Db_typedef

type writer =
  { writer_names : Suffix_tree.With_elts.writer
  ; writer_types : Suffix_tree.With_occ.writer
  }

let make () =
  { writer_names = Suffix_tree.With_elts.make ()
  ; writer_types = Suffix_tree.With_occ.make ()
  }

let export db =
  let db =
    { db_names = Suffix_tree.With_elts.export db.writer_names
    ; db_types = Suffix_tree.With_occ.export db.writer_types
    }
  in
  db

let store db name elt ~count =
  Suffix_tree.With_occ.add_suffixes db.writer_types name (count, elt)

let store_type_polarities db elt polarities =
  List.iter (fun (word, count) ->
    store db ~count word elt) polarities

let store_word db word elt =
  Suffix_tree.With_elts.add_suffixes db.writer_names word elt
