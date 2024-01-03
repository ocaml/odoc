module Entry = Entry
module Suffix_tree = Suffix_tree
module Occ = Occ
module Storage = Storage
module Type_polarity = Type_polarity
module Typexpr = Typexpr

type t = Storage.db =
  { db_names : Suffix_tree.With_elts.reader
  ; db_pos_types : Suffix_tree.With_occ.reader
  ; db_neg_types : Suffix_tree.With_occ.reader
  }

type writer =
  { writer_names : Suffix_tree.With_elts.writer
  ; writer_pos_types : Suffix_tree.With_occ.writer
  ; writer_neg_types : Suffix_tree.With_occ.writer
  }

let make () =
  let buffer_names = Suffix_tree.Buf.make () in
  let buffer_types = Suffix_tree.Buf.make () in
  { writer_names = Suffix_tree.With_elts.make buffer_names
  ; writer_pos_types = Suffix_tree.With_occ.make buffer_types
  ; writer_neg_types = Suffix_tree.With_occ.make buffer_types
  }

let export db =
  { Storage.db_names = Suffix_tree.With_elts.export db.writer_names
  ; db_pos_types = Suffix_tree.With_occ.export db.writer_pos_types
  ; db_neg_types = Suffix_tree.With_occ.export db.writer_neg_types
  }

let store db name elt ~count ~polarity =
  let st =
    match polarity with
    | Type_polarity.Sign.Pos -> db.writer_pos_types
    | Type_polarity.Sign.Neg -> db.writer_neg_types
  in
  Suffix_tree.With_occ.add_suffixes st name (count, elt)

let store_type_polarities db elt polarities =
  Seq.iter (fun (path, count, polarity) -> store db ~count ~polarity path elt) polarities

let store_word db word elt = Suffix_tree.With_elts.add_suffixes db.writer_names word elt
