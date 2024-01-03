module Entry = Entry
module Suffix_tree = Suffix_tree
module Storage = Storage
module Type_polarity = Type_polarity
module Typexpr = Typexpr
module Occurences = Storage.Occurences

type t = Storage.db =
  { db_names : Suffix_tree.With_elts.reader
  ; db_pos_types : Suffix_tree.With_elts.reader Occurences.t
  ; db_neg_types : Suffix_tree.With_elts.reader Occurences.t
  }

type writer =
  { writer_names : Suffix_tree.With_elts.writer
  ; buffer_types : Suffix_tree.Buf.t
  ; mutable writer_pos_types : Suffix_tree.With_elts.writer Occurences.t
  ; mutable writer_neg_types : Suffix_tree.With_elts.writer Occurences.t
  }

let make () =
  let buffer_names = Suffix_tree.Buf.make () in
  let buffer_types = Suffix_tree.Buf.make () in
  { writer_names = Suffix_tree.With_elts.make buffer_names
  ; buffer_types
  ; writer_pos_types = Occurences.empty
  ; writer_neg_types = Occurences.empty
  }

let export db =
  { Storage.db_names = Suffix_tree.With_elts.export db.writer_names
  ; db_pos_types = Occurences.map Suffix_tree.With_elts.export db.writer_pos_types
  ; db_neg_types = Occurences.map Suffix_tree.With_elts.export db.writer_neg_types
  }

let store db name elt ~count ~polarity =
  let st =
    match polarity with
    | Type_polarity.Sign.Pos -> begin
      try Occurences.find count db.writer_pos_types with
      | Not_found ->
        let st = Suffix_tree.With_elts.make db.buffer_types in
        db.writer_pos_types <- Occurences.add count st db.writer_pos_types ;
        st
    end
    | Type_polarity.Sign.Neg -> begin
      try Occurences.find count db.writer_neg_types with
      | Not_found ->
        let st = Suffix_tree.With_elts.make db.buffer_types in
        db.writer_neg_types <- Occurences.add count st db.writer_neg_types ;
        st
    end
  in
  Suffix_tree.With_elts.add_suffixes st name elt

let store_type_polarities db elt polarities =
  Seq.iter (fun (path, count, polarity) -> store db ~count ~polarity path elt) polarities

let store_word db word elt = Suffix_tree.With_elts.add_suffixes db.writer_names word elt
