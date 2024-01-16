open Db

type s =
  { mutable load : int
  ; writer_names : Suffix_tree.t
  ; buffer_types : Suffix_tree.Buf.t
  ; mutable writer_pos_types : Suffix_tree.t Occurences.t
  ; mutable writer_neg_types : Suffix_tree.t Occurences.t
  ; type_cache : Type_cache.t
  }

type t = s ref

let load t = !t.load

let make_empty () =
  let buffer_names = Suffix_tree.Buf.make () in
  let buffer_types = Suffix_tree.Buf.make () in
  { load = 0
  ; writer_names = Suffix_tree.make buffer_names
  ; buffer_types
  ; writer_pos_types = Occurences.empty
  ; writer_neg_types = Occurences.empty
  ; type_cache = Type_cache.make ()
  }

let make () = ref (make_empty ())

let export ~summarize db =
  let shard =
    let db = !db in
    let db_names = Suffix_tree.export ~summarize db.writer_names in
    let db_pos_types =
      Occurences.map (Suffix_tree.export ~summarize) db.writer_pos_types
    in
    let db_neg_types =
      Occurences.map (Suffix_tree.export ~summarize) db.writer_neg_types
    in
    { Storage.db_names; db_pos_types; db_neg_types }
  in
  db := make_empty () ;
  shard

let store db name elt ~count ~polarity =
  db.load <- db.load + 1 ;
  let st =
    match polarity with
    | Type_polarity.Sign.Pos -> begin
      try Occurences.find count db.writer_pos_types with
      | Not_found ->
        let st = Suffix_tree.make db.buffer_types in
        db.writer_pos_types <- Occurences.add count st db.writer_pos_types ;
        st
    end
    | Type_polarity.Sign.Neg -> begin
      try Occurences.find count db.writer_neg_types with
      | Not_found ->
        let st = Suffix_tree.make db.buffer_types in
        db.writer_neg_types <- Occurences.add count st db.writer_neg_types ;
        st
    end
  in
  Suffix_tree.add_suffixes st name elt

let store_type_polarities db elt polarities =
  let db = !db in
  Seq.iter (fun (path, count, polarity) -> store db ~count ~polarity path elt) polarities

let store_word db word elt =
  let db = !db in
  db.load <- db.load + 1 ;
  Suffix_tree.add_suffixes db.writer_names word elt

let type_of_odoc ~db ty =
  let db = !db in
  Type_cache.of_odoc ~cache:db.type_cache ty
