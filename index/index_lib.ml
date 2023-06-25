module Storage = Db.Storage

let main ~index_docstring ~index_name ~type_search ~index ~db_filename storage =
  let module Storage = (val storage : Storage.S) in
  let db = Db.make () in
  let h = Storage.open_out db_filename in
  let flush () =
    let t = Db.export db in
    Storage.save ~db:h t
  in
  let t0 = Unix.gettimeofday () in
  Load_doc.run ~db ~index_docstring ~index_name ~type_search ~index ;
  let t1 = Unix.gettimeofday () in
  Format.printf "Indexing in %fms@." (1000.0 *. (t1 -. t0)) ;
  flush () ;
  Storage.close_out h
