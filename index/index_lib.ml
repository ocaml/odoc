module Storage = Db.Storage

let main ~index_docstring ~index_name ~type_search ~index ~db_filename storage =
  print_endline "Index_lib.main" ;
  let module Storage = (val storage : Storage.S) in
  let module Load_doc = Load_doc.Make (Storage) in
  let module Db = Load_doc.Db in
  let h = Storage.open_out db_filename in
  let flush () =
    Load_doc.clear () ;
    Db.export h
  in
  let t0 = Unix.gettimeofday () in
  Load_doc.run ~index_docstring ~index_name ~type_search ~index ;
  let t1 = Unix.gettimeofday () in
  Format.printf "Indexing in %fs@." (t1 -. t0) ;
  flush () ;
  Storage.close_out h
