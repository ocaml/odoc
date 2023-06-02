module Storage = Db.Storage

let main ~index_docstring ~index_name ~type_search ~empty_payload ~index
    ~db_filename storage =
    print_endline "Index_lib.main" ;
  let module Storage = (val storage : Storage.S) in
  let module Load_doc = Load_doc.Make (Storage) in
  let module Db = Load_doc.Db in
  let h = Storage.open_out db_filename in
  let flush () =
    Load_doc.clear () ;
    Db.export h
  in
  Load_doc.run ~index_docstring ~index_name ~type_search ~empty_payload ~index ;
  print_endline "doc loaded" ;
  flush () ;
  Storage.close_out h
