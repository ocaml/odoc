module Storage = Db.Storage

let of_filename f =
  let module_name =
    String.capitalize_ascii Filename.(chop_extension (basename f))
  in
  module_name, f

let filenames odoc_directory = List.map of_filename (Files.list odoc_directory)

let main ~odoc_directory ~db_filename ~optimize storage =
  let module Storage = (val storage : Storage.S) in
  let module Load_doc = Load_doc.Make (Storage) in
  let module Db = Load_doc.Db in
  let files = filenames odoc_directory in
  let total = List.length files in
  let h = Storage.open_out db_filename in
  let flush () =
    if optimize then Db.optimize () ;
    Load_doc.clear () ;
    Db.export h
  in
  List.iteri
    (fun i file ->
      if !Db.load_counter > 10_000_000
      then begin
        Printf.printf
          "---------------- SHARD %i / %i -----------------------\n%!" i total ;
        flush ()
      end ;
      Load_doc.run ~odoc_directory file)
    files ;
  flush () ;
  Storage.close_out h
