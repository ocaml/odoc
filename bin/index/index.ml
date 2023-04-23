let main odoc_directory db_filename db_format =
  let optimize, storage =
    match db_format with
    | `ancient -> true, (module Storage_ancient : Db.Storage.S)
    | `marshal -> false, (module Storage_marshal : Db.Storage.S)
    | `js -> false, (module Storage_js : Db.Storage.S)
  in
  Index_lib.main ~odoc_directory ~db_filename ~optimize storage

open Cmdliner

let db_format =
  let doc = "Databse format" in
  let kind = Arg.enum [ "ancient", `ancient; "marshal", `marshal; "js", `js ] in
  Arg.(
    required & opt (some kind) None & info [ "format" ] ~docv:"DB_FORMAT" ~doc)

let db_filename =
  let doc = "Database filename" in
  Arg.(required & opt (some string) None & info [ "db" ] ~docv:"DB" ~doc)

let odoc_path =
  let doc = "Path to a directory containing odocl files" in
  Arg.(required & opt (some dir) None & info [ "odoc" ] ~docv:"ODOC_FILES" ~doc)

let index = Term.(const main $ odoc_path $ db_filename $ db_format)

let cmd =
  let doc = "Index odocl files" in
  let info = Cmd.info "index" ~doc in
  Cmd.v info index

let () = exit (Cmd.eval cmd)
