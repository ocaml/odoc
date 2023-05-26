let main files index_docstring index_name type_search empty_payload db_filename
    db_format =
  let index = files |> List.map Fpath.of_string |> List.map Result.get_ok in
  let optimize, storage =
    match db_format with
    | `ancient -> true, (module Storage_ancient : Db.Storage.S)
    | `marshal -> false, (module Storage_marshal : Db.Storage.S)
    | `js -> false, (module Storage_js : Db.Storage.S)
  in
  let add_entries li e = Odoc_search.Entry.entries_of_item e @ li in
  let index =
    index
    |> List.fold_left
         (fun li file ->
           file
           |> Odoc_odoc.Indexing.handle_file
                ~page:(Odoc_model.Fold.page ~f:add_entries li)
                ~unit:(Odoc_model.Fold.unit ~f:add_entries li)
           |> Result.get_ok |> Option.value ~default:[])
         []
  in
  Index_lib.main ~index_docstring ~index_name ~type_search ~empty_payload ~index
    ~db_filename ~optimize storage

open Cmdliner

let index_docstring =
  let doc = "Use the docstring to index the results." in
  Arg.(value & opt bool true & info ~doc [ "index-docstring" ])

let index_name =
  let doc = "Use the name to index the results." in
  Arg.(value & opt bool true & info ~doc [ "index-name" ])

let type_search =
  let doc = "Enable type based search" in
  Arg.(value & opt bool true & info ~doc [ "type-search" ])

let empty_payload =
  let doc =
    "Dont put anything in the payloads. For testing purposes, will break the \
     UI."
  in
  Arg.(value & flag & info ~doc [ "empty-payload" ])

let db_format =
  let doc = "Database format" in
  let kind = Arg.enum [ "ancient", `ancient; "marshal", `marshal; "js", `js ] in
  Arg.(
    required & opt (some kind) None & info [ "format" ] ~docv:"DB_FORMAT" ~doc)

let db_filename =
  let doc = "Output filename" in
  Arg.(required & opt (some string) None & info [ "db" ] ~docv:"DB" ~doc)

let odoc_files =
  let doc = "Path to a binary odoc index" in
  Arg.(non_empty & (pos_all file [] @@ info ~doc ~docv:"ODOC_FILE" []))

let index =
  Term.(
    const main $ odoc_files $ index_docstring $ index_name $ type_search
    $ empty_payload $ db_filename $ db_format)

let cmd =
  let doc = "Index odocl files" in
  let info = Cmd.info "index" ~doc in
  Cmd.v info index

let () = exit (Cmd.eval cmd)
