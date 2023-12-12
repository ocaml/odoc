let index_file register filename =
  match Fpath.of_string filename with
  | Error (`Msg msg) -> Format.printf "FILE ERROR %s: %s@." filename msg
  | Ok file -> (
      let open Odoc_model in
      let page p =
        let id = p.Lang.Page.name in
        Fold.page ~f:(register (id :> Paths.Identifier.t)) () p
      in
      let unit u =
        let id = u.Lang.Compilation_unit.id in
        Fold.unit ~f:(register (id :> Paths.Identifier.t)) () u
      in
      match Odoc_odoc.Indexing.handle_file ~page ~unit file with
      | Ok result -> result
      | Error (`Msg msg) ->
          Format.printf "Odoc warning or error %s: %s@." filename msg)

let storage_module = function
  | `ancient ->
      (* (module Storage_ancient : Db.Storage.S) *)
      failwith "TODO"
  | `marshal -> (module Storage_marshal : Db.Storage.S)
  | `js -> (module Storage_js : Db.Storage.S)

let main files file_list index_docstring index_name type_search db_filename
    db_format =
  let module Storage = (val storage_module db_format) in
  let db = Db.make () in
  let register id () item =
    List.iter
      (Load_doc.register_entry ~db ~index_docstring ~index_name ~type_search)
      (Odoc_search.Entry.entries_of_item id item)
  in
  let h = Storage.open_out db_filename in
  let files =
    match file_list with
    | None -> files
    | Some file_list ->
        let file_list = open_in file_list in
        files @ (file_list |> In_channel.input_all |> String.split_on_char '\n')
  in
  List.iter (index_file register) files ;
  let t = Db.export db in
  Storage.save ~db:h t ;
  Storage.close_out h

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

let db_format =
  let doc = "Database format" in
  let kind = Arg.enum [ "ancient", `ancient; "marshal", `marshal; "js", `js ] in
  Arg.(
    required & opt (some kind) None & info [ "format" ] ~docv:"DB_FORMAT" ~doc)

let db_filename =
  let doc = "Output filename" in
  Arg.(
    required
    & opt (some string) None
    & info [ "db"; "output"; "o" ] ~docv:"DB" ~doc)

let file_list =
  let doc =
    "File containing a list of .odocl files.\n\
     Useful for system where there is a limit on the number of arguments to a \
     command."
  in
  Arg.(value & opt (some file) None & info [ "file-list" ] ~doc)

let odoc_files =
  let doc = "Path to a .odocl file" in
  Arg.(non_empty & (pos_all file [] @@ info ~doc ~docv:"ODOCL_FILE" []))

let index =
  Term.(
    const main $ odoc_files $ file_list $ index_docstring $ index_name
    $ type_search $ db_filename $ db_format)

let cmd =
  let doc = "Index odocl files" in
  let info = Cmd.info "index" ~doc in
  Cmd.v info index

let () = exit (Cmd.eval cmd)
