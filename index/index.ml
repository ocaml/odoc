let index_file register filename =
  match Fpath.of_string filename with
  | Error (`Msg msg) -> Format.printf "FILE ERROR %s: %s@." filename msg
  | Ok file -> (
      match
        Odoc_odoc.Indexing.handle_file
          ~page:(Odoc_model.Fold.page ~f:register ())
          ~unit:(Odoc_model.Fold.unit ~f:register ())
          file
      with
      | Ok (Some result) -> result
      | Ok None -> ()
      | Error (`Msg msg) -> Format.printf "ODOC ERROR %s: %s@." filename msg)

let storage_module = function
  | `ancient -> (module Storage_ancient : Db.Storage.S)
  | `marshal -> (module Storage_marshal : Db.Storage.S)
  | `js -> (module Storage_js : Db.Storage.S)

let main files index_docstring index_name type_search db_filename db_format =
  let module Storage = (val storage_module db_format) in
  let db = Db.make () in
  let register () item =
    List.iter
      (Load_doc.register_entry ~db ~index_docstring ~index_name ~type_search)
      (Odoc_search.Entry.entries_of_item item)
  in
  let h = Storage.open_out db_filename in
  let t0 = Unix.gettimeofday () in
  List.iter (index_file register) files ;
  let t1 = Unix.gettimeofday () in
  Format.printf "Indexing in %fms@." (1000.0 *. (t1 -. t0)) ;
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
  Arg.(required & opt (some string) None & info [ "db" ] ~docv:"DB" ~doc)

let odoc_files =
  let doc = "Path to a binary odoc index" in
  Arg.(non_empty & (pos_all file [] @@ info ~doc ~docv:"ODOC_FILE" []))

let index =
  Term.(
    const main $ odoc_files $ index_docstring $ index_name $ type_search
    $ db_filename $ db_format)

let cmd =
  let doc = "Index odocl files" in
  let info = Cmd.info "index" ~doc in
  Cmd.v info index

let () = exit (Cmd.eval cmd)
