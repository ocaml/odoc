let index_file register filename =
  match Fpath.of_string filename with
  | Error (`Msg msg) -> Format.printf "FILE ERROR %s: %s@." filename msg
  | Ok file ->
    let open Odoc_model in
    let page p =
      let id = p.Lang.Page.name in
      Fold.page ~f:(register (id :> Paths.Identifier.t)) () p
    in
    let unit u =
      let id = u.Lang.Compilation_unit.id in
      Fold.unit ~f:(register (id :> Paths.Identifier.t)) () u
    in
    (match Odoc_odoc.Indexing.handle_file ~page ~unit file with
     | Ok result -> result
     | Error (`Msg msg) -> Format.printf "Odoc warning or error %s: %s@." filename msg)

let main files file_list index_docstring index_name type_search db_format db_filename =
  let module Storage = (val Db_store.storage_module db_format) in
  let db = Db_writer.make () in
  let no_pkg = Db.Entry.Package.v ~name:"" ~version:"" in
  let register ~pkg id () item =
    List.iter
      (Load_doc.register_entry ~db ~index_docstring ~index_name ~type_search ~pkg)
      (Odoc_search.Entry.entries_of_item id item)
  in
  let files =
    match file_list with
    | None -> files
    | Some file_list ->
      let h = open_in file_list in
      let rec read_all acc =
        match Stdlib.input_line h with
        | exception End_of_file -> List.rev acc
        | line -> read_all (line :: acc)
      in
      let other_files = read_all [] in
      close_in h ;
      files @ other_files
  in
  let h = Storage.open_out db_filename in
  let flush () =
    let t = Db_writer.export ~summarize:(db_format = `ancient) db in
    Storage.save ~db:h t
  in
  List.iter
    (fun odoc ->
      let pkg, odoc =
        match String.split_on_char '\t' odoc with
        | [ filename ] -> no_pkg, filename
        | [ name; filename ] -> Db.Entry.Package.v ~name ~version:"", filename
        | [ name; version; filename ] -> Db.Entry.Package.v ~name ~version, filename
        | _ -> failwith ("invalid line: " ^ odoc)
      in
      index_file (register ~pkg) odoc ;
      if db_format = `ancient && Db_writer.load db > 1_000_000 then flush ())
    files ;
  flush () ;
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

let file_list =
  let doc =
    "File containing a list of .odocl files.\n\
     Useful for system where there is a limit on the number of arguments to a command."
  in
  Arg.(value & opt (some file) None & info [ "file-list" ] ~doc)

let odoc_files =
  let doc = "Path to a .odocl file" in
  Arg.(value & (pos_all file [] @@ info ~doc ~docv:"ODOCL_FILE" []))

let term =
  Term.(const main $ odoc_files $ file_list $ index_docstring $ index_name $ type_search)
