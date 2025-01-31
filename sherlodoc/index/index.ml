let handle_file register file =
  let ( >>= ) = Result.bind in
  let open Odoc_odoc in
  let open Odoc_index in
  match Fpath.get_ext file with
  | ".odoc-index" -> Odoc_file.load_index file >>= fun index -> Ok (register index)
  | ".odocl" ->
      Odoc_file.load file
      >>= fun unit' ->
      (match unit' with
       | { Odoc_file.content = Unit_content unit'; _ } when unit'.hidden ->
           Error (`Msg "Hidden units are ignored when generating an index")
       | { content = Unit_content u; _ } -> Ok (register [ Skeleton.from_unit u ])
       | { content = Page_content p; _ } -> Ok (register [ Skeleton.from_page p ])
       | _ ->
           Error
             (`Msg "Only pages and unit are allowed as input when generating an index"))
  | _ ->
      Error
        (`Msg "Only .odocl and .odoc-index are allowed as input when generating an index")

let index_file register filename =
  match Fpath.of_string filename with
  | Error (`Msg msg) -> Format.printf "FILE ERROR %s: %s@." filename msg
  | Ok file ->
      (match handle_file register file with
       | Ok result -> result
       | Error (`Msg msg) ->
           Format.printf "Odoc warning or error %a: %s@." Fpath.pp file msg)

let main
      files
      favourite_files
      file_list
      index_docstring
      index_name
      type_search
      favoured_prefixes
      db_format
      db_filename
  =
  let module Storage = (val Db_store.storage_module db_format) in
  let db = Db_writer.make () in
  let no_pkg = Db.Entry.Package.v ~name:"" ~version:"" in
  let register ~pkg ~favourite =
    List.iter
    @@ Odoc_utils.Tree.iter
         ~f:
           (Load_doc.register_entry
              ~db
              ~index_docstring
              ~index_name
              ~type_search
              ~favourite
              ~favoured_prefixes
              ~pkg)
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
  let loop ~favourite odoc =
    let pkg, odoc =
      match String.split_on_char '\t' odoc with
      | [ filename ] -> no_pkg, filename
      | [ name; filename ] -> Db.Entry.Package.v ~name ~version:"", filename
      | [ name; version; filename ] -> Db.Entry.Package.v ~name ~version, filename
      | _ -> failwith ("invalid line: " ^ odoc)
    in
    index_file (register ~pkg ~favourite) odoc ;
    if db_format = `ancient && Db_writer.load db > 1_000_000 then flush ()
  in
  List.iter (loop ~favourite:false) files ;
  List.iter (loop ~favourite:true) favourite_files ;
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
  let doc = "Enable type based search." in
  Arg.(value & opt bool true & info ~doc [ "type-search" ])

let favoured_prefixes =
  let doc =
    "The list of favoured prefixes. Entries that start with a favoured prefix are ranked \
     higher."
  in
  Arg.(value & opt (list string) [ "Stdlib." ] & info ~doc [ "favoured-prefixes" ])

let file_list =
  let doc =
    "File containing a list of .odocl files.\n\
     Useful for system where there is a limit on the number of arguments to a command."
  in
  Arg.(value & opt (some file) None & info [ "file-list" ] ~doc)

let odoc_favourite_file =
  let doc = "Path to a .odocl file whose entries will be ranked higher." in
  Arg.(value & opt_all file [] & info [ "favoured" ] ~doc)

let odoc_files =
  let doc = "Path to a .odocl file or a .odoc-index file" in
  Arg.(value & (pos_all file [] @@ info ~doc ~docv:"ODOCL_FILE" []))

let term =
  Term.(
    const main
    $ odoc_files
    $ odoc_favourite_file
    $ file_list
    $ index_docstring
    $ index_name
    $ type_search
    $ favoured_prefixes)
