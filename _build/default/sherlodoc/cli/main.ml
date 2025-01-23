let guess_db_format db_format db_filename =
  match db_format with
  | Some db_format -> db_format
  | None -> begin
    let ext = Filename.extension db_filename in
    let ext_len = String.length ext in
    let ext = if ext_len = 0 then ext else String.sub ext 1 (ext_len - 1) in
    try List.assoc ext Db_store.available_backends with
    | Not_found ->
      Format.fprintf
        Format.err_formatter
        "Unknown db format extension %S (expected: %s)@."
        ext
        (String.concat ", " @@ List.map fst Db_store.available_backends) ;
      exit 1
  end

open Cmdliner

let db_format =
  let env =
    let doc = "Database format" in
    Cmd.Env.info "SHERLODOC_FORMAT" ~doc
  in
  let kind = Arg.enum Db_store.available_backends in
  Arg.(value & opt (some kind) None & info [ "format" ] ~docv:"DB_FORMAT" ~env)

let db_filename =
  let env =
    let doc = "The database to query" in
    Cmd.Env.info "SHERLODOC_DB" ~doc
  in
  Arg.(required & opt (some string) None & info [ "db"; "o" ] ~docv:"DB" ~env)

let db_path =
  let env =
    let doc = "The database to query" in
    Cmd.Env.info "SHERLODOC_DB" ~doc
  in
  Arg.(required & opt (some file) None & info [ "db" ] ~docv:"DB" ~env)

let with_db fn db_path =
  let apply fn db_format db_filename =
    let db_format = guess_db_format db_format db_filename in
    fn db_format db_filename
  in
  Term.(const apply $ fn $ db_format $ db_path)

let cmd_search =
  let info = Cmd.info "search" ~doc:"Command-line search" in
  Cmd.v info (with_db Search.term db_path)

let cmd_index =
  let doc = "Index odocl files to create a Sherlodoc database" in
  let info = Cmd.info "index" ~doc in
  Cmd.v info (with_db Index.term db_filename)

let cmd_serve =
  let doc = "Webserver interface" in
  let info = Cmd.info "serve" ~doc in
  Cmd.v info (with_db Serve.term db_path)

let cmd_jsoo =
  let doc = "For dune/odoc integration, sherlodoc compiled as javascript" in
  let info = Cmd.info "js" ~doc in
  let target =
    let doc = "Name of the file to create" in
    Arg.(value & pos 0 string "" & info [] ~docv:"QUERY" ~doc)
  in
  let emit_js_dep filename =
    let close, h = if filename = "" then false, stdout else true, open_out filename in
    output_string h [%blob "../jsoo/sherlodoc.js"] ;
    if close then close_out h
  in
  Cmd.v info Term.(const emit_js_dep $ target)

let cmd =
  let doc = "Sherlodoc" in
  let version = "0.2" in
  let info = Cmd.info "sherlodoc" ~version ~doc in
  Cmd.group info [ cmd_search; cmd_index; cmd_serve; cmd_jsoo ]

let () = exit (Cmd.eval cmd)
