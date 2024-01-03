let string_of_kind =
  let open Db.Entry.Kind in
  function
  | Doc -> "doc"
  | Type_decl _ -> "type"
  | Module -> "mod"
  | Exception _ -> "exn"
  | Class_type -> "class"
  | Method -> "meth"
  | Class -> "class"
  | Type_extension -> "type"
  | Extension_constructor _ -> "cons"
  | Module_type -> "sig"
  | Constructor _ -> "cons"
  | Field _ -> "field"
  | Val _ -> "val"

let print_result ~print_cost ~no_rhs (elt : Db.Entry.t) =
  let cost = if print_cost then string_of_int elt.cost ^ " " else "" in
  let typedecl_params =
    (match elt.kind with
     | Type_decl args -> args
     | _ -> None)
    |> Option.map (fun str -> str ^ " ")
    |> Option.value ~default:""
  in
  let kind = elt.kind |> string_of_kind |> Unescape.string in
  let name = Unescape.string elt.name in
  let pp_rhs h = function
    | None -> ()
    | Some _ when no_rhs -> ()
    | Some rhs -> Format.fprintf h "%s" (Unescape.string rhs)
  in
  Format.printf "%s%s %s%s%a@." cost kind typedecl_params name pp_rhs elt.rhs

let search ~print_cost ~static_sort ~limit ~db ~no_rhs ~pretty_query query =
  let query = Query.{ query; packages = []; limit } in
  if pretty_query then print_endline (Query.pretty query) ;
  match Query.(search ~shards:db ~dynamic_sort:(not static_sort) query) with
  | [] -> print_endline "[No results]"
  | _ :: _ as results ->
    List.iter (print_result ~print_cost ~no_rhs) results ;
    flush stdout

let rec search_loop ~print_cost ~no_rhs ~pretty_query ~static_sort ~limit ~db =
  match In_channel.input_line stdin with
  | Some query ->
    search ~print_cost ~static_sort ~limit ~db ~no_rhs ~pretty_query query ;
    search_loop ~print_cost ~no_rhs ~pretty_query ~static_sort ~limit ~db
  | None -> print_endline "[Search session ended]"

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

let search query print_cost no_rhs static_sort limit pretty_query db_format db_filename =
  let module Storage = (val Db_store.storage_module db_format) in
  let db = Storage.load db_filename in
  match query with
  | None -> search_loop ~print_cost ~no_rhs ~pretty_query ~static_sort ~limit ~db
  | Some query -> search ~print_cost ~no_rhs ~pretty_query ~static_sort ~limit ~db query

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
  Arg.(required & opt (some string) None & info [ "db" ] ~docv:"DB" ~env)

let db_path =
  let env =
    let doc = "The database to query" in
    Cmd.Env.info "SHERLODOC_DB" ~doc
  in
  Arg.(required & opt (some file) None & info [ "db" ] ~docv:"DB" ~env)

let limit =
  let doc = "The maximum number of results per query" in
  Arg.(value & opt int 50 & info [ "limit"; "n" ] ~docv:"N" ~doc)

let query =
  let doc = "The query. If absent, sherlodoc will read queries in the standard input." in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"QUERY" ~doc)

let print_cost =
  let doc = "For debugging purposes : prints the cost of each result" in
  Arg.(value & flag & info [ "print-cost" ] ~doc)

let static_sort =
  let doc =
    "Sort the results without looking at the query.\n\
     Enabling it allows to look at the static costs of elements.\n\
     Mainly for testing purposes."
  in
  Arg.(value & flag & info [ "static-sort" ] ~doc)

let no_rhs =
  let doc = "Do not print the right-hand side of results." in
  Arg.(value & flag & info [ "no-rhs"; "no-right-hand-side" ] ~doc)

let pretty_query =
  let doc = "Prints the query itself as it was parsed" in
  Arg.(value & flag & info [ "pretty-query" ] ~doc)

let search_term =
  Term.(const search $ query $ print_cost $ no_rhs $ static_sort $ limit $ pretty_query)

let with_db fn db_path =
  let apply fn db_format db_filename =
    let db_format = guess_db_format db_format db_filename in
    fn db_format db_filename
  in
  Term.(const apply $ fn $ db_format $ db_path)

let cmd_search =
  let info = Cmd.info "search" ~doc:"Command-line search" in
  Cmd.v info (with_db search_term db_path)

let cmd_index =
  let doc = "Index odocl files to create a Sherlodoc database" in
  let info = Cmd.info "index" ~doc in
  Cmd.v info (with_db Index.term db_filename)

let cmd_serve =
  let doc = "Webserver interface" in
  let info = Cmd.info "serve" ~doc in
  Cmd.v info (with_db Serve.term db_path)

let cmd =
  let doc = "Sherlodoc" in
  let info = Cmd.info "sherlodoc" ~doc in
  Cmd.group info [ cmd_search; cmd_index; cmd_serve ]

let () = exit (Cmd.eval cmd)
