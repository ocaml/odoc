(** This executable allows to search in a sherlodoc database on the commandline.
    It is mainly used for testing, but should work as is as a commandline tool. *)

let pp_or cond pp_true pp_false ppf = if cond then pp_true ppf else pp_false ppf

let string_of_kind =
  let open Db.Entry.Kind in
  function
  | Doc -> "doc"
  | TypeDecl _ -> "type"
  | Module -> "mod"
  | Exception _ -> "exn"
  | Class_type -> "class"
  | Method -> "meth"
  | Class -> "class"
  | TypeExtension -> "type"
  | ExtensionConstructor _ -> "cons"
  | ModuleType -> "sig"
  | Constructor _ -> "cons"
  | Field _ -> "field"
  | Val _ -> "val"

let print_result ~print_cost ~no_rhs
    Db.Entry.
      { name
      ; rhs
      ; url = _
      ; kind
      ; cost
      ; doc_html = _
      ; pkg = _
      ; is_from_module_type = _
      } =
  let cost = if print_cost then string_of_int cost ^ " " else "" in
  let typedecl_params =
    (match kind with
    | Db.Entry.Kind.TypeDecl args -> args
    | _ -> None)
    |> Option.map (fun str -> str ^ " ")
    |> Option.value ~default:""
  in
  let kind = kind |> string_of_kind |> Unescape.string in
  let name = Unescape.string name in
  let pp_rhs h = function
    | None -> ()
    | Some _ when no_rhs -> ()
    | Some rhs -> Format.fprintf h "%s" (Unescape.string rhs)
  in
  Format.printf "%s%s %s%s%a\n" cost kind typedecl_params name pp_rhs rhs

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

let main db query print_cost no_rhs static_sort limit pretty_query =
  match db with
  | None ->
      output_string stderr
        "No database provided. Provide one by exporting the SHERLODOC_DB \
         variable, or using the --db option\n" ;
      exit 1
  | Some db -> (
      let db = Storage_marshal.load db in
      match query with
      | None ->
          search_loop ~print_cost ~no_rhs ~pretty_query ~static_sort ~limit ~db
      | Some query ->
          search ~print_cost ~no_rhs ~pretty_query ~static_sort ~limit ~db query
      )

open Cmdliner

let db_filename =
  let env =
    let doc = "The database to query" in
    Cmd.Env.info "SHERLODOC_DB" ~doc
  in
  Arg.(value & opt (some file) None & info [ "db" ] ~docv:"DB" ~env)

let limit =
  let doc = "The maximum number of results per query" in
  Arg.(value & opt int 50 & info [ "limit"; "n" ] ~docv:"N" ~doc)

let query =
  let doc =
    "The query. If absent, sherlodoc will read queries in the standard input."
  in
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

let main =
  Term.(
    const main $ db_filename $ query $ print_cost $ no_rhs $ static_sort $ limit
    $ pretty_query)

let cmd =
  let doc = "CLI interface to query sherlodoc" in
  let info = Cmd.info "sherlodoc" ~doc in
  Cmd.v info main

let () = exit (Cmd.eval cmd)
