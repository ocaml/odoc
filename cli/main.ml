(** This executable allows to search in a sherlodoc database on the commandline.
    It is mainly used for testing, but should work as is as a commandline tool. *)

let pp_or cond pp_true pp_false ppf = if cond then pp_true ppf else pp_false ppf

let string_of_kind =
  let open Db.Elt.Kind in
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

let print_result ~print_cost
    Db.Elt.{ name; rhs; url = _; kind; score; doc_html = _; pkg = _ } =
  let score = if print_cost then string_of_int score ^ " " else "" in
  let kind = kind |> string_of_kind |> Unescape.string in
  let name = Unescape.string name in
  let pp_rhs h = function
    | None -> ()
    | Some rhs -> Format.fprintf h "%s" (Unescape.string rhs)
  in
  Format.printf "%s%s %s%a\n" score kind name pp_rhs rhs

let search ~print_cost ~dynamic_sort ~limit ~db query =
  match
    Query.(api ~shards:db ~dynamic_sort { query; packages = []; limit })
  with
  | _, [] -> print_endline "[No results]"
  | _, (_ :: _ as results) ->
      List.iter (print_result ~print_cost) results ;
      flush stdout

let rec search_loop ~print_cost ~dynamic_sort ~limit ~db =
  match In_channel.input_line stdin with
  | Some query ->
      search ~print_cost ~dynamic_sort ~limit ~db query ;
      search_loop ~print_cost ~dynamic_sort ~limit ~db
  | None -> print_endline "[Search session ended]"

let main db query print_cost dynamic_sort limit =
  match db with
  | None ->
      output_string stderr
        "No database provided. Provide one by exporting the SHERLODOC_DB \
         variable, or using the --db option\n" ;
      exit 1
  | Some db -> (
      let db = Storage_marshal.load db in
      match query with
      | None -> search_loop ~print_cost ~dynamic_sort ~limit ~db
      | Some query -> search ~print_cost ~dynamic_sort ~limit ~db query)

open Cmdliner

let db_filename =
  let env =
    let doc = "The database to query" in
    Cmd.Env.info "SHERLODOC_DB" ~doc
  in
  Arg.(value & opt (some file) None & info [ "db" ] ~docv:"DB" ~env)

let limit =
  let doc = "The maximum number of results" in
  Arg.(value & opt int 50 & info [ "limit"; "n" ] ~docv:"N" ~doc)

let query =
  let doc = "The query" in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"QUERY" ~doc)

let print_cost =
  let doc = "Prints cost of each result" in
  Arg.(value & flag & info [ "print-cost" ] ~doc)

let dynamic_sort =
  let doc =
    "Sort the results by looking at the query.\n\
     Disabling it allows to look at the static costs of elements."
  in
  Arg.(value & flag & info [ "dynamic-sort" ] ~doc)

let main = Term.(const main $ db_filename $ query $ print_cost $ dynamic_sort $ limit)

let cmd =
  let doc = "CLI interface to query sherlodoc" in
  let info = Cmd.info "sherlodoc" ~doc in
  Cmd.v info main

let () = exit (Cmd.eval cmd)
