let header =
  {|Sherlodoc v0.2 -- search OCaml documentation by name and type (use CTRL-D to exit)|}

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

let print_result ~print_cost ~print_docstring ~no_rhs (elt : Db.Entry.t) =
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
  let docstring = if print_docstring then "\n" ^ elt.doc_html else "" in
  Format.printf "%s%s %s%s%a%s@." cost kind typedecl_params name pp_rhs elt.rhs docstring

let search
  ~print_cost
  ~static_sort
  ~limit
  ~db
  ~no_rhs
  ~pretty_query
  ~time
  ~print_docstring
  query
  =
  let query = Query.{ query; packages = []; limit } in
  if pretty_query then print_endline (Query.pretty query) ;
  let t0 = Unix.gettimeofday () in
  let r = Query.Blocking.search ~shards:db ~dynamic_sort:(not static_sort) query in
  let t1 = Unix.gettimeofday () in
  match r with
  | [] -> print_endline "[No results]"
  | _ :: _ as results ->
    List.iter (print_result ~print_cost ~print_docstring ~no_rhs) results ;
    flush stdout ;
    if time then Format.printf "Search in %f@." (t1 -. t0)

let rec search_loop
  ~print_cost
  ~no_rhs
  ~pretty_query
  ~static_sort
  ~limit
  ~time
  ~print_docstring
  ~db
  =
  Printf.printf "%ssearch>%s %!" "\027[0;36m" "\027[0;0m" ;
  match Stdlib.input_line stdin with
  | query ->
    search
      ~print_cost
      ~static_sort
      ~limit
      ~db
      ~no_rhs
      ~pretty_query
      ~time
      ~print_docstring
      query ;
    search_loop
      ~print_cost
      ~no_rhs
      ~pretty_query
      ~static_sort
      ~limit
      ~time
      ~print_docstring
      ~db
  | exception End_of_file -> Printf.printf "\n%!"

let search
  query
  print_cost
  no_rhs
  static_sort
  limit
  pretty_query
  time
  print_docstring
  db_format
  db_filename
  =
  let module Storage = (val Db_store.storage_module db_format) in
  let db = Storage.load db_filename in
  match query with
  | None ->
    print_endline header ;
    search_loop
      ~print_cost
      ~no_rhs
      ~pretty_query
      ~static_sort
      ~limit
      ~time
      ~print_docstring
      ~db
  | Some query ->
    search
      ~print_cost
      ~no_rhs
      ~pretty_query
      ~static_sort
      ~limit
      ~time
      ~print_docstring
      ~db
      query

open Cmdliner

let limit =
  let doc = "The maximum number of results per query" in
  Arg.(value & opt int 25 & info [ "limit"; "n" ] ~docv:"N" ~doc)

let query =
  let doc = "The query. If absent, queries will be read interactively." in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"QUERY" ~doc)

let print_cost =
  let doc = "For debugging purposes: prints the cost of each result" in
  Arg.(value & flag & info [ "print-cost" ] ~doc)

let print_time =
  let doc = "For debugging purposes: prints the search time" in
  Arg.(value & flag & info [ "print-time" ] ~doc)

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

let print_docstring =
  let doc = "Print the HTML of the docstring of the results" in
  Arg.(value & flag & info [ "print-docstring-html" ] ~doc)

let term =
  Term.(
    const search
    $ query
    $ print_cost
    $ no_rhs
    $ static_sort
    $ limit
    $ pretty_query
    $ print_time
    $ print_docstring)
