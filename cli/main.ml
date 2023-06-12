let print_result
    Db.Elt.{ name; rhs; url = _; kind; score = _; doc_html = _; pkg = _ } =
  let kind = kind |> Db.Elt.Kind.to_string |> Unescape.string in
  let name = Unescape.string name in
  let rhs = rhs |> Option.value ~default:"" |> Unescape.string in
  Printf.printf "%s %s%s\n" kind name rhs

let search db query =
  match Query.(api ~shards:db { query; packages = []; limit = 50 }) with
  | _, [] -> print_endline "[No results]"
  | _, (_ :: _ as results) ->
      List.iter print_result results ;
      flush stdout

let rec search_loop db =
  match In_channel.input_line stdin with
  | Some query ->
      search db query ;
      search_loop db
  | None -> print_endline "[Search session ended]"

let main db query =
  let db = Storage_marshal.load db in
  match query with
  | None -> search_loop db
  | Some query -> search db query

open Cmdliner

let db_filename =
  let doc = "The database to query" in
  Arg.(required & opt (some file) None & info [ "db" ] ~docv:"DB" ~doc)

let query =
  let doc = "The query" in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"QUERY" ~doc)

let main = Term.(const main $ db_filename $ query)

let cmd =
  let doc = "CLI interface to query sherlodoc" in
  let info = Cmd.info "sherlodoc" ~doc in
  Cmd.v info main

let () = exit (Cmd.eval cmd)
