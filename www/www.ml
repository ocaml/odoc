module Storage = Db.Storage
module H = Tyxml.Html

let api ~shards params =
  let results = Query.search ~shards params in
  let pretty = Query.pretty params in
  Lwt.return (Ui.render ~pretty results)

let api ~shards params =
  if String.trim params.Query.query = ""
  then Lwt.return Ui.explain
  else api ~shards params

open Lwt.Syntax

let get_query params = Option.value ~default:"" (Dream.query params "q")

let get_packages params =
  match Dream.query params "packages" with
  | None -> []
  | Some str -> String.split_on_char ',' str

let get_limit params =
  let default = 100 in
  match Dream.query params "limit" with
  | None -> default
  | Some str ->
    (try max 1 (min default (int_of_string str)) with
     | _ -> default)

let get_params params =
  { Query.query = get_query params
  ; packages = get_packages params
  ; limit = get_limit params
  }

let root fn params =
  let* result = fn params in
  Dream.html result

let string_of_tyxml html = Format.asprintf "%a" (Tyxml.Html.pp ()) html
let string_of_tyxml' html = Format.asprintf "%a" (Tyxml.Html.pp_elt ()) html

let root fn params =
  let params = get_params params in
  try root fn params with
  | err ->
    Format.printf "ERROR: %S@." (Printexc.to_string err) ;
    Dream.html (string_of_tyxml @@ Ui.template params.query Ui.explain)

let root fn params =
  try root fn params with
  | _ -> Dream.html (string_of_tyxml @@ Ui.template "" Ui.explain)

let cache_header : int option -> Dream.middleware =
  fun max_age f req ->
  let+ response = f req in
  begin
    match max_age with
    | None -> ()
    | Some max_age ->
      Dream.add_header
        response
        "Cache-Control"
        ("public, max-age=" ^ string_of_int max_age)
  end ;
  response

let cors_header f req =
  let+ response = f req in
  Dream.add_header response "Access-Control-Allow-Origin" "*" ;
  response

let cors_options =
  Dream.options "**" (fun _ ->
    let+ response = Dream.empty `No_Content in
    Dream.add_header response "Access-Control-Allow-Methods" "GET, OPTIONS" ;
    Dream.add_header response "Access-Control-Allow-Headers" "*" ;
    response)

let main db_format db_filename cache_max_age =
  let module Storage = (val Db_store.storage_module db_format) in
  let shards = Storage.load db_filename in
  Dream.run ~interface:"127.0.0.1" ~port:1234
  @@ Dream.logger
  @@ cache_header cache_max_age
  @@ cors_header
  @@ Dream.router
       [ Dream.get
           "/"
           (root (fun params ->
              let+ result = api ~shards params in
              string_of_tyxml @@ Ui.template params.query result))
       ; Dream.get
           "/api"
           (root (fun params ->
              let+ result = api ~shards params in
              string_of_tyxml' result))
       ; Dream.get "/s.css" (Dream.from_filesystem "static" "style.css")
       ; Dream.get "/robots.txt" (Dream.from_filesystem "static" "robots.txt")
       ; Dream.get "/favicon.ico" (Dream.from_filesystem "static" "favicon.ico")
       ; Dream.get "/bg.jpg" (Dream.from_filesystem "static" "bg.jpg")
       ; cors_options
       ]

open Cmdliner

let db_format =
  let doc = "Database format" in
  let kind = Arg.enum Db_store.available_backends in
  Arg.(required & opt (some kind) None & info [ "format" ] ~docv:"DB_FORMAT" ~doc)

let db_path =
  let doc = "Database filename" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"DB" ~doc)

let cache_max_age =
  let doc = "HTTP cache max age (in seconds)" in
  Arg.(value & opt (some int) None & info [ "c"; "cache" ] ~docv:"MAX_AGE" ~doc)

let www = Term.(const main $ db_format $ db_path $ cache_max_age)

let cmd =
  let doc = "Webserver for sherlodoc" in
  let info = Cmd.info "www" ~doc in
  Cmd.v info www

let () = exit (Cmd.eval cmd)
