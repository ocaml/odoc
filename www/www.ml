module Storage = Db.Storage
module H = Tyxml.Html

let api ~shards params =
  let results = Query.search ~shards params in
  let pretty = Query.pretty params in
  Lwt.return (Ui.render ~pretty results)

let api ~shards params =
  if String.trim params.Query.query = ""
  then Lwt.return (Ui.explain ())
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
    Dream.html (string_of_tyxml @@ Ui.template params.query (Ui.explain ()))

let root fn params =
  try root fn params with
  | _ -> Dream.html (string_of_tyxml @@ Ui.template "" (Ui.explain ()))

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

let static ctype contents = Dream.respond ~headers:[ "Content-Type", ctype ] contents
let style_css _ = static "text/css" [%blob "www/static/style.css"]
let favicon_ico _ = static "image/x-icon" [%blob "www/static/favicon.ico"]
let robots_txt _ = static "text/plain" [%blob "www/static/robots.txt"]
let bg_jpg _ = static "image/jpeg" [%blob "www/static/bg.jpg"]

let main cache_max_age db_format db_filename =
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
       ; Dream.get "/s.css" style_css
       ; Dream.get "/robots.txt" robots_txt
       ; Dream.get "/favicon.ico" favicon_ico
       ; Dream.get "/bg.jpg" bg_jpg
       ; cors_options
       ]

open Cmdliner

let cache_max_age =
  let doc = "HTTP cache max age (in seconds)" in
  Arg.(value & opt (some int) None & info [ "c"; "cache" ] ~docv:"MAX_AGE" ~doc)

let term = Term.(const main $ cache_max_age)
