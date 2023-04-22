module Storage = Db.Storage
module Succ = Query.Succ
module Sort = Query.Sort
module H = Tyxml.Html

let api ~shards params =
  let r = Query.api ~shards params in
  Lwt.return (Marshal.to_string r [])

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
  | Some str -> (
      try max 1 (min default (int_of_string str)) with _ -> default)

let get_params params =
  { Query.query = get_query params
  ; packages = get_packages params
  ; limit = get_limit params
  }

let root fn params =
  let params = get_params params in
  let* result = fn params in
  Dream.respond result

let string_of_tyxml html = Format.asprintf "%a" (Tyxml.Html.pp ()) html
let string_of_tyxml' html = Format.asprintf "%a" (Tyxml.Html.pp_elt ()) html

let cache_header : int option -> Dream.middleware =
 fun max_age f req ->
  let+ response = f req in
  begin
    match max_age with
    | None -> ()
    | Some max_age ->
        Dream.add_header response "Cache-Control"
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

let main port db_filename cache_max_age =
  let shards = Storage_marshal.load db_filename in
  Dream.run ~interface:"127.0.0.1" ~port
  @@ Dream.logger @@ cache_header cache_max_age @@ cors_header
  @@ Dream.router
       [ Dream.get "/" (root (fun params -> api ~shards params)); cors_options ]

open Cmdliner

let path =
  let doc = "Database filename" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"DB" ~doc)

let port =
  let doc = "Port" in
  Arg.(value & opt int 1234 & info [] ~docv:"PORT" ~doc)

let cache_max_age =
  let doc = "HTTP cache max age (in seconds)" in
  Arg.(value & opt (some int) None & info [ "c"; "cache" ] ~docv:"MAX_AGE" ~doc)

let www = Term.(const main $ port $ path $ cache_max_age)

let cmd =
  let doc = "API for sherlodoc" in
  let info = Cmd.info "shelodoc_api" ~doc in
  Cmd.v info www

let () = exit (Cmd.eval cmd)
